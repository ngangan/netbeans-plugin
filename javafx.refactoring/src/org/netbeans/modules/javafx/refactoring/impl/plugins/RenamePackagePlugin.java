/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.impl.plugins;

import java.text.MessageFormat;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;
import javax.lang.model.element.ElementKind;
import org.netbeans.api.fileinfo.NonRecursiveFolder;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.modules.javafx.refactoring.RefactoringSupport;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.repository.ClassModel;
import org.netbeans.modules.javafx.refactoring.repository.ElementDef;
import org.netbeans.modules.javafx.refactoring.repository.PackageDef;
import org.netbeans.modules.javafx.refactoring.repository.Usage;
import org.netbeans.modules.javafx.refactoring.transformations.ReplaceTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.Transformation;
import org.netbeans.modules.refactoring.api.AbstractRefactoring;
import org.netbeans.modules.refactoring.api.Problem;
import org.netbeans.modules.refactoring.api.RenameRefactoring;
import org.netbeans.modules.refactoring.spi.ProgressProviderAdapter;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
import org.netbeans.modules.refactoring.spi.RefactoringPlugin;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.NbBundle;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class RenamePackagePlugin extends ProgressProviderAdapter implements RefactoringPlugin {
    private AbstractRefactoring refactoring;

    public RenamePackagePlugin(AbstractRefactoring refactoring) {
        this.refactoring = refactoring;
    }

    public void cancelRequest() {
        //
    }

    public Problem checkParameters() {
        return null;
    }

    public Problem fastCheckParameters() {
        FileObject f = refactoring.getRefactoringSource().lookup(NonRecursiveFolder.class).getFolder();
        if (f!=null) {
            String newName = ((RenameRefactoring) refactoring).getNewName();
            if (!SourceUtils.isValidPackageName(newName)) {
                String msg = new MessageFormat(NbBundle.getMessage(RenamePackagePlugin.class, "ERR_InvalidPackage")).format(
                        new Object[] {newName}
                );
                return new Problem(true, msg);
            }

            if (f.getParent().getFileObject(newName, f.getExt())!=null) {
                String msg = new MessageFormat(NbBundle.getMessage(RenamePackagePlugin.class,"ERR_PackageExists")).format(
                        new Object[] {newName}
                );
                return new Problem(true, msg);
            }
        }
        return null;
    }

    public Problem preCheck() {
        Problem preCheckProblem = null;
        FileObject file = refactoring.getRefactoringSource().lookup(NonRecursiveFolder.class).getFolder();
        if (!SourceUtils.isFileInOpenProject(file)) {
            preCheckProblem = chainProblems(preCheckProblem, new Problem(true, NbBundle.getMessage(
                    RenamePackagePlugin.class,
                    "ERR_ProjectNotOpened",
                FileUtil.getFileDisplayName(file))));
        }
        return preCheckProblem;
    }

    public Problem prepare(RefactoringElementsBag reb) {
        fireProgressListenerStart(RenameRefactoring.INIT, 1);
        FileObject packageFolder = refactoring.getRefactoringSource().lookup(NonRecursiveFolder.class).getFolder();
        final String targetPkgName = getTargetPackageName(packageFolder);
        final String sourcePkgName = getSourcePackageName(packageFolder);

        Set<FileObject> relevantFiles = new HashSet<FileObject>();

        collectRelevantFiles(packageFolder, relevantFiles);
        fireProgressListenerStep();
        fireProgressListenerStop();
        
        fireProgressListenerStart(RenameRefactoring.PREPARE, relevantFiles.size() * 3);
        for(FileObject file : relevantFiles) {
            ClassModel cm = RefactoringSupport.classModelFactory(refactoring).classModelFor(file);
            final PackageDef pd = cm.getPackageDef();
            BaseRefactoringElementImplementation ref = new BaseRefactoringElementImplementation(file, reb.getSession()) {

                @Override
                protected Set<Transformation> prepareTransformations(FileObject fo) {
                    Set<Transformation> transformations = new HashSet<Transformation>();
                    if (pd.getName().equals(sourcePkgName)) {
                        transformations.add(new ReplaceTextTransformation(pd.getStartFQN(), pd.getName(), targetPkgName));
                    }
                    return transformations;
                }

                protected String getRefactoringText() {
                    return "Rename Package";
                }
            };
            if (ref.hasChanges()) {
                reb.add(refactoring, ref);
            }
            fireProgressListenerStep();

            final ClassIndex index = RefactoringSupport.classIndex(refactoring);
            fireProgressListenerStep();
            for(ElementDef cDef : cm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE))) {
                for(FileObject referenced : index.getResources(cDef.createHandle(), EnumSet.of(ClassIndex.SearchKind.TYPE_REFERENCES), EnumSet.allOf(ClassIndex.SearchScope.class))) {
                    BaseRefactoringElementImplementation bre = new BaseRefactoringElementImplementation(referenced, reb.getSession()) {

                        @Override
                        protected Set<Transformation> prepareTransformations(FileObject fo) {
                            Set<Transformation> transformations = new HashSet<Transformation>();
                            ClassModel rcm = RefactoringSupport.classModelFactory(refactoring).classModelFor(fo);
                            for(Usage usg : rcm.getUsages(pd)) {
                                transformations.add(new ReplaceTextTransformation(usg.getStartPos(), sourcePkgName, targetPkgName));
                            }
                            return transformations;
                        }

                        protected String getRefactoringText() {
                            return "Rename Occurences";
                        }
                    };
                    if (bre.hasChanges()) {
                        reb.add(refactoring, bre);
                    }
                }
            }
            fireProgressListenerStep();
        }
        fireProgressListenerStop();
        return null;
    }

    private void collectRelevantFiles(FileObject parent, Set<FileObject> relevantFiles) {
        for(FileObject fo : parent.getChildren()) {
            if (fo.isData() && SourceUtils.isJavaFXFile(fo)) {
                relevantFiles.add(fo);
            } else if (fo.isFolder()) {
                collectRelevantFiles(fo, relevantFiles);
            }
        }
    }

    String getTargetPackageName(FileObject fo) {
        if (refactoring.getRefactoringSource().lookup(NonRecursiveFolder.class) !=null) {
            //package rename
            return getNewPackageName();
        } else {

            //folder rename
            FileObject folder = refactoring.getRefactoringSource().lookup(FileObject.class);
            ClassPath cp = ClassPath.getClassPath(folder, ClassPath.SOURCE);
            FileObject root = cp.findOwnerRoot(folder);
            String prefix = FileUtil.getRelativePath(root, folder.getParent()).replace('/','.');
            String postfix = FileUtil.getRelativePath(folder, fo.isFolder() ? fo : fo.getParent()).replace('/', '.');
            String t = concat(prefix, getNewPackageName(), postfix);
            return t;
        }
    }

    String getSourcePackageName(FileObject fo) {
        ClassPath cp = ClassPath.getClassPath(fo, ClassPath.SOURCE);
        return cp.getResourceName(fo, '.', false);
    }

    String getNewPackageName() {
        return ((RenameRefactoring) refactoring).getNewName();
    }

    private String concat(String s1, String s2, String s3) {
        String result = "";
        if (s1 != null && !"".equals(s1)) {
            result += s1 + "."; // NOI18N
        }
        result +=s2;
        if (s3 != null && !"".equals(s3)) {
            result += ("".equals(result)? "" : ".") + s3; // NOI18N
        }
        return result;
    }

    private static Problem chainProblems(Problem p,Problem p1) {
        Problem problem;

        if (p==null) return p1;
        if (p1==null) return p;
        problem=p;
        while(problem.getNext()!=null) {
            problem=problem.getNext();
        }
        problem.setNext(p1);
        return p;
    }
}
