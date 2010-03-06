/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.impl.plugins;

import java.io.IOException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.PackageElement;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementUtilities;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.modules.javafx.refactoring.RefactoringSupport;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.impl.plugins.elements.ReindexFilesElement;
import org.netbeans.modules.javafx.refactoring.impl.scanners.MoveProblemCollector;
import org.netbeans.modules.javafx.refactoring.repository.ClassModel;
import org.netbeans.modules.javafx.refactoring.repository.ElementDef;
import org.netbeans.modules.javafx.refactoring.repository.ImportEntry;
import org.netbeans.modules.javafx.refactoring.repository.ImportSet;
import org.netbeans.modules.javafx.refactoring.repository.PackageDef;
import org.netbeans.modules.javafx.refactoring.repository.Usage;
import org.netbeans.modules.javafx.refactoring.transformations.InsertTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.RemoveTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.ReplaceTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.Transformation;
import org.netbeans.modules.refactoring.api.MoveRefactoring;
import org.netbeans.modules.refactoring.api.Problem;
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
public class MoveRefactoringPlugin extends ProgressProviderAdapter implements RefactoringPlugin {
    private MoveRefactoring refactoring;

    public MoveRefactoringPlugin(MoveRefactoring refactoring) {
        this.refactoring = refactoring;
    }

    public void cancelRequest() {
        //
    }

    public Problem checkParameters() {
        // for now use MoveProblemCollector; might rewrite it to use ClassModel later on
        final Set<String> movingClasses = new HashSet<String>();
        final Map<String, String> renameMap = new HashMap<String, String>();

        Collection<? extends FileObject> files = refactoring.getRefactoringSource().lookupAll(FileObject.class);
        Set<FileObject> related = new HashSet<FileObject>();

        final ClassIndex ci = RefactoringSupport.classIndex(refactoring);

        fireProgressListenerStart(MoveRefactoring.INIT, files.size());

        final Problem p[] = new Problem[1];

        for (FileObject f: files) {
            ClassModel cm = RefactoringSupport.classModelFactory(refactoring).classModelFor(f);
            for(ElementDef edef : cm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE, ElementKind.ENUM))) {
                related.addAll(ci.getResources(edef.createHandle(), EnumSet.of(ClassIndex.SearchKind.TYPE_REFERENCES), EnumSet.allOf(ClassIndex.SearchScope.class)));
                movingClasses.add(edef.createHandle().getQualifiedName());
                Element e = edef.getElement();
                PackageElement pe = e != null ? ElementUtilities.enclosingPackageElement(e) : null;
                if (pe != null) {
                    renameMap.put(pe.getQualifiedName().toString(), getNewPackageName());
                }
            }
            fireProgressListenerStep();
        }

        Collection<FileObject> allFiles = new ArrayList<FileObject>();
        allFiles.addAll(files);
        allFiles.addAll(related);
        fireProgressListenerStop();
        
        fireProgressListenerStart(MoveRefactoring.PARAMETERS_CHECK, allFiles.size());
        for(FileObject f : allFiles) {
            JavaFXSource jfxs = JavaFXSource.forFileObject(f);
            try {
                jfxs.runUserActionTask(new Task<CompilationController>() {

                    public void run(CompilationController cc) throws Exception {
                        MoveProblemCollector mpc = new MoveProblemCollector<Void, Void>(cc, movingClasses, renameMap);
                        mpc.scan(cc.getCompilationUnit(), null);
                        if (mpc.getProblem() != null) {
                            p[0] = chainProblems(p[0], mpc.getProblem());
                        }
                    }
                }, true);
            } catch (IOException e) {
            }
            fireProgressListenerStep();
        }
        fireProgressListenerStop();

        return p[0];
    }

    public Problem fastCheckParameters() {
        try {
            for (FileObject f: refactoring.getRefactoringSource().lookupAll(FileObject.class)) {
                if (!SourceUtils.isJavaFXFile(f))
                    continue;
                String targetPackageName = getNewPackageName();
                if (!SourceUtils.isValidPackageName(targetPackageName)) {
                    String s = NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_InvalidPackage"); //NOI18N
                    String msg = new MessageFormat(s).format(
                            new Object[] {targetPackageName}
                    );
                    return new Problem(true, msg);
                }
                FileObject targetRoot = SourceUtils.getClassPathRoot(((MoveRefactoring)refactoring).getTarget().lookup(URL.class));
                FileObject targetF = targetRoot.getFileObject(targetPackageName.replace('.', '/'));

                String pkgName = null;
                if ((targetF!=null && !targetF.canWrite())) {
                    return new Problem(true, new MessageFormat(NbBundle.getMessage(MoveRefactoringPlugin.class,"ERR_PackageIsReadOnly")).format( // NOI18N
                            new Object[] {targetPackageName}
                    ));
                }

                pkgName = targetPackageName;

                if (pkgName == null) {
                    pkgName = ""; // NOI18N
                } else if (pkgName.length() > 0) {
                    pkgName = pkgName + '.'; // NOI18N
                }
                String fileName = f.getName();
                if (targetF!=null) {
                    FileObject[] children = targetF.getChildren();
                    for (int x = 0; x < children.length; x++) {
                        if (children[x].getName().equals(fileName) && "java".equals(children[x].getExt()) && !children[x].equals(f) && !children[x].isVirtual()) { //NOI18N
                            return new Problem(true, new MessageFormat(
                                    NbBundle.getMessage(MoveRefactoringPlugin.class,"ERR_ClassToMoveClashes")).format(new Object[] {fileName} // NOI18N
                            ));
                        }
                    } // for
                }
            }
        } catch (IOException ioe) {
            //do nothing
        }
        return null;
    }

    public Problem preCheck() {
        Problem preCheckProblem = null;
        for (FileObject file:refactoring.getRefactoringSource().lookupAll(FileObject.class)) {
            if (!SourceUtils.isElementInOpenProject(file)) {
                preCheckProblem = JavaFXRefactoringPlugin.createProblem(preCheckProblem, true, NbBundle.getMessage(
                        MoveRefactoringPlugin.class,
                        "ERR_ProjectNotOpened", // NOI18N
                        FileUtil.getFileDisplayName(file)));
            }
        }
        return preCheckProblem;
    }

    public Problem prepare(RefactoringElementsBag reb) {
        Collection<? extends FileObject> files = refactoring.getRefactoringSource().lookupAll(FileObject.class);

        fireProgressListenerStart(MoveRefactoring.PREPARE, 14);
        final Set<ElementDef> movingDefs = new HashSet<ElementDef>();
        for(FileObject file : files) {
            ClassModel cm = RefactoringSupport.classModelFactory(refactoring).classModelFor(file);
            movingDefs.addAll(cm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE, ElementKind.ENUM)));
        }
        fireProgressListenerStep();
        final ClassIndex ci = RefactoringSupport.classIndex(refactoring);
        for(FileObject file : files) {
            final ClassModel cm =RefactoringSupport.classModelFactory(refactoring).classModelFor(file);

            final String newPkgName = getNewPackageName();

            Set<FileObject> related = new HashSet<FileObject>();
            for(ElementDef refDef : cm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE, ElementKind.ENUM))) {
                related.addAll(ci.getResources(refDef.createHandle(), EnumSet.of(ClassIndex.SearchKind.TYPE_REFERENCES, ClassIndex.SearchKind.IMPLEMENTORS), EnumSet.allOf(ClassIndex.SearchScope.class)));
            }
            fireProgressListenerStep();

            BaseRefactoringElementImplementation ref = null;
            if (!(newPkgName == null && cm.getPackageDef().getName() == null) && !cm.getPackageDef().getName().equals(newPkgName)) {
                if (newPkgName == null || newPkgName.length() == 0) {
                    ref = new BaseRefactoringElementImplementation(file, reb.getSession()) {
                        @Override
                        protected Set<Transformation> prepareTransformations(FileObject fo) {
                            Transformation t = new RemoveTextTransformation(cm.getPackageDef().getStartPos(), cm.getPackageDef().getEndPos() - cm.getPackageDef().getStartPos());
                            return Collections.singleton(t);
                        }

                        protected String getRefactoringText() {
                            return "Remove Package Definition";
                        }
                    };
                } else {
                    if (cm.getPackageDef() == PackageDef.DEFAULT) {
                        ref = new BaseRefactoringElementImplementation(file, reb.getSession()) {
                            @Override
                            protected Set<Transformation> prepareTransformations(FileObject fo) {
                                Transformation t = new InsertTextTransformation(cm.getPackagePos(), "package " + newPkgName + ";\n"); // NOI18N
                                return Collections.singleton(t);
                            }

                            protected String getRefactoringText() {
                                return "Add Package Definition";
                            }
                        };
                    } else {
                        ref = new BaseRefactoringElementImplementation(file, reb.getSession()) {
                            @Override
                            protected Set<Transformation> prepareTransformations(FileObject fo) {
                                Transformation t = new ReplaceTextTransformation(cm.getPackageDef().getStartFQN(), cm.getPackageDef().getName(), getNewPackageName());
                                return Collections.singleton(t);
                            }

                            protected String getRefactoringText() {
                                return "Rename Package";
                            }
                        };
                    }
                }

            }

            if (ref != null && ref.hasChanges()) {
                reb.add(refactoring, ref);
            }

            fireProgressListenerStep();

            BaseRefactoringElementImplementation fixImports = new BaseRefactoringElementImplementation(file, reb.getSession()) {

                @Override
                protected Set<Transformation> prepareTransformations(FileObject fo) {
                    Set<Transformation> transformations = new HashSet<Transformation>();
                    ImportSet is = cm.getImportSet();

                    is.setPkgName(newPkgName);
                    fixImports(movingDefs, is, cm, true, transformations);
                    return transformations;
                }

                protected String getRefactoringText() {
                    return "Fix Imports";
                }
            };
            if (fixImports.hasChanges()) {
                reb.add(refactoring, fixImports);
            }

            if ((ref != null && ref.hasChanges()) || fixImports.hasChanges()) {
                reb.addFileChange(refactoring, new ReindexFilesElement(file, related));
            }

            fireProgressListenerStep();

            int batchSize = related.size() / 10; // the 10 allocated steps
            int cntr = 0;
            for(FileObject refFo : related) {
                if (refFo.equals(file)) continue;
                BaseRefactoringElementImplementation updateRefs = new BaseRefactoringElementImplementation(refFo, reb.getSession()) {

                    @Override
                    protected Set<Transformation> prepareTransformations(FileObject fo) {
                        Set<Transformation> transformations = new HashSet<Transformation>();
                        ClassModel refCm = RefactoringSupport.classModelFactory(refactoring).classModelFor(fo);
                        for(Usage usg : refCm.getUsages(cm.getPackageDef())) {
                            if (usg.getStartPos() == refCm.getPackageDef().getStartFQN()) continue; // don't process the package name
                            // a small hack
                            ElementDef typeDef = refCm.getDefForPos(usg.getEndPos() + 2); // move 1 character past the "." delimiter
                            if (typeDef != null && movingDefs.contains(typeDef)) {
                                transformations.add(new ReplaceTextTransformation(usg.getStartPos(), cm.getPackageDef().getName(), newPkgName));
                            }
                        }
                        return transformations;
                    }

                    protected String getRefactoringText() {
                        return "Update References";
                    }
                };
                if (updateRefs.hasChanges()) {
                    reb.add(refactoring, updateRefs);
                }
                if (!files.contains(refFo)) {
                    fixImports = new BaseRefactoringElementImplementation(refFo, reb.getSession()) {

                        @Override
                        protected Set<Transformation> prepareTransformations(FileObject fo) {
                            Set<Transformation> transformations = new HashSet<Transformation>();
                            ClassModel refCm = RefactoringSupport.classModelFactory(refactoring).classModelFor(fo);
                            ImportSet is = refCm.getImportSet();
                            for(ElementDef mDef : movingDefs) {
                                String fqn = mDef.createHandle().getQualifiedName();
                                is.addRename(fqn, fqn.replace(cm.getPackageDef().getName(), newPkgName));
                            }
                            fixImports(movingDefs, is, refCm, false, transformations);
                            return transformations;
                        }

                        protected String getRefactoringText() {
                            return "Fix Imports";
                        }
                    };
                    if (fixImports.hasChanges()) {
                        reb.add(refactoring, fixImports);
                    }
                }
                if (++cntr == batchSize) {
                    fireProgressListenerStep();
                    cntr = 0;
                }
            }

            fireProgressListenerStop();
        }

        return null;
    }

    private String getNewPackageName() {
        // XXX cache it !!!
        return SourceUtils.getPackageName(((MoveRefactoring) refactoring).getTarget().lookup(URL.class));
    }

    private void fixImports(Set<ElementDef> movingDefs, ImportSet is, ClassModel cm, boolean isMoving, Set<Transformation> transformations) {
        for(ImportSet.Touple<ElementDef, ImportEntry> missing : is.getMissing()) {
            if (isMoving ^ movingDefs.contains(missing.getT1())) {
                transformations.add(new InsertTextTransformation(cm.getImportPos(), missing.getT2().toString() + ";\n")); // NOI18N
            }
        }
        for(ImportEntry ie : is.getUnused()) {
            transformations.add(new RemoveTextTransformation(ie.getStartPos(), ie.getEndPos() - ie.getStartPos()));
        }
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
