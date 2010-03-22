/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
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
import org.netbeans.modules.javafx.refactoring.impl.plugins.elements.ReindexFileElement;
import org.netbeans.modules.javafx.refactoring.impl.plugins.elements.RenameOccurencesElement;
import org.netbeans.modules.javafx.refactoring.impl.plugins.elements.UpdatePackageDeclarationElement;
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
                String msg = new MessageFormat(NbBundle.getMessage(RenamePackagePlugin.class, "ERR_InvalidPackage")).format( // NOI18N
                        new Object[] {newName}
                );
                return new Problem(true, msg);
            }

            if (f.getParent().getFileObject(newName, f.getExt())!=null) {
                String msg = new MessageFormat(NbBundle.getMessage(RenamePackagePlugin.class,"ERR_PackageExists")).format( // NOI18N
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
                    NbBundle.getMessage(RenamePackagePlugin.class, "ERR_ProjectNotOpened", FileUtil.getFileDisplayName(file)))) // NOI18N
            );
        }
        return preCheckProblem;
    }

    public Problem prepare(RefactoringElementsBag reb) {
        FileObject packageFolder = refactoring.getRefactoringSource().lookup(NonRecursiveFolder.class).getFolder();
        final String targetPkgName = getTargetPackageName(packageFolder);
        final String sourcePkgName = getSourcePackageName(packageFolder);

        Set<FileObject> relevantFiles = new HashSet<FileObject>();

        collectRelevantFiles(packageFolder, relevantFiles);
        
        fireProgressListenerStart(RenameRefactoring.PREPARE, relevantFiles.size());
        Set<BaseRefactoringElementImplementation> refelems = new HashSet<BaseRefactoringElementImplementation>();
        for(FileObject file : relevantFiles) {
            ClassModel cm = RefactoringSupport.classModelFactory(refactoring).classModelFor(file);
            final PackageDef pd = cm.getPackageDef();
            UpdatePackageDeclarationElement ref = new UpdatePackageDeclarationElement(pd.getName(), targetPkgName, file, reb.getSession()) {

                @Override
                protected Set<Transformation> prepareTransformations(FileObject fo) {
                    Set<Transformation> transformations = new HashSet<Transformation>();
                    if (pd.getName().equals(sourcePkgName)) {
                        transformations.add(new ReplaceTextTransformation(pd.getStartFQN(), getOldPkgName(), getNewPkgName()));
                    }
                    return transformations;
                }
            };
            refelems.add(ref);
            
            final ClassIndex index = RefactoringSupport.classIndex(refactoring);
            for(ElementDef cDef : cm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE, ElementKind.ENUM))) {
                for(FileObject referenced : index.getResources(cDef.createHandle(), EnumSet.of(ClassIndex.SearchKind.TYPE_REFERENCES), EnumSet.allOf(ClassIndex.SearchScope.class))) {
                    RenameOccurencesElement bre = new RenameOccurencesElement(sourcePkgName, targetPkgName, referenced, reb.getSession()) {

                        @Override
                        protected Set<Transformation> prepareTransformations(FileObject fo) {
                            Set<Transformation> transformations = new HashSet<Transformation>();
                            ClassModel rcm = RefactoringSupport.classModelFactory(refactoring).classModelFor(fo);
                            for(Usage usg : rcm.getUsages(pd)) {
                                transformations.add(new ReplaceTextTransformation(usg.getStartPos(), getOldName(), getNewName()));
                            }
                            return transformations;
                        }
                    };
                    refelems.add(bre);
                }
            }
            fireProgressListenerStep();
        }
        for(BaseRefactoringElementImplementation ref : refelems) {
            if (ref.hasChanges()) {
                reb.add(refactoring, ref);
            } else if (SourceUtils.isJavaFXFile(ref.getParentFile())) {
                reb.add(refactoring, new ReindexFileElement(ref.getParentFile()));
            }
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
        String result = ""; // NOI18N
        if (s1 != null && s1.length() > 0) {
            result += s1 + "."; // NOI18N
        }
        result +=s2;
        if (s3 != null && s3.length() > 0) {
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
