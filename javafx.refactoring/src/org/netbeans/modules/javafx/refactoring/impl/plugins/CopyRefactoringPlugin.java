/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.MemberSelectTree;
import com.sun.tools.javafx.code.JavafxFlags;
import com.sun.tools.mjavac.code.Symbol;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.*;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.JavaFXSourceUtils;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.impl.plugins.elements.FixReferences;
import org.netbeans.modules.javafx.refactoring.impl.plugins.elements.RenameClass;
import org.netbeans.modules.javafx.refactoring.impl.plugins.elements.RenamePackage;
import org.netbeans.modules.javafx.refactoring.transformations.ReplaceTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.Transformation;
import org.netbeans.modules.refactoring.api.*;
import org.netbeans.modules.refactoring.spi.ProgressProviderAdapter;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
import org.netbeans.modules.refactoring.spi.RefactoringPlugin;
import org.netbeans.modules.refactoring.spi.SimpleRefactoringElementImplementation;
import org.openide.ErrorManager;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;
import org.openide.text.PositionBounds;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;

/**
 * Implemented abilities:
 * <ul>
 * <li>Copy file</li>
 * <li>Copy files</li>
  * </ul>
 */
public class CopyRefactoringPlugin extends ProgressProviderAdapter implements RefactoringPlugin  {
    /** Reference to the parent refactoring instance */
    private final AbstractRefactoring refactoring;

    /** Creates a new instance of CopyRefactoringPlugin
     * @param refactoring Parent refactoring instance.
     */
    public CopyRefactoringPlugin(AbstractRefactoring refactoring) {
        this.refactoring = refactoring;
    }

    public void cancelRequest() {
//        cancelRequest = true;
//        if (currentTask!=null) {
//            currentTask.cancel();
//        }
//        RetoucheUtils.cancel = true;
    }

    @Override
    public Problem fastCheckParameters() {
        if (refactoring instanceof SingleCopyRefactoring) {
            SingleCopyRefactoring scRefactoring = (SingleCopyRefactoring)refactoring;
            if (!Utilities.isJavaIdentifier(scRefactoring.getNewName())) {
                String msg = new MessageFormat(NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_InvalidIdentifier")).format( // NOI18N
                    new Object[] {scRefactoring.getNewName()}
                );
                return createProblem(null, true, msg);
            }
            URL target = scRefactoring.getTarget().lookup(URL.class);
            FileObject fo = target != null ? URLMapper.findFileObject(target) : null;
            if (fo == null) {
                return createProblem(null, true, NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_TargetFolderNotSet")); // NOI18N
            }
            if (!SourceUtils.isOnSourceClasspath(fo)) {
                return createProblem(null, true, NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_TargetFolderNotJavaPackage")); // NOI18N
            }
            String targetPackageName = SourceUtils.getPackageName(target);
            if (!SourceUtils.isValidPackageName(targetPackageName)) {
                String msg = new MessageFormat(NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_InvalidPackage")).format( // NOI18N
                    new Object[] {targetPackageName}
                );
                return createProblem(null, true, msg);
            }
            if (fo.getFileObject(scRefactoring.getNewName(), (refactoring.getRefactoringSource().lookup(FileObject.class)).getExt()) != null)
                return createProblem(null, true, new MessageFormat(NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_ClassToMoveClashes")).format(new Object[]{scRefactoring.getNewName()})); // NOI18N
        }
        return null;
    }

    @Override
    public Problem checkParameters() {
        return null;
    }

    @Override
    public Problem preCheck() {
        return null;
    }

    public Problem prepare(RefactoringElementsBag refactoringElements) {
        String newPkgName = getTargetPackageName();
        String oldPkgName = getSourcePackageName();

        final Set<String> movedClasses = new HashSet<String>();
        final Map<String, String> renameMap = new HashMap<String, String>();
        renameMap.put(oldPkgName, newPkgName);

        Problem problem = null;
        Collection<? extends FileObject> affectedFiles = refactoring.getRefactoringSource().lookupAll(FileObject.class);
        for(FileObject fo : affectedFiles) {
            for(ElementHandle<TypeElement> eh : JavaFXSourceUtils.getClasses(fo)) {
                movedClasses.add(eh.getQualifiedName());
            }
        }
        for(FileObject fobj : affectedFiles) {
            problem = checkForProblem(fobj, problem);
            // Refactoring API doesn't handle copy of multiple files
            if (refactoring instanceof MultipleCopyRefactoring) {
                refactoringElements.add(refactoring, new CopyFile(fobj, refactoringElements.getSession()));
            }
            if (!newPkgName.equals(oldPkgName)) {
                RenamePackage rp = new RenamePackage(fobj, renameMap, refactoringElements.getSession()) {

                    @Override
                    protected FileObject getTargetFO() {
                        return CopyRefactoringPlugin.this.getTargetFO(getSourceFO());
                    }

                };
                if (rp.hasChanges()) {
                    refactoringElements.add(refactoring, rp);
                }
            } else if (refactoring instanceof SingleCopyRefactoring) {
                RenameClass rc = new RenameClass(fobj, fobj.getName(), ((SingleCopyRefactoring)refactoring).getNewName(), refactoringElements.getSession()) {

                    @Override
                    protected FileObject getTargetFO() {
                        return CopyRefactoringPlugin.this.getTargetFO(getSourceFO());
                    }
                };
                if (rc.hasChanges()) {
                    refactoringElements.add(refactoring, rc);
                }
            }
            FixReferences fr = new FixReferences(fobj, renameMap, movedClasses, true, refactoringElements.getSession()) {

                @Override
                protected FileObject getTargetFO() {
                    return CopyRefactoringPlugin.this.getTargetFO(getSourceFO());
                }

            };
            if (fr.hasChanges()) {
                refactoringElements.add(refactoring, fr);
            }
        }
        return problem;
    }

    private FileObject getTargetFO(FileObject srcFO) {
        try {
            String targetName = srcFO.getNameExt();
            URL targetURL = null;
            if (refactoring instanceof SingleCopyRefactoring) {
                targetURL = ((SingleCopyRefactoring) refactoring).getTarget().lookup(URL.class);
                targetName = ((SingleCopyRefactoring)refactoring).getNewName() + ".fx";
            } else {
                targetURL = ((MultipleCopyRefactoring) refactoring).getTarget().lookup(URL.class);
            }
            FileObject targetFolder = FileUtil.toFileObject(new File(targetURL.toURI()));
            FileObject targetFile = targetFolder.getFileObject(targetName);
            return targetFile;
        } catch (URISyntaxException uRISyntaxException) {
        }
        return null;
    }

    private String getTargetPackageName() {
        if (refactoring instanceof SingleCopyRefactoring) {
            return SourceUtils.getPackageName(((SingleCopyRefactoring)refactoring).getTarget().lookup(URL.class));
        } else {
            return SourceUtils.getPackageName(((MultipleCopyRefactoring)refactoring).getTarget().lookup(URL.class));
        }
    }

    private String getSourcePackageName() {
        return SourceUtils.getPackageName(refactoring.getRefactoringSource().lookup(FileObject.class).getParent());
    }

    private Problem checkForProblem(FileObject fobj, final Problem p) {
        final Problem[] problem = new Problem[]{p};

        JavaFXSource jfxs = JavaFXSource.forFileObject(fobj);
        try {
            jfxs.runUserActionTask(new Task<CompilationController>() {

                public void run(final CompilationController cc) throws Exception {
                    JavaFXTreePathScanner<Void, Void> scanner = new JavaFXTreePathScanner<Void, Void>() {
                        private String clzName = "";
                        @Override
                        public Void visitIdentifier(IdentifierTree node, Void p) {
                            Element e = cc.getTrees().getElement(getCurrentPath());
                            if (e != null && (((Symbol) e).flags_field & JavafxFlags.PACKAGE_ACCESS) == JavafxFlags.PACKAGE_ACCESS) {
                                TypeElement topClass = JavaFXSourceUtils.getOutermostEnclosingTypeElement(e);
                                String message = "";
                                if (e.getKind() == ElementKind.CLASS || e.getKind() == ElementKind.INTERFACE) {
                                    message = NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_AccessesPackagePrivateClass2", new Object[]{clzName, e.getSimpleName().toString()});
                                } else {
                                    message = NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_AccessesPackagePrivateFeature2", new Object[]{clzName, e.getSimpleName().toString(), topClass.getSimpleName().toString()});
                                }
                                problem[0] = createProblem(problem[0], false, message);
                            }
                            return super.visitIdentifier(node, p);
                        }

                        @Override
                        public Void visitMemberSelect(MemberSelectTree node, Void p) {
                            Element e = cc.getTrees().getElement(getCurrentPath());
                            if (e != null && (((Symbol) e).flags_field & JavafxFlags.PACKAGE_ACCESS) == JavafxFlags.PACKAGE_ACCESS) {
                                TypeElement topClass = JavaFXSourceUtils.getOutermostEnclosingTypeElement(e);
                                String message = "";
                                if (e.getKind() == ElementKind.CLASS || e.getKind() == ElementKind.INTERFACE) {
                                    message = NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_AccessesPackagePrivateClass2", new Object[]{clzName, e.getSimpleName().toString()});
                                } else {
                                    message = NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_AccessesPackagePrivateFeature2", new Object[]{clzName, e.getSimpleName().toString(), topClass.getSimpleName().toString()});
                                }
                                problem[0] = createProblem(problem[0], false, message);
                            }
                            return super.visitMemberSelect(node, p);
                        }

                        @Override
                        public Void visitClassDeclaration(ClassDeclarationTree node, Void p) {
                            String clzNameBak = clzName;
                            try {
                                clzName = node.getSimpleName().toString();
                                return super.visitClassDeclaration(node, p);
                            } finally {
                                clzName = clzNameBak;
                            }
                        }
                    };

                    scanner.scan(cc.getCompilationUnit(), null);
                }
            }, true);
        } catch (IOException iOException) {
            problem[0] = createProblem(problem[0], true, iOException.getLocalizedMessage());
        }
        return problem[0];
    }

//    private class RenameClass extends BaseRefactoringElementImplementation {
//        public RenameClass(FileObject srcFO, RefactoringSession session) {
//            super(srcFO, session);
//        }
//
//        public String getDisplayText() {
//            return new MessageFormat (NbBundle.getMessage(CopyRefactoringPlugin.class, "TXT_RenameClass")).format ( // NOI18N
//                new Object[] {((SingleCopyRefactoring)refactoring).getNewName()}
//            );
//        }
//
//        @Override
//        protected FileObject getTargetFO() {
//            return CopyRefactoringPlugin.this.getTargetFO(getSourceFO());
//        }
//
//        protected Set<Transformation> prepareTransformations(final CompilationController cc) {
//            final Set<Transformation> transformations = new HashSet<Transformation>();
//            JavaFXTreePathScanner scanner = new JavaFXTreePathScanner() {
//                private int[] findClassNamePos(ClassDeclarationTree cdt, CompilationController cc) {
//                    int start = (int)cc.getTrees().getSourcePositions().getStartPosition(cc.getCompilationUnit(), cdt);
//                    int end = start;
//                    TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
//                    ts.move(start);
//
//                    while (ts.moveNext()) {
//                        Token<JFXTokenId> t = ts.token();
//                        if (t.id() != JFXTokenId.IDENTIFIER) {
//                            start += t.length();
//                        } else {
//                            end = start + t.length();
//                            break;
//                        }
//                    }
//                    return new int[]{start, end};
//                }
//
//                @Override
//                public Object visitClassDeclaration(ClassDeclarationTree node, Object p) {
//                    String oldName = node.getSimpleName().toString();
//                    String newName = ((SingleCopyRefactoring)refactoring).getNewName();
//                    if (cc.getFileObject().getName().equals(node.getSimpleName().toString())) {
//                        int[] pos = findClassNamePos(node, cc);
//                        transformations.add(new ReplaceTextTransformation(pos[0], oldName, newName));
//                    }
//                    return super.visitClassDeclaration(node, p);
//                }
//            };
//
//            scanner.scan(cc.getCompilationUnit(), null);
//            return transformations;
//        }
//    }

    /**
     * WTH - the default file copy refactoring does not handle multiple files
     * So we just copy/paste the responsible code here .... Nice and clean, buhaha
     */
    private class CopyFile extends SimpleRefactoringElementImplementation {

        private FileObject fo;
        private RefactoringSession session;
        private DataObject newOne;
        public CopyFile(FileObject fo, RefactoringSession session) {
            this.fo = fo;
            this.session = session;
        }

        /**
         * creates or finds FileObject according to
         * @param url
         * @return FileObject
         */
        private FileObject getOrCreateFolder(URL url) throws IOException {
            try {
                FileObject result = URLMapper.findFileObject(url);
                if (result != null)
                    return result;
                File f = new File(url.toURI());

                result = FileUtil.createFolder(f);
                return result;
            } catch (URISyntaxException ex) {
                throw (IOException) new IOException().initCause(ex);
            }
        }

        public String getText() {
            return NbBundle.getMessage(CopyRefactoringPlugin.class, "TXT_CopyFile", fo.getNameExt());
        }

        public String getDisplayText() {
            return getText();
        }

        public void performChange() {
            try {
                Lookup target = (refactoring instanceof SingleCopyRefactoring) ? ((SingleCopyRefactoring)refactoring).getTarget() : ((MultipleCopyRefactoring)refactoring).getTarget();
                FileObject folder = getOrCreateFolder(target.lookup(URL.class));
                DataObject dob = DataObject.find(fo);
                newOne = dob.copy(DataFolder.findFolder(folder));
                if (refactoring instanceof SingleCopyRefactoring) {
                    newOne.rename(((SingleCopyRefactoring)refactoring).getNewName());
                }
                refactoring.getContext().add(newOne.getPrimaryFile());
            } catch (IOException ex) {
                throw new IllegalStateException(ex);
            }
        }

        @Override
        public void undoChange() {
            try {
                if (newOne != null) {
                    newOne.delete();
                }
            } catch (IOException ex) {
                ErrorManager.getDefault().notify(ex);
            }
        }

        public Lookup getLookup() {
            return Lookup.EMPTY;
        }

        public FileObject getParentFile() {
            return fo;
        }

        public PositionBounds getPosition() {
            return null;
        }
    }

    private static final Problem createProblem(Problem result, boolean isFatal, String message) {
        Problem problem = new Problem(isFatal, message);
        if (result == null) {
            return problem;
        } else if (isFatal) {
            problem.setNext(result);
            return problem;
        } else {
            //problem.setNext(result.getNext());
            //result.setNext(problem);

            // [TODO] performance
            Problem p = result;
            while (p.getNext() != null)
                p = p.getNext();
            p.setNext(problem);
            return result;
        }
    }
}    
