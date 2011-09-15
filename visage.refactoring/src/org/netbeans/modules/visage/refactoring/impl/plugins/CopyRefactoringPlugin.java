/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * Portions Copyrighted 1997-2008 Sun Microsystems, Inc.
 */

package org.netbeans.modules.visage.refactoring.impl.plugins;

import org.netbeans.modules.visage.refactoring.impl.plugins.elements.BaseRefactoringElementImplementation;
import com.sun.visage.api.tree.ClassDeclarationTree;
import com.sun.visage.api.tree.IdentifierTree;
import com.sun.visage.api.tree.VisageTreePathScanner;
import com.sun.visage.api.tree.MemberSelectTree;
import com.sun.tools.visage.code.JavafxFlags;
import com.sun.tools.mjavac.code.Symbol;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.visage.source.CompilationController;
import org.netbeans.api.visage.source.VisageSource;
import org.netbeans.api.visage.source.VisageSourceUtils;
import org.netbeans.api.visage.source.Task;
import org.netbeans.modules.visage.refactoring.RefactoringSupport;
import org.netbeans.modules.visage.refactoring.impl.visagec.SourceUtils;
import org.netbeans.modules.visage.refactoring.impl.plugins.elements.FixImportsElement;
import org.netbeans.modules.visage.refactoring.impl.plugins.elements.UpdatePackageDeclarationElement;
import org.netbeans.modules.visage.refactoring.repository.ClassModel;
import org.netbeans.modules.visage.refactoring.repository.ElementDef;
import org.netbeans.modules.visage.refactoring.repository.ImportEntry;
import org.netbeans.modules.visage.refactoring.repository.ImportSet;
import org.netbeans.modules.visage.refactoring.transformations.InsertTextTransformation;
import org.netbeans.modules.visage.refactoring.transformations.RemoveTextTransformation;
import org.netbeans.modules.visage.refactoring.transformations.ReplaceTextTransformation;
import org.netbeans.modules.visage.refactoring.transformations.Transformation;
import org.netbeans.modules.refactoring.api.AbstractRefactoring;
import org.netbeans.modules.refactoring.api.MultipleCopyRefactoring;
import org.netbeans.modules.refactoring.api.Problem;
import org.netbeans.modules.refactoring.api.RefactoringSession;
import org.netbeans.modules.refactoring.api.SingleCopyRefactoring;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
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

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class CopyRefactoringPlugin extends VisageRefactoringPlugin {
    private AbstractRefactoring refactoring;

    public CopyRefactoringPlugin(AbstractRefactoring refactoring) {
        this.refactoring = refactoring;
    }

    public Problem checkParameters() {
        Problem problem = null;
        Collection<? extends FileObject> affectedFiles = refactoring.getRefactoringSource().lookupAll(FileObject.class);
        for(FileObject fo : affectedFiles) {
            problem = checkForProblem(fo, problem);
        }

        return problem;
    }

    public Problem fastCheckParameters() {
        Problem p = null;
        for(FileObject fo : refactoring.getRefactoringSource().lookupAll(FileObject.class)) {
            FileObject target = getTargetFO(fo);
            if (target != null) {
                p = chainProblems(p, new Problem(true, NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_FileAlreadyExists", target.getName(), getTargetPackageName()))); // NOI18N
            }
        }
        return p;
    }

    public Problem preCheck() {
        return null;
    }

    public Problem prepare(RefactoringElementsBag reb) {
        final String newPkgName = getTargetPackageName();
        
        Collection<? extends FileObject> affectedFiles = refactoring.getRefactoringSource().lookupAll(FileObject.class);
        
        final Set<ElementDef> movingDefs = new HashSet<ElementDef>();
        for(FileObject fobj : affectedFiles) {
            if (isCancelled()) return null;
            ClassModel cm = RefactoringSupport.classModelFactory(refactoring).classModelFor(fobj);
            movingDefs.addAll(cm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE, ElementKind.ENUM)));
        }

        for(FileObject fobj : affectedFiles) {
            if (isCancelled()) return null;
            if (!SourceUtils.isVisageFile(fobj)) continue;
            final ClassModel cm = RefactoringSupport.classModelFactory(refactoring).classModelFor(fobj);
            // Refactoring API doesn't handle copy of multiple files
            if (refactoring instanceof MultipleCopyRefactoring) {
                reb.add(refactoring, new CopyFile(fobj, reb.getSession()));
            }
            BaseRefactoringElementImplementation ref = null;
            if (!cm.getPackageDef().getName().equals(newPkgName)) {
                ref = new UpdatePackageDeclarationElement(cm.getPackageDef().getName(), newPkgName, fobj, reb.getSession()) {
                        @Override
                        protected Set<Transformation> prepareTransformations(FileObject fo) {
                            Transformation t;
                            if (isOldDefault()) {
                                t = new RemoveTextTransformation(cm.getPackageDef().getStartPos(), cm.getPackageDef().getEndPos() - cm.getPackageDef().getStartPos());
                            } else if (isNewDefault()) {
                                t = new InsertTextTransformation(cm.getPackagePos(), "package " + newPkgName + ";\n"); // NOI18N
                            } else {
                                t = new ReplaceTextTransformation(cm.getPackageDef().getStartFQN(), getOldPkgName(), getNewPkgName());
                            }
                            return Collections.singleton(t);
                        }

                        @Override
                        protected FileObject getTargetFO() {
                            return CopyRefactoringPlugin.this.getTargetFO(getParentFile());
                        }
                    };
            }
            if (ref != null && ref.hasChanges()) {
                reb.add(refactoring, ref);
            }
            
            if (!getSourcePackageName().equals(getTargetPackageName())) {
                FixImportsElement fixImports = new FixImportsElement(fobj, reb.getSession(), false) {

                    @Override
                    protected Set<Transformation> prepareTransformations(FileObject fo) {
                        Set<Transformation> transformations = new HashSet<Transformation>();
                        ClassModel refCm = RefactoringSupport.classModelFactory(refactoring).classModelFor(fo);
                        ImportSet is = refCm.getImportSet();
                        for(ElementDef mDef : movingDefs) {
                            String fqn = mDef.createHandle().getQualifiedName();
                            is.addRename(fqn, fqn.replace(cm.getPackageDef().getName(), newPkgName));
                        }
                        is.setPkgName(newPkgName);
                        fixImports(movingDefs, is, refCm, true, transformations);
                        return transformations;
                    }

                    @Override
                    protected FileObject getTargetFO() {
                        return CopyRefactoringPlugin.this.getTargetFO(getParentFile());
                    }
                };
                if (fixImports.hasChanges()) {
                    reb.add(refactoring, fixImports);
                }
            }

            if (refactoring instanceof SingleCopyRefactoring) {
                final String newClsName = ((SingleCopyRefactoring)refactoring).getNewName();
                BaseRefactoringElementImplementation renameClass = new BaseRefactoringElementImplementation(fobj, reb.getSession(), false) {

                    @Override
                    protected Set<Transformation> prepareTransformations(FileObject fo) {
                        Set<Transformation> t = new HashSet<Transformation>();
                        for(ElementDef edef : cm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE, ElementKind.ENUM))) {
                            if (!edef.getName().contentEquals(newClsName) && !edef.isSynthetic() && edef.getName().equals(fo.getName())) {
                                t.add(new ReplaceTextTransformation(edef.getStartFQN(), edef.getName(), newClsName));
                                break;
                            }
                        }
                        return t;
                    }

                    protected String getRefactoringText() {
                        return NbBundle.getMessage(CopyRefactoringPlugin.class, "LBL_SetClassName", ((SingleCopyRefactoring)refactoring).getNewName()); // NOI18N
                    }

                    @Override
                    protected FileObject getTargetFO() {
                        return CopyRefactoringPlugin.this.getTargetFO(getParentFile());
                    }
                };
                if (renameClass.hasChanges()) {
                    reb.add(refactoring, renameClass);
                }
            }
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

    private FileObject getTargetFO(FileObject srcFO) {
        try {
            String targetName = srcFO.getNameExt();
            URL targetURL = null;
            if (refactoring instanceof SingleCopyRefactoring) {
                targetURL = ((SingleCopyRefactoring) refactoring).getTarget().lookup(URL.class);
                targetName = ((SingleCopyRefactoring)refactoring).getNewName() + ".fx"; // NOI18N
            } else {
                targetURL = ((MultipleCopyRefactoring) refactoring).getTarget().lookup(URL.class);
            }
            FileObject targetFolder = FileUtil.toFileObject(new File(targetURL.toURI()));
            FileObject targetFile = targetFolder != null ? targetFolder.getFileObject(targetName) : null;
            return targetFile;
        } catch (URISyntaxException uRISyntaxException) {
        }
        return null;
    }

    private Problem checkForProblem(FileObject fobj, final Problem p) {
        // don't check if the package is not changing
        if (getSourcePackageName().equals(getTargetPackageName())) return null;
        
        // use the scanner to check for problems; might be rewritten to ClassModel usage later
        // can't use MoveProblemCollector as the preconditions are a bit different :(
        final Problem[] problem = new Problem[]{p};

        VisageSource jfxs = VisageSource.forFileObject(fobj);
        try {
            jfxs.runUserActionTask(new Task<CompilationController>() {

                public void run(final CompilationController cc) throws Exception {
                    VisageTreePathScanner<Void, Void> scanner = new VisageTreePathScanner<Void, Void>() {
                        private String clzName = ""; // NOI18N
                        @Override
                        public Void visitIdentifier(IdentifierTree node, Void p) {
                            Element e = cc.getTrees().getElement(getCurrentPath());
                            if (e != null && (((Symbol) e).flags_field & JavafxFlags.PACKAGE_ACCESS) == JavafxFlags.PACKAGE_ACCESS) {
                                TypeElement topClass = VisageSourceUtils.getOutermostEnclosingTypeElement(e);
                                String message = ""; // NOI18N
                                if (e.getKind().isInterface() || e.getKind().isClass()) {
                                    message = NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_AccessesPackagePrivateClass2", new Object[]{clzName, e.getSimpleName().toString()}); // NOI18N
                                } else {
                                    message = NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_AccessesPackagePrivateFeature2", new Object[]{clzName, e.getSimpleName().toString(), topClass.getSimpleName().toString()}); // NOI18N
                                }
                                problem[0] = createProblem(problem[0], false, message);
                            }
                            return super.visitIdentifier(node, p);
                        }

                        @Override
                        public Void visitMemberSelect(MemberSelectTree node, Void p) {
                            Element e = cc.getTrees().getElement(getCurrentPath());
                            if (e != null && (((Symbol) e).flags_field & JavafxFlags.PACKAGE_ACCESS) == JavafxFlags.PACKAGE_ACCESS) {
                                TypeElement topClass = VisageSourceUtils.getOutermostEnclosingTypeElement(e);
                                String message = ""; // NOI18N
                                if (e.getKind().isInterface() || e.getKind().isClass()) {
                                    message = NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_AccessesPackagePrivateClass2", new Object[]{clzName, e.getSimpleName().toString()}); // NOI18n
                                } else {
                                    message = NbBundle.getMessage(CopyRefactoringPlugin.class, "ERR_AccessesPackagePrivateFeature2", new Object[]{clzName, e.getSimpleName().toString(), topClass.getSimpleName().toString()}); // NOI18N
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
}
