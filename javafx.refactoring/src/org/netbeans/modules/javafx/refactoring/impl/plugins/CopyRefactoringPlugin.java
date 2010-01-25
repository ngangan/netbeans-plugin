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
import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.MemberSelectTree;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.UnitTree;
import com.sun.tools.javafx.api.JavafxcTrees;
import com.sun.tools.javafx.code.JavafxFlags;
import com.sun.tools.javafx.tree.JFXIdent;
import com.sun.tools.javafx.tree.JFXTree;
import com.sun.tools.mjavac.code.Symbol;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.*;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.JavaFXSourceUtils;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.editor.BaseDocument;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.transformations.InsertTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.RemoveTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.ReplaceTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.Transformation;
import org.netbeans.modules.javafx.refactoring.transformations.Transformer;
import org.netbeans.modules.refactoring.api.*;
import org.netbeans.modules.refactoring.spi.ProgressProviderAdapter;
import org.netbeans.modules.refactoring.spi.RefactoringElementImplementation;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
import org.netbeans.modules.refactoring.spi.RefactoringPlugin;
import org.netbeans.modules.refactoring.spi.SimpleRefactoringElementImplementation;
import org.openide.ErrorManager;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;
import org.openide.text.DataEditorSupport;
import org.openide.text.PositionBounds;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;
import org.openide.util.lookup.Lookups;

/**
 * Implemented abilities:
 * <ul>
 * <li>Copy file</li>
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

        Problem problem = null;
        for(FileObject fobj : refactoring.getRefactoringSource().lookupAll(FileObject.class)) {
            problem = checkForProblem(fobj, problem);
            // Refactoring API doesn't handle copy of multiple files
            if (refactoring instanceof MultipleCopyRefactoring) {
                refactoringElements.add(refactoring, new CopyFile(fobj, refactoringElements.getSession()));
            }
            if (!newPkgName.equals(oldPkgName)) {
                refactoringElements.add(refactoring, new UpdatePackage(fobj));
            }
            refactoringElements.add(refactoring, new FixReferences(fobj));
        }
        return problem;
    }

    private FileObject getTargetFO(FileObject srcFO) {
        try {
            String srcName = srcFO.getNameExt();
            URL targetURL = null;
            if (refactoring instanceof SingleCopyRefactoring) {
                targetURL = ((SingleCopyRefactoring) refactoring).getTarget().lookup(URL.class);
            } else {
                targetURL = ((MultipleCopyRefactoring) refactoring).getTarget().lookup(URL.class);
            }
            FileObject targetFolder = FileUtil.toFileObject(new File(targetURL.toURI()));
            FileObject targetFile = targetFolder.getFileObject(srcName);
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

    private class UpdatePackage extends SimpleRefactoringElementImplementation implements RefactoringElementImplementation {
        private FileObject srcFO;
        public UpdatePackage(FileObject srcFO) {
            this.srcFO = srcFO;
        }

        public String getDisplayText() {
            return new MessageFormat (NbBundle.getMessage(CopyRefactoringPlugin.class, "TXT_UpdatePackage")).format ( // NOI18N
                new Object[] {(refactoring instanceof SingleCopyRefactoring) ? ((SingleCopyRefactoring)refactoring).getNewName() : getParentFile().getName(), getTargetPackageName()}
            );
        }

        public Lookup getLookup() {
            return Lookups.fixed(srcFO);
        }

        public FileObject getParentFile() {
            return srcFO;
        }

        public PositionBounds getPosition() {
            return null;
        }

        public String getText() {
            return getDisplayText();
        }

        public void performChange() {
            FileObject targetFO = getTargetFO(srcFO);
            if (targetFO != null) {
                Transformer t = Transformer.forFileObject(targetFO);
                if (t != null) {
                    t.addTransformations(getTransformations());
                    t.transform();
                }
            }
        }

        @Override
        protected String getNewFileContent() {
            Transformer t = Transformer.forFileObject(srcFO).newClone();
            t.addTransformations(getTransformations());
            return t.preview();
        }

        final private Object transformationsLock = new Object();
        // @GuarddBy transformationsLock
        private Set<Transformation> transformations = null;

        private Set<Transformation> getTransformations() {
            synchronized(transformationsLock) {
                if (transformations == null) {
                    transformations = new HashSet<Transformation>();
                    try {
                        JavaFXSource jfxs = JavaFXSource.forFileObject(srcFO);

                        jfxs.runUserActionTask(new Task<CompilationController>() {

                            public void run(final CompilationController cc) throws Exception {
                                JavaFXTreePathScanner scanner = new JavaFXTreePathScanner() {

                                    @Override
                                    public Object visitCompilationUnit(UnitTree node, Object p) {
                                        Tree pkgNameTree = node.getPackageName();
                                        if (pkgNameTree == null) { // default package; no package statement
                                            TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
                                            ts.moveStart();
                                            ts.moveNext();
                                            int pos = 0;
                                            Token<JFXTokenId> token = ts.token();
                                            while (token.id() == JFXTokenId.COMMENT && ts.moveNext()) {
                                                pos += token.length();
                                                token = ts.token();
                                            }
                                            String separator = (token.text().toString().equals("\n")) ? "" : "\n";

                                            transformations.add(new InsertTextTransformation(pos, "\npackage " + getTargetPackageName() + ";" + separator)); // NOI18N
                                        } else {
                                            long startPos = cc.getTrees().getSourcePositions().getStartPosition(node, pkgNameTree);
                                            long endPos = cc.getTrees().getSourcePositions().getEndPosition(node, pkgNameTree);
                                            String tgtPkgName = getTargetPackageName();
                                            if (tgtPkgName != null && tgtPkgName.length() > 0) {
                                                transformations.add(new ReplaceTextTransformation((int)startPos, pkgNameTree.toString(), getTargetPackageName()));
                                            } else {
                                                TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
                                                ts.move((int)startPos);
                                                ts.movePrevious();
                                                int decrement = 0;
                                                Token<JFXTokenId> token = ts.token();
                                                while (token.id() != JFXTokenId.PACKAGE && ts.movePrevious()) {
                                                    decrement += token.length();
                                                    token = ts.token();
                                                }
                                                if (token != null && token.id() == JFXTokenId.PACKAGE) {
                                                    decrement += token.length();
                                                    startPos = startPos - decrement;
                                                    ts.move((int)endPos);
                                                    ts.moveNext();
                                                    int increment = 0;
                                                    token = ts.token();
                                                    while (token.id() != JFXTokenId.SEMI && ts.moveNext()) {
                                                        increment += token.length();
                                                        token = ts.token();
                                                    }
                                                    if (token != null && token.id() == JFXTokenId.SEMI) {
                                                        increment += token.length();
                                                        endPos = endPos + increment;
                                                        transformations.add(new RemoveTextTransformation((int)startPos, (int)(endPos - startPos + 1)));
                                                    }
                                                }
                                            }
                                        }

                                        return super.visitCompilationUnit(node, p);
                                    }
                                };

                                scanner.scan(cc.getCompilationUnit(), null);
                            }
                        }, true);
                    } catch (Exception ioe) {
                        ErrorManager.getDefault().notify(ioe);
                    }
                }
            }
            return transformations;
        }
    }

    private class FixReferences extends SimpleRefactoringElementImplementation implements RefactoringElementImplementation {
        private FileObject srcFO;

        public FixReferences(FileObject srcFO) {
            this.srcFO = srcFO;
        }

        @Override
        public void undoChange() {
            super.undoChange();
        }

        public String getDisplayText() {
            return new MessageFormat (NbBundle.getMessage(CopyRefactoringPlugin.class, "TXT_FixReferences")).format ( // NOI18N
                new Object[] {(refactoring instanceof SingleCopyRefactoring) ? ((SingleCopyRefactoring)refactoring).getNewName() : getParentFile().getName()}
            );
        }

        public Lookup getLookup() {
            return Lookups.singleton(srcFO);
        }

        public FileObject getParentFile() {
            return srcFO;
        }

        public PositionBounds getPosition() {
            return null;
        }

        public String getText() {
            return getDisplayText();
        }

        public void performChange() {
            FileObject targetFO = getTargetFO(srcFO);
            if (targetFO != null) {
                Transformer t = Transformer.forFileObject(targetFO);
                if (t != null) {
                    t.addTransformations(getTransformations());
                    t.transform();
                }
            }
        }

        @Override
        protected String getNewFileContent() {
            Transformer t = Transformer.forFileObject(srcFO).newClone();
            t.addTransformations(getTransformations());

            return t.preview();
        }

        final private Object transformationsLock = new Object();
        // @GuarddBy transformationsLock
        private Set<Transformation> transformations = null;

        private Set<Transformation> getTransformations() {
            synchronized(transformationsLock) {
                if (transformations == null) {
                    transformations = new HashSet<Transformation>();
                    try {
                        final Set<String> toImport = new HashSet<String>();
                        final long[] lastImportEndPos = new long[]{Long.MIN_VALUE};

                        JavaFXSource jfxs = JavaFXSource.forFileObject(srcFO);

                        jfxs.runUserActionTask(new Task<CompilationController>() {

                            public void run(final CompilationController cc) throws Exception {
                                JavaFXTreePathScanner scanner = new JavaFXTreePathScanner() {
                                    final private Set<String> imported = new HashSet<String>();

                                    private boolean inImport = false;
                                    private boolean inSelect = false;
                                    private boolean wildcard = false;

                                    @Override
                                    public Object visitCompilationUnit(UnitTree node, Object p) {
                                        try {
                                            return super.visitCompilationUnit(node, p);
                                        } finally {
                                            TokenSequence ts = cc.getTokenHierarchy().tokenSequence();
                                            ts.move((int)lastImportEndPos[0]);
                                            ts.moveNext();
                                            Token<JFXTokenId> token = ts.token();
                                            int increment = 0;
                                            while (token.id() != JFXTokenId.SEMI && ts.moveNext()) {
                                                increment += token.length();
                                                token = ts.token();
                                            }
                                            if (token != null && token.id() == JFXTokenId.SEMI) {
                                                lastImportEndPos[0] += increment;
                                            }
                                        }
                                    }

                                    @Override
                                    public Object visitIdentifier(IdentifierTree node, Object p) {
                                        if (!inSelect) {
                                            Element e = cc.getTrees().getElement(getCurrentPath());
                                            if (isTypeElement(e)) {
                                                PackageElement pe = getTypePackage((TypeElement)e);
                                                if (pe.getQualifiedName().contentEquals(getSourcePackageName())) {
                                                    if (inImport) {
                                                        long pos = cc.getTrees().getSourcePositions().getStartPosition(cc.getCompilationUnit(), node);
                                                        transformations.add(new InsertTextTransformation((int)pos, getSourcePackageName() + ".")); // NOI18N
                                                    } else {
                                                        String usedId = node.toString();
                                                        if (!usedId.startsWith(getSourcePackageName())) {
                                                            String newImport = getSourcePackageName() + "." + usedId; // NOI18N
                                                            if (!imported.contains(newImport)) {
                                                                String wildCard = getSourcePackageName() + ".*"; // NOI18N
                                                                if (!imported.contains(wildCard)) {
                                                                    toImport.add(newImport);
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        } else {
                                            wildcard = false;
                                        }
                                        return super.visitIdentifier(node, p);
                                    }

                                    @Override
                                    public Object visitImport(ImportTree node, Object p) {
                                        inImport = true;
                                        try {
                                            long endPos = cc.getTrees().getSourcePositions().getEndPosition(cc.getCompilationUnit(), node);
                                            if (endPos > lastImportEndPos[0]) {
                                                lastImportEndPos[0] = endPos;
                                            }
                                            Element e = cc.getTrees().getElement(JavafxcTrees.getPath(getCurrentPath(), node.getQualifiedIdentifier()));
                                            if (e != null) {
                                                if (isTypeElement(e)) {
                                                    imported.add(((TypeElement)e).getQualifiedName().toString());
                                                } else {
                                                    imported.add(node.getQualifiedIdentifier().toString());
                                                }
                                            }
                                            return super.visitImport(node, p);
                                        } finally {
                                            inImport = false;
                                        }
                                    }

                                    @Override
                                    public Object visitMemberSelect(MemberSelectTree node, Object p) {
                                        try {
                                            if (inImport) {
                                                if (node.getIdentifier().contentEquals("*")) {
                                                    wildcard = true;
                                                } else {
                                                    Element e = cc.getTrees().getElement(getCurrentPath());
                                                    if (e != null && e.getKind() == ElementKind.PACKAGE) {
                                                        PackageElement pe = (PackageElement)e;
                                                        if (pe.getQualifiedName().contentEquals(getTargetPackageName())) {
                                                            long startPos = cc.getTrees().getSourcePositions().getStartPosition(cc.getCompilationUnit(), node);
                                                            long endPos = cc.getTrees().getSourcePositions().getEndPosition(cc.getCompilationUnit(), node);
                                                            if (wildcard) {
                                                                TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
                                                                ts.move((int)startPos);
                                                                ts.movePrevious();
                                                                Token<JFXTokenId> token = ts.token();
                                                                int decrement = 0;
                                                                while (token.id() != JFXTokenId.IMPORT && ts.movePrevious()) {
                                                                    decrement += token.length();
                                                                    token = ts.token();
                                                                }
                                                                if (token != null && token.id() == JFXTokenId.IMPORT) {
                                                                    decrement += token.length();
                                                                    startPos -= decrement;
                                                                    ts.move((int)endPos);
                                                                    ts.moveNext();
                                                                    int increment = 0;
                                                                    token = ts.token();
                                                                    while (token.id() != JFXTokenId.SEMI && ts.moveNext()) {
                                                                        increment += token.length();
                                                                        token = ts.token();
                                                                    }
                                                                    if (token != null && token.id() == JFXTokenId.SEMI) {
                                                                        increment += token.length();
                                                                        endPos = endPos + increment;
                                                                        transformations.add(new RemoveTextTransformation((int)startPos, (int)(endPos - startPos + 1)));
                                                                    }
                                                                }
                                                            } else {
                                                                transformations.add(new RemoveTextTransformation((int)startPos, node.toString().length() + 2));
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            inSelect = true;
                                            return super.visitMemberSelect(node, p);
                                        } finally {
                                            inSelect = false;
                                        }
                                    }
                                };

                                scanner.scan(cc.getCompilationUnit(), null);
                                for(String imprt : toImport) {
                                    transformations.add(new InsertTextTransformation((int)lastImportEndPos[0] + 1, "\nimport " + imprt + ";")); // NOI18N
                                }
                            }
                        }, true);
                    } catch (Exception ioe) {
                        ErrorManager.getDefault().notify(ioe);
                    }
                }
            }
            return transformations;
        }
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

    private static boolean isTypeElement(Element e) {
        return e != null && (e.getKind() == ElementKind.CLASS || e.getKind() == ElementKind.INTERFACE);
    }

    private static PackageElement getTypePackage(TypeElement te) {
        Element parent = te.getEnclosingElement();
        while (parent != null && parent.getKind() != ElementKind.PACKAGE) {
            parent = parent.getEnclosingElement();
        }
        return (PackageElement)parent;
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
