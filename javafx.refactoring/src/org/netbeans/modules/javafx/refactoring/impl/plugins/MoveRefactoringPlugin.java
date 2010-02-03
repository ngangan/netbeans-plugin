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
import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.MemberSelectTree;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.TypeClassTree;
import com.sun.javafx.api.tree.UnitTree;
import com.sun.tools.javafx.api.JavafxcTrees;
import com.sun.tools.mjavac.code.Symbol;
import java.io.IOException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.*;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import org.netbeans.api.fileinfo.NonRecursiveFolder;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.queries.VisibilityQuery;
import org.netbeans.modules.javafx.refactoring.transformations.Transformation;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.impl.scanners.MoveProblemCollector;
import org.netbeans.modules.javafx.refactoring.transformations.InsertTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.RemoveTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.ReplaceTextTransformation;
import org.netbeans.modules.parsing.api.indexing.IndexingManager;
import org.netbeans.modules.refactoring.api.*;
import org.netbeans.modules.refactoring.spi.ProgressProviderAdapter;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
import org.netbeans.modules.refactoring.spi.RefactoringPlugin;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;
import org.openide.util.Exceptions;
import org.openide.util.NbBundle;

/**
 * Implemented abilities:
 * <ul>
 * <li>Move file(s)</li>
 * <li>Move folder(s)</li>
 * <li>Rename folder</li>
 * <li>Rename package</li>
 * </ul>
 */
public class MoveRefactoringPlugin extends ProgressProviderAdapter implements RefactoringPlugin  {
    final private Logger LOG = Logger.getLogger(MoveRefactoringPlugin.class.getName());
    final private boolean DEBUG = LOG.isLoggable(Level.FINEST);
    
    private Map packagePostfix = new HashMap();
    final AbstractRefactoring refactoring;
    final boolean isRenameRefactoring;
    private ArrayList<FileObject> filesToMove = new ArrayList<FileObject>();
    /** list of folders grouped by source roots */
    private List<List<FileObject>> foldersToMove = new ArrayList<List<FileObject>>();

    private class ImportParts {
        final String typeName;
        final String packageName;
        final JavaFXTreePath treePath;
        final String fqn;

        public ImportParts(String typeName, String packageName, JavaFXTreePath treePath) {
            this.typeName = typeName;
            this.packageName = packageName;
            this.treePath = treePath;
            this.fqn = (packageName != null ? packageName + "." : "") + typeName;
        }
    }

    public MoveRefactoringPlugin(MoveRefactoring move) {
        this.refactoring = move;
        this.isRenameRefactoring = false;
        setup(move.getRefactoringSource().lookupAll(FileObject.class), "", true);
    }
    
    public MoveRefactoringPlugin(RenameRefactoring rename) {
        this.refactoring = rename;
        this.isRenameRefactoring = true;
        FileObject fo = rename.getRefactoringSource().lookup(FileObject.class);
        if (fo!=null) {
            setup(Collections.singletonList(fo), "", true);
        } else {
            setup(Collections.singletonList((rename.getRefactoringSource().lookup(NonRecursiveFolder.class)).getFolder()), "", false); // NOI18N
        }
    }
    
    @Override
    public Problem preCheck() {
        Problem preCheckProblem = null;
        for (FileObject file:filesToMove) {
            if (!SourceUtils.isElementInOpenProject(file)) {
                preCheckProblem = JavaFXRefactoringPlugin.createProblem(preCheckProblem, true, NbBundle.getMessage(
                        MoveRefactoringPlugin.class,
                        "ERR_ProjectNotOpened", // NOI18N
                        FileUtil.getFileDisplayName(file)));
            }
        }
        return preCheckProblem;
    }

    @Override
    public Problem checkParameters() {
        return null;
    }

    @Override
    public Problem fastCheckParameters() {
        if (isRenameRefactoring) {
            //folder rename
            FileObject f = refactoring.getRefactoringSource().lookup(FileObject.class);
            if (f!=null) {
                String newName = ((RenameRefactoring) refactoring).getNewName();
                if (!SourceUtils.isValidPackageName(newName)) {
                    String msg = new MessageFormat(NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_InvalidPackage")).format( // NOI18N
                            new Object[] {newName}
                    );
                    return new Problem(true, msg);
                }
                
                if (f.getParent().getFileObject(newName, f.getExt())!=null) {
                    String msg = new MessageFormat(NbBundle.getMessage(RenameRefactoringPlugin.class,"ERR_PackageExists")).format( // NOI18N
                            new Object[] {newName}
                    );
                    return new Problem(true, msg);
                }
            }
        }
        if (!isRenameRefactoring) {
            try {
                for (FileObject f: filesToMove) {
                    if (!SourceUtils.isJavaFXFile(f))
                        continue;
                    String targetPackageName = this.getTargetPackageName(f);
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
        }
        return null;
    }

    private Problem checkProjectDeps(Set<FileObject> a) {
        if (!isRenameRefactoring) {
            Set<FileObject> sourceRoots = new HashSet<FileObject>();
            for (FileObject file : filesToMove) {
                ClassPath cp = ClassPath.getClassPath(file, ClassPath.SOURCE);
                if (cp != null) {
                    FileObject root = cp.findOwnerRoot(file);
                    sourceRoots.add(root);
                }
            }
            URL target = ((MoveRefactoring) refactoring).getTarget().lookup(URL.class);
            if (target == null) {
                return null;
            }
            try {
                FileObject r = SourceUtils.getClassPathRoot(target);
                URL targetUrl = URLMapper.findURL(r, URLMapper.EXTERNAL);
                Set<URL> deps = SourceUtils.getDependentRoots(targetUrl);
                for (FileObject sourceRoot : sourceRoots) {
                    URL sourceUrl = URLMapper.findURL(sourceRoot, URLMapper.INTERNAL);
                    if (!deps.contains(sourceUrl)) {
                        Project sourceProject = FileOwnerQuery.getOwner(sourceRoot);
                        for (FileObject affected: a) {
                            if (FileOwnerQuery.getOwner(affected).equals(sourceProject) && !filesToMove.contains(affected)) {
                                Project targetProject = FileOwnerQuery.getOwner(r);
                                assert sourceProject!=null;
                                assert targetProject!=null;
                                String sourceName = ProjectUtils.getInformation(sourceProject).getDisplayName();
                                String targetName = ProjectUtils.getInformation(targetProject).getDisplayName();
                                return JavaFXRefactoringPlugin.createProblem(null, false, NbBundle.getMessage(MoveRefactoringPlugin.class, "ERR_MissingProjectDeps", sourceName, targetName)); // NOI18N
                            }
                        }
                    }
                }
            } catch (IOException iOException) {
                Exceptions.printStackTrace(iOException);
            }
        }
        return null;
    }

    private void collectMoveData(final Map<String, String> renameMap, final Set<FileObject> related, final Set<String> movedClasses) {
        for(final FileObject fo : filesToMove) {
            final String targetPkg = getTargetPackageName(fo);

            JavaFXSource jfxs = JavaFXSource.forFileObject(fo);
            try {
                jfxs.runUserActionTask(new Task<CompilationController>() {

                    public void run(final CompilationController cc) throws Exception {
                        new JavaFXTreePathScanner<Void, Void>() {

                            @Override
                            public Void visitCompilationUnit(UnitTree node, Void p) {
                                ExpressionTree packageNameTree = node.getPackageName();
                                String packageName = packageNameTree != null ? packageNameTree.toString() : "";
                                renameMap.put(packageName, targetPkg);
                                return super.visitCompilationUnit(node, p);
                            }

                            @Override
                            public Void visitClassDeclaration(ClassDeclarationTree node, Void p) {
                                ElementHandle eh = ElementHandle.create(cc.getTrees().getElement(getCurrentPath()));
                                movedClasses.add(eh.getQualifiedName());
                                related.addAll(cc.getClasspathInfo().getClassIndex().getResources(eh, EnumSet.of(ClassIndex.SearchKind.TYPE_REFERENCES), EnumSet.allOf(ClassIndex.SearchScope.class)));
                                return super.visitClassDeclaration(node, p);
                            }


                        }.scan(cc.getCompilationUnit(), null);
                    }
                }, false);
            } catch (IOException e) {}
        }
    }

    private class RenamePackage extends BaseRefactoringElementImplementation {
        private Map<String, String> renameMap;

        public RenamePackage(FileObject srcFO, Map<String, String> renameMap, RefactoringSession session) {
            super(srcFO, session);
            this.renameMap = renameMap;
        }

        @Override
        protected Set<Transformation> prepareTransformations(final CompilationController cc) {
            final Set<Transformation> transformations = new HashSet<Transformation>();

            JavaFXTreePathScanner<Void, Void> scanner = new JavaFXTreePathScanner<Void, Void>() {
                @Override
                public Void visitCompilationUnit(UnitTree node, Void p) {
                    ExpressionTree packageNameTree = node.getPackageName();
                    String packageName = packageNameTree != null ? packageNameTree.toString() : ""; // NOI18N
                    String targetPkg = renameMap.get(packageName);
                    if (targetPkg != null) {
                        if (targetPkg.equals(packageName)) return null; // the same package

                        if (targetPkg.length() > 0) { // non-default target package
                            if (packageName.length() > 0) { // from non-default package
                                JavaFXTreePath tp = JavafxcTrees.getPath(getCurrentPath(), node.getPackageName());
                                int pos = (int)cc.getTrees().getSourcePositions().getStartPosition(node, node.getPackageName());
                                transformations.add(new ReplaceTextTransformation(pos, packageName, targetPkg));
                            } else {
                                moveFromDefault(targetPkg);
                            }
                        } else { // default target package
                            moveToDefault(packageNameTree);
                        }
                    }
                    return super.visitCompilationUnit(node, p);
                }

                private void moveToDefault(ExpressionTree packageNameTree) {
                    // default package
                    TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
                    // set position to the first character of the package name
                    int startPos = (int) cc.getTrees().getSourcePositions().getStartPosition(cc.getCompilationUnit(), packageNameTree);
                    ts.move(startPos);
                    // search the "package" keyword (backwards)
                    ts.movePrevious();
                    Token<JFXTokenId> t = ts.token();
                    while (t.id() != JFXTokenId.PACKAGE && ts.movePrevious()) {
                        t = ts.token();
                        startPos -= t.length();
                    }
                    if (t.id() == JFXTokenId.PACKAGE && ts.movePrevious()) {
                        t = ts.token();
                        while (t.id() == JFXTokenId.WS && ts.movePrevious()) {
                            startPos -= t.length();
                            t = ts.token();
                        }
                        // set position to the last character of the package name
                        int endPos = (int) cc.getTrees().getSourcePositions().getEndPosition(cc.getCompilationUnit(), packageNameTree);
                        ts.move(endPos);
                        ts.moveNext();
                        t = ts.token();
                        while (t.id() != JFXTokenId.SEMI && ts.moveNext()) {
                            endPos += t.length();
                            t = ts.token();
                        }
                        transformations.add(new RemoveTextTransformation(startPos, endPos - startPos + 1));
                    }
                }

                private void moveFromDefault(String targetPkg) {
                    TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
                    // skip comments; possibly license and other headers
                    int startPos = 0;
                    ts.moveStart();
                    ts.moveNext();
                    Token<JFXTokenId> t = ts.token();
                    while (t.id() == JFXTokenId.COMMENT && ts.moveNext()) {
                        startPos += t.length();
                        t = ts.token();
                    }
                    boolean appendCRLF = false;
                    if (ts.moveNext()) {
                        t = ts.token();
                        appendCRLF = !t.text().toString().equals("\n"); // NOI18N
                    }
                    transformations.add(new InsertTextTransformation(startPos, "package " + targetPkg + ";" + (appendCRLF ? "\n" : ""))); // NOI18N
                }
            };
            scanner.scan(cc.getCompilationUnit(), null);
            return transformations;
        }

        public String getDisplayText() {
            return "Rename package";
        }

    }

    private class FixImports extends BaseRefactoringElementImplementation {
        private class Import {
            String fqn, typeName, packageName;
            int start, end, startFQN, endFQN;

            public Import(String packageName, String typeName, int start, int end, int startFQN, int endFQN) {
                this(packageName + "." + typeName, start, end, startFQN, endFQN);
                this.packageName = packageName;
                this.typeName = typeName;
            }

             public Import(String packageName, String typeName) {
                 this(packageName, typeName, -1, -1, -1, -1);
             }

            public Import(String fqn, int start, int end, int startFQN, int endFQN) {
                this.fqn = fqn;
                this.start = start;
                this.end = end;
                this.startFQN = startFQN;
                this.endFQN = endFQN;
            }

            public Import(String fqn) {
                this(fqn, -1, -1, -1, -1);
            }
        }

        private Set<String> movedClasses;
        private Map<String, String> renameMap;

        public FixImports(FileObject srcFO, Map<String, String> renameMap, Set<String> movedClasses, RefactoringSession session) {
            super(srcFO, session);
            this.renameMap = renameMap;
            this.movedClasses = movedClasses;
        }

        @Override
        protected Set<Transformation> prepareTransformations(final CompilationController cc) {
            final Set<Transformation> transformations = new HashSet<Transformation>();
            final int[] importLastLine = new int[]{-1};
            final boolean[] addTail = new boolean[1];
            final List<Import> imports = new ArrayList<Import>();
            final List<Import> origImports = new ArrayList<Import>();

            JavaFXTreePathScanner<Void, Void> scanner = new JavaFXTreePathScanner<Void, Void>() {
                private String myPkgName = ""; // NOI18N

                private boolean handlingImport = false;
                private TypeMirror currentClass;

                @Override
                public Void visitCompilationUnit(UnitTree node, Void p) {
                    myPkgName = node.getPackageName() != null ? node.getPackageName().toString() : "";
                    return super.visitCompilationUnit(node, p);
                }

                @Override
                public Void visitImport(ImportTree node, Void p) {
                    try {
                        handlingImport = true;
                        Tree qualidTree = node.getQualifiedIdentifier();
                        if (qualidTree != null && (qualidTree.getJavaFXKind() == Tree.JavaFXKind.MEMBER_SELECT || qualidTree.getJavaFXKind() == Tree.JavaFXKind.IDENTIFIER)) {
//                            String name = qualidTree.toString();
                            SourcePositions sp = cc.getTrees().getSourcePositions();
                            int start = (int)sp.getStartPosition(cc.getCompilationUnit(), node);
                            int end = (int)sp.getEndPosition(cc.getCompilationUnit(), node);
                            int startFQN = (int)sp.getStartPosition(cc.getCompilationUnit(), node.getQualifiedIdentifier());
                            int endFQN = (int)sp.getEndPosition(cc.getCompilationUnit(), node.getQualifiedIdentifier());

                            ImportParts parts = getImportParts((MemberSelectTree)qualidTree);
                            if (DEBUG) {
                                LOG.finest("Import: " + parts.packageName + "." + parts.typeName); // NOI18N
                            }

                            origImports.add(new Import(parts.packageName, parts.typeName, start, end, startFQN, endFQN));
                            if (movedClasses.contains(parts.fqn)) {
                                imports.add(new Import(renameMap.get(parts.packageName) != null ? renameMap.get(parts.packageName) : parts.packageName, parts.typeName, start, end, startFQN, endFQN));
                            }
                        }
                        addTail[0] = true;
                        importLastLine[0] = (int)cc.getTrees().getSourcePositions().getEndPosition(cc.getCompilationUnit(), node);
                        return super.visitImport(node, p);
                    } finally {
                        handlingImport = false;
                    }
                }

                @Override
                public Void visitClassDeclaration(ClassDeclarationTree node, Void p) {
                    if (importLastLine[0] == -1) {
                        addTail[0] = false;
                        importLastLine[0] = (int)cc.getTrees().getSourcePositions().getStartPosition(cc.getCompilationUnit(), node);
                    }
                    TypeElement te = (TypeElement)cc.getTrees().getElement(getCurrentPath());
                    currentClass = te.asType();

                    // check the existing imports and remove the ones not needed
                    String thisPkg = movedClasses.contains(currentClass.toString()) ? renameMap.get(myPkgName) : myPkgName;
                    for(Iterator<Import> iter = imports.iterator();iter.hasNext();) {
                        Import imprt = iter.next();
                        String otherPkg = movedClasses.contains(imprt.fqn) ? renameMap.get(imprt.packageName) : imprt.packageName;
                        if (thisPkg.equals(otherPkg)) { // import from the same package can be removed
                            transformations.add(new RemoveTextTransformation(imprt.start, imprt.end - imprt.start + 2));
                            iter.remove();
                        }
                    }
                    for(Iterator<Import> iter = origImports.iterator();iter.hasNext();) {
                        Import imprt = iter.next();
                        if (thisPkg.equals(imprt.packageName)) { // import from the same package can be removed
                            transformations.add(new RemoveTextTransformation(imprt.start, imprt.end - imprt.start + 2));
                            iter.remove();
                        }
                    }
                    // end
                    // check for import renames
                    for(Import imprt : origImports) {
                        String otherPkg = movedClasses.contains(imprt.fqn) ? renameMap.get(imprt.packageName) : imprt.packageName;
                        if (isImported(otherPkg + "." + imprt.typeName, imports)) {
                            transformations.add(new ReplaceTextTransformation(imprt.startFQN, imprt.packageName, otherPkg));
                        }
                    }

                    // check the supertypes and add imports as needed
                    for(ExpressionTree et : node.getSupertypeList()) {
                        Element e = cc.getTrees().getElement(JavafxcTrees.getPath(getCurrentPath(), et));
                        if (e != null && (e.getKind() == ElementKind.CLASS || e.getKind() == ElementKind.INTERFACE)) {
                            ImportParts ip = getImportParts(et);
                            thisPkg = movedClasses.contains(currentClass.toString()) ? renameMap.get(myPkgName) : myPkgName;

                            if (ip.packageName != null) {
                                String otherPkg = movedClasses.contains(ip.fqn) ? renameMap.get(ip.packageName) : ip.packageName;
                                if (!thisPkg.equals(otherPkg)) {
                                    if (!ip.fqn.equals(et.toString())) { // not a FQN
                                        if (!isImported(otherPkg + "." + ip.typeName, movedClasses.contains(ip.fqn) ? imports : origImports)) {
                                            imports.add(new Import(otherPkg, ip.typeName));
                                        }
                                    }
                                }
                            }
                        }
                    }
                    // end
                    return super.visitClassDeclaration(node, p);
                }

                @Override
                public Void visitTypeClass(TypeClassTree node, Void p) {
//                    ExpressionTree et = node.getClassName();
//                    ImportParts parts = getImportParts(et);
//                    if (parts.fqn != null && !parts.fqn.equals(et.toString())) {
//                        String thisPkg = movedClasses.contains(currentClass.toString()) ? renameMap.get(myPkgName) : myPkgName;
//                        if (parts.packageName != null) {
//                            String otherPkg = movedClasses.contains(parts.fqn) ? renameMap.get(parts.packageName) : parts.packageName;
//
//                            if (myPkgName.equals(parts.packageName) || isImported(parts.fqn, origImports)) {
//                                if (!thisPkg.equals(otherPkg) && !isImported(otherPkg + "." + parts.typeName, imports)) {
//                                    imports.add(new Import(otherPkg, parts.typeName));
//                                }
//                            }
//                        }
//                    }
                    return super.visitTypeClass(node, p);
                }

                @Override
                public Void visitMemberSelect(MemberSelectTree node, Void p) {
                    if (currentClass != null) {
                        if (!handlingImport) {
                            ImportParts ip = getImportParts(node);
                            Element e = cc.getTrees().getElement(getCurrentPath());
                            String fqn = node.toString();
                            if (fqn != null && ip.packageName != null && fqn.startsWith(ip.packageName)) {
                                if (e != null && (e.getKind() == ElementKind.PACKAGE || e.getKind() == ElementKind.CLASS || e.getKind() == ElementKind.INTERFACE)) {
                                    if (movedClasses.contains(ip.fqn)) {
                                        String oldPkgName = ip.packageName;
                                        String newPkgName = renameMap.get(oldPkgName);
                                        int start = (int)cc.getTrees().getSourcePositions().getStartPosition(cc.getCompilationUnit(), node);
                                        transformations.add(new ReplaceTextTransformation(start, oldPkgName, newPkgName));
                                        return null;
                                    }
                                }
                            }
                        }
                    }
                    return super.visitMemberSelect(node, p);
                }

                @Override
                public Void visitIdentifier(IdentifierTree node, Void p) {
                    if (currentClass != null) {
                        ImportParts parts = getImportParts(node);
                        if (parts.fqn != null) {
                            String thisPkg = movedClasses.contains(currentClass.toString()) ? renameMap.get(myPkgName) : myPkgName;
                            if (parts.packageName != null) {
                                String otherPkg = movedClasses.contains(parts.fqn) ? renameMap.get(parts.packageName) : parts.packageName;

                                if (myPkgName.equals(parts.packageName) || isImported(parts.fqn, origImports)) {
                                    if (!thisPkg.equals(otherPkg) && !isImported(otherPkg + "." + parts.typeName, imports)) {
                                        imports.add(new Import(otherPkg, parts.typeName));
                                    }
                                }
                            }
                        }
                    }
                    return super.visitIdentifier(node, p);
                }



                private ImportParts getImportParts(Tree t) {
                    JavaFXTreePath tp = JavafxcTrees.getPath(getCurrentPath(), t);
                    if (tp == null) tp = getCurrentPath();
                    Element e = null;
                    while ((e = cc.getTrees().getElement(tp)) == null) {
                        if (t.getJavaFXKind() == Tree.JavaFXKind.MEMBER_SELECT) {
                            tp = JavafxcTrees.getPath(getCurrentPath(), ((MemberSelectTree)t).getExpression());
                            t = tp.getLeaf();
                        } else {
                            break;
                        }
                    }
                    String[] types = new String[2];
                    while (e != null && tp != null && e.getKind() != ElementKind.PACKAGE) {
                        if (e.getKind() == ElementKind.CLASS && ((TypeElement)e).getNestingKind() == NestingKind.TOP_LEVEL) {
                            types[0] = ((TypeElement)e).getQualifiedName().toString();
                        }
                        e = e.getEnclosingElement();
//                        if (t.getJavaFXKind() == Tree.JavaFXKind.MEMBER_SELECT) {
//                            tp = JavafxcTrees.getPath(getCurrentPath(), ((MemberSelectTree)t).getExpression());
//                            t = tp.getLeaf();
//                        } else {
//                            tp = null;
//                            t = null;
//                        }
                    }
                    if (e != null) {
                        types[1] = ((PackageElement)e).getQualifiedName().toString();
                        types[0] = types[0] != null ? types[0].substring(types[1].length() + 1) : "*";
                    }
                    return new ImportParts(types[0], types[1], tp);
                }
                private boolean isImported(String typeName, Collection<Import> usingImports) {
                    for(Import imp : usingImports) {
                        if (imp.fqn.equals(typeName) || (imp.fqn.endsWith(".*") && typeName.startsWith(imp.fqn.substring(0, imp.fqn.length() - 1)))) { // NOI18N
                            return true;
                        }
                    }
                    return false;
                }
            };
            scanner.scan(cc.getCompilationUnit(), null);
            if (importLastLine[0] > -1) {
                for(Import imprt : imports) {
                    if (imprt.start == -1) { // process only newly added imports; they don't have the position info
                        if (DEBUG) {
                            LOG.finest("adding import \"" + imprt.fqn); // NOI18N
                        }
                        transformations.add(new InsertTextTransformation(importLastLine[0], "import " + imprt.fqn + ";\n")); // NOI18N
                    }
                }
            }
            return transformations;
        }

        public String getDisplayText() {
            return "Fix Imports";
        }

    }
    
    private void renamePackages(final Map<String, String> renameMap, final RefactoringElementsBag elements) {
        for(FileObject fo : filesToMove) {
            RenamePackage rp = new RenamePackage(fo, renameMap, elements.getSession());
            if (rp.hasChanges()) {
                elements.add(refactoring, rp);
            }
        }
    }

    private void fixImports(final Map<String, String> renameMap, final Collection<FileObject> files, final Set<String> movedClasses, final RefactoringElementsBag elements) {
        for(final FileObject fo : files) {
            FixImports elem = new FixImports(fo, renameMap, movedClasses, elements.getSession());
            if (elem.hasChanges()) {
                elements.add(refactoring, elem);
            }
        }
    }

    private Problem collectProblems(Collection<FileObject> files, final Set<String> movedClasses, final Map<String, String> renameMap) {
        final Problem[] p = new Problem[] {null};
        for(FileObject fo : files) {
            JavaFXSource jfxs = JavaFXSource.forFileObject(fo);
            try {
                jfxs.runUserActionTask(new Task<CompilationController>() {
                    public void run(final CompilationController cc) throws Exception {
                        MoveProblemCollector mpc = new MoveProblemCollector(cc, movedClasses, renameMap);
                        mpc.scan(cc.getCompilationUnit(), null);
                        p[0] = chainProblems(p[0], mpc.getProblem());
                    }
                }, true);
            } catch (IOException iOException) {
                LOG.log(Level.WARNING, null, iOException);
            }
        }
        return p[0];
    }

    public Problem prepare(RefactoringElementsBag elements) {
        fireProgressListenerStart(ProgressEvent.START, 20);

        Set<String> movedClasses = new HashSet<String>();
        Set<FileObject> related = new HashSet<FileObject>();
        Map<String, String> renameMap = new HashMap<String, String>();

        collectMoveData(renameMap, related, movedClasses);
        fireProgressListenerStep(2);
        IndexingManager.getDefault().refreshAllIndices((FileObject[])filesToMove.toArray(new FileObject[filesToMove.size()]));
        fireProgressListenerStep(4);
        IndexingManager.getDefault().refreshAllIndices((FileObject[])related.toArray(new FileObject[related.size()]));
        fireProgressListenerStep(5);
        Problem p = null;
        p = chainProblems(p, checkProjectDeps(related));
        p = chainProblems(p, collectProblems(filesToMove, movedClasses, renameMap));
        p = chainProblems(p, collectProblems(related, movedClasses, renameMap));
        fireProgressListenerStep(3);
        fixImports(renameMap, filesToMove, movedClasses, elements);
        fireProgressListenerStep(1);
        fixImports(renameMap, related, movedClasses, elements);
        fireProgressListenerStep(2);
        renamePackages(renameMap, elements); // this must be the last step otherwise ...
        fireProgressListenerStop();
        return p;
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

    String getNewPackageName() {
        if (isRenameRefactoring) {
            return ((RenameRefactoring) refactoring).getNewName();
        } else {
            // XXX cache it !!!
            return SourceUtils.getPackageName(((MoveRefactoring) refactoring).getTarget().lookup(URL.class));
        }
    }
    
    String getTargetPackageName(FileObject fo) {
        if (isRenameRefactoring) {
            if (refactoring.getRefactoringSource().lookup(NonRecursiveFolder.class) !=null)
                //package rename
                return getNewPackageName();
            else {
                //folder rename
                FileObject folder = refactoring.getRefactoringSource().lookup(FileObject.class);
                ClassPath cp = ClassPath.getClassPath(folder, ClassPath.SOURCE);
                FileObject root = cp.findOwnerRoot(folder);
                String prefix = FileUtil.getRelativePath(root, folder.getParent()).replace('/','.'); // NOI18N
                String postfix = FileUtil.getRelativePath(folder, fo.isFolder() ? fo : fo.getParent()).replace('/', '.'); // NOI18N
                String t = concat(prefix, getNewPackageName(), postfix);
                return t;
            }
        } else if (packagePostfix != null) {
            if (fo == null) {
                return getNewPackageName();
            }
            String postfix = (String) packagePostfix.get(fo);
            String packageName = concat(null, getNewPackageName(), postfix);
            return packageName;
        } else
            return getNewPackageName();
    }
    
    private void setup(Collection fileObjects, String postfix, boolean recursively) {
        setup(fileObjects, postfix, recursively, null);
    }
    
    private void setup(Collection fileObjects, String postfix, boolean recursively, List<FileObject> sameRootList) {
        for (Iterator i = fileObjects.iterator(); i.hasNext(); ) {
            FileObject fo = (FileObject) i.next();
            if (SourceUtils.isJavaFXFile(fo)) {
                packagePostfix.put(fo, postfix.replace('/', '.')); // NOI18N
                filesToMove.add(fo);
            } else if (!(fo.isFolder())) {
                packagePostfix.put(fo, postfix.replace('/', '.')); // NOI18N
            } else if (VisibilityQuery.getDefault().isVisible(fo)) {
                //o instanceof DataFolder
                //CVS folders are ignored
                boolean addDot = !"".equals(postfix); // NOI18N
                Collection col = new ArrayList();
                for (FileObject fo2: fo.getChildren()) {
                    if (!fo2.isFolder() || (fo2.isFolder() && recursively)) 
                        col.add(fo2);
                }
                List<FileObject> curRootList = sameRootList;
                if (sameRootList == null) {
                    curRootList = new ArrayList<FileObject>();
                    foldersToMove.add(curRootList);
                }
                curRootList.add(fo);
                setup(col,
                        postfix + (addDot ? "." : "") + fo.getName(), // NOI18N
                        recursively,
                        curRootList);
            }
        }
    }
 
    private String concat(String s1, String s2, String s3) {
        String result = ""; // NOI18N
        if (s1 != null && !"".equals(s1)) { // NOI18N
            result += s1 + "."; // NOI18N
        }
        result +=s2;
        if (s3 != null && !"".equals(s3)) { // NOI18N
            result += ("".equals(result)? "" : ".") + s3; // NOI18N
        }
        return result;
    }        

    public void cancelRequest() {
        // do nothing
    }


}    
