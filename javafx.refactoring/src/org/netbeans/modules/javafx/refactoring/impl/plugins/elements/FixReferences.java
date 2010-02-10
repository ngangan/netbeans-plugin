/*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 *  Copyright 1997-2010 Sun Microsystems, Inc. All rights reserved.
 *
 *  The contents of this file are subject to the terms of either the GNU
 *  General Public License Version 2 only ("GPL") or the Common
 *  Development and Distribution License("CDDL") (collectively, the
 *  "License"). You may not use this file except in compliance with the
 *  License. You can obtain a copy of the License at
 *  http://www.netbeans.org/cddl-gplv2.html
 *  or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 *  specific language governing permissions and limitations under the
 *  License.  When distributing the software, include this License Header
 *  Notice in each file and include the License file at
 *  nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Sun in the GPL Version 2 section of the License file that
 *  accompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 *
 *  Contributor(s):
 *
 *  Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.refactoring.impl.plugins.elements;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.MemberSelectTree;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.UnitTree;
import com.sun.tools.javafx.api.JavafxcTrees;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.javafx.refactoring.impl.plugins.BaseRefactoringElementImplementation;
import org.netbeans.modules.javafx.refactoring.impl.plugins.MoveRefactoringPlugin;
import org.netbeans.modules.javafx.refactoring.transformations.InsertTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.RemoveTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.ReplaceTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.Transformation;
import org.netbeans.modules.refactoring.api.RefactoringSession;
import org.openide.filesystems.FileObject;

public class FixReferences extends BaseRefactoringElementImplementation {
    final private Logger LOG = Logger.getLogger(MoveRefactoringPlugin.class.getName());
    final private boolean DEBUG = LOG.isLoggable(Level.FINEST);
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
    private boolean packageRename = false;

    public FixReferences(FileObject srcFO, Map<String, String> renameMap, Set<String> movedClasses, RefactoringSession session) {
        this(srcFO, renameMap, movedClasses, false, session);
    }
    
    public FixReferences(FileObject srcFO, Map<String, String> renameMap, Set<String> movedClasses, boolean packageRename, RefactoringSession session) {
        super(srcFO, session);
        this.renameMap = renameMap;
        this.movedClasses = movedClasses;
        this.packageRename = packageRename;
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

                TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
                int pos = (int) cc.getTrees().getSourcePositions().getEndPosition(node, node.getPackageName());
                ts.move(pos);
                boolean semiFound = false;
                while (ts.moveNext()) {
                    Token<JFXTokenId> token = ts.token();
                    pos += token.length();
                    if (token.id() == JFXTokenId.SEMI) {
                        semiFound = true;
                        break;
                    }
                }
                if (semiFound) {
                    while (ts.moveNext()) {
                        Token<JFXTokenId> token = ts.token();
                        pos += token.length();
                        if (token.text().charAt(0) == '\n') {
                            importLastLine[0] = pos;
                            break;
                        }
                    }
                }

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
                        int start = (int) sp.getStartPosition(cc.getCompilationUnit(), node);
                        int end = getImportLastPos((int) sp.getEndPosition(cc.getCompilationUnit(), node), cc);
                        int startFQN = (int) sp.getStartPosition(cc.getCompilationUnit(), qualidTree);
                        int endFQN = (int) sp.getEndPosition(cc.getCompilationUnit(), qualidTree);

                        Import origImprt = getImport((MemberSelectTree) qualidTree);
                        if (DEBUG) {
                            LOG.finest("Import: " + origImprt.packageName + "." + origImprt.typeName); // NOI18N
                        }

                        if (movedClasses.contains(origImprt.fqn) || (packageRename && renameMap.containsKey(origImprt.packageName))) {
                            Import imprt = new Import(renameMap.get(origImprt.packageName) != null ? renameMap.get(origImprt.packageName) : origImprt.packageName, origImprt.typeName, start, end, startFQN, endFQN);
                            imports.add(imprt);
                            transformations.add(new ReplaceTextTransformation(startFQN, origImprt.packageName, imprt.packageName));
                        } else {
                            origImports.add(new Import(origImprt.packageName, origImprt.typeName, start, end, startFQN, endFQN));
                        }
                    }
                    addTail[0] = true;
                    importLastLine[0] = getImportLastPos((int) cc.getTrees().getSourcePositions().getEndPosition(cc.getCompilationUnit(), node), cc);
                    return super.visitImport(node, p);
                } finally {
                    handlingImport = false;
                }
            }

            @Override
            public Void visitClassDeclaration(ClassDeclarationTree node, Void p) {
                if (importLastLine[0] == -1) {
                    addTail[0] = false;
                    importLastLine[0] = (int) cc.getTrees().getSourcePositions().getStartPosition(cc.getCompilationUnit(), node);
                }
                TypeElement te = (TypeElement) cc.getTrees().getElement(getCurrentPath());
                // shortcut; don't even try to enter the mangled syntehtic inner classes generated for object literals; they !@#$ everything
                if (cc.getElementUtilities().isSynthetic(te)) {
                    return null;
                }

                currentClass = te.asType();

                // check the existing imports and remove the ones not needed
                String thisPkg = movedClasses.contains(currentClass.toString()) ? renameMap.get(myPkgName) : myPkgName;
                // remove same-package imports from the newly added ones
                for (Iterator<Import> iter = imports.iterator(); iter.hasNext();) {
                    Import imprt = iter.next();
                    String otherPkg = movedClasses.contains(imprt.fqn) ? renameMap.get(imprt.packageName) : imprt.packageName;
                    if (thisPkg.equals(otherPkg)) { // import from the same package can be removed
                        // remove any renaming transformation possibly introduced during processing imports
                        transformations.remove(new ReplaceTextTransformation(imprt.startFQN, myPkgName, imprt.packageName));
                        // and now add the transformation to remove unused import
                        transformations.add(new RemoveTextTransformation(imprt.start, imprt.end - imprt.start));
                        iter.remove();
                    }
                }
                // remove the same-package imports from the original imports
                for (Iterator<Import> iter = origImports.iterator(); iter.hasNext();) {
                    Import imprt = iter.next();
                    if (thisPkg.equals(imprt.packageName)) { // import from the same package can be removed
                        transformations.add(new RemoveTextTransformation(imprt.start, imprt.end - imprt.start));
                        iter.remove();
                    }
                }
                // end
                // check for import renames
                for (Import imprt : origImports) {
                    String otherPkg = movedClasses.contains(imprt.fqn) ? renameMap.get(imprt.packageName) : imprt.packageName;
                    if (isImported(otherPkg, imprt.typeName, imports)) {
                        transformations.add(new ReplaceTextTransformation(imprt.startFQN, imprt.packageName, otherPkg));
                    }
                }

                // check the supertypes and add imports as needed
                for (ExpressionTree et : node.getSupertypeList()) {
                    if (et != null) {
                        Element e = cc.getTrees().getElement(JavafxcTrees.getPath(getCurrentPath(), et));
                        if (e != null && (e.getKind() == ElementKind.CLASS || e.getKind() == ElementKind.INTERFACE)) {
                            Import ip = getImport(et);
                            thisPkg = movedClasses.contains(currentClass.toString()) ? renameMap.get(myPkgName) : myPkgName;

                            if (ip.packageName != null) {
                                String otherPkg = movedClasses.contains(ip.fqn) ? renameMap.get(ip.packageName) : ip.packageName;
                                if (!thisPkg.equals(otherPkg)) {
                                    if (!ip.fqn.equals(et.toString())) { // not a FQN
                                        if (!isImported(otherPkg, ip.typeName, movedClasses.contains(ip.fqn) ? imports : origImports)) {
                                            imports.add(new Import(otherPkg, ip.typeName));
                                        }
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
            public Void visitMemberSelect(MemberSelectTree node, Void p) {
                if (currentClass != null) {
                    if (!handlingImport) {
                        Import ip = getImport(node);
                        Element e = cc.getTrees().getElement(getCurrentPath());
                        String fqn = node.toString();
                        if (fqn != null && ip.packageName != null && fqn.startsWith(ip.packageName)) {
                            if (e != null && (e.getKind() == ElementKind.PACKAGE || e.getKind() == ElementKind.CLASS || e.getKind() == ElementKind.INTERFACE)) {
                                if (movedClasses.contains(ip.fqn)) {
                                    String oldPkgName = ip.packageName;
                                    String newPkgName = renameMap.get(oldPkgName);
                                    int start = (int) cc.getTrees().getSourcePositions().getStartPosition(cc.getCompilationUnit(), node);
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
                    Import parts = getImport(node);
                    if (parts.fqn != null) {
                        String thisPkg = movedClasses.contains(currentClass.toString()) ? renameMap.get(myPkgName) : myPkgName;
                        if (parts.packageName != null) {
                            String otherPkg = movedClasses.contains(parts.fqn) ? renameMap.get(parts.packageName) : parts.packageName;

                            if (myPkgName.equals(parts.packageName) || isImported(parts.packageName, parts.typeName, origImports)) {
                                if (!thisPkg.equals(otherPkg) && !isImported(otherPkg, parts.typeName, imports) && !isImported(otherPkg, parts.typeName, origImports)) {
                                    imports.add(new Import(otherPkg, parts.typeName));
                                }
                            }
                        }
                    }
                }
                return super.visitIdentifier(node, p);
            }

            private Import getImport(Tree t) {
                JavaFXTreePath tp = JavafxcTrees.getPath(getCurrentPath(), t);
                if (tp == null) {
                    tp = getCurrentPath();
                }
                int startPos = (int)cc.getTrees().getSourcePositions().getStartPosition(cc.getCompilationUnit(), t);
                int endPos = (int)cc.getTrees().getSourcePositions().getEndPosition(cc.getCompilationUnit(), t);
                if (t.toString().endsWith(".*")) {
                    return new Import(((MemberSelectTree) t).getExpression().toString(), "*", startPos, endPos, -1, -1);
                } else {
                    Element e = null;
                    while ((e = cc.getTrees().getElement(tp)) == null) {
                        if (t.getJavaFXKind() == Tree.JavaFXKind.MEMBER_SELECT) {
                            tp = JavafxcTrees.getPath(getCurrentPath(), ((MemberSelectTree) t).getExpression());
                            t = tp.getLeaf();
                        } else {
                            break;
                        }
                    }
                    String[] types = new String[2];
                    while (e != null && tp != null && e.getKind() != ElementKind.PACKAGE) {
                        if ((e.getKind() == ElementKind.CLASS || e.getKind() == ElementKind.INTERFACE) && ((TypeElement) e).getNestingKind() == NestingKind.TOP_LEVEL) {
                            types[1] = ((TypeElement) e).getQualifiedName().toString();
                        }
                        e = e.getEnclosingElement();
                    }
                    if (e != null) {
                        types[0] = ((PackageElement) e).getQualifiedName().toString();
                        types[1] = types[1] != null ? types[1].substring(types[0].length() + 1) : "*";
                    }
                    return new Import(types[0], types[1], startPos, endPos, -1, -1);
                }
            }

            private boolean isImported(String pkgName, String typeName, Collection<Import> usingImports) {
                for (Import imp : usingImports) {
                    if (imp.fqn.equals(pkgName + "." + typeName) || (imp.typeName.equals("*") && pkgName.equals(imp.packageName))) { // NOI18N
                        return true;
                    }
                }
                return false;
            }
        };
        scanner.scan(cc.getCompilationUnit(), null);
        if (importLastLine[0] > -1) {
            for (Import imprt : imports) {
                if (imprt.start == -1 && !imprt.typeName.equals("*")) { // process only newly added non-wildcard imports; they don't have the position info
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
        return "Fix References";
    }

    private int getImportLastPos(int pos, CompilationController cc) {
        TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
        ts.move(pos);
        OUTER:
        while (ts.moveNext()) {
            Token<JFXTokenId> token = ts.token();
            pos += token.length();
            if (token.id() == JFXTokenId.SEMI) {
                while (ts.moveNext()) {
                    token = ts.token();
                    pos += token.length();
                    if (token.text().charAt(0) == '\n') {
                        break OUTER;
                    }
                }
            }
        }
        return pos;
    }
}
