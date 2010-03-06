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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
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

package org.netbeans.modules.javafx.refactoring.repository;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.MemberSelectTree;
import com.sun.javafx.api.tree.ObjectLiteralPartTree;
import com.sun.javafx.api.tree.OnReplaceTree;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.SyntheticTree.SynthType;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.UnitTree;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javafx.api.JavafxcTrees;
import com.sun.tools.javafx.tree.JFXClassDeclaration;
import com.sun.tools.javafx.tree.JFXFunctionDefinition;
import com.sun.tools.javafx.tree.JFXIdent;
import com.sun.tools.javafx.tree.JFXObjectLiteralPart;
import com.sun.tools.javafx.tree.JFXOverrideClassVar;
import com.sun.tools.javafx.tree.JFXTree;
import com.sun.tools.javafx.tree.JFXVar;
import com.sun.tools.javafx.tree.JavafxTreeInfo;
import com.sun.tools.mjavac.tree.JCTree;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.JavaFXSourceUtils;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.javafx.refactoring.RefactoringSupport;
import org.openide.filesystems.FileObject;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
final public class ClassModelFactory {
//    final private static Map<RefactoringSession, ClassModelFactory> factories = new WeakHashMap<RefactoringSession, ClassModelFactory>();

    final private static class ClassModelPopulator extends JavaFXTreePathScanner<Void, ClassModel> {
        private CompilationController cc;
        private SourcePositions positions;
        private boolean inImport = false;
        private int localCounter = 0;

        private Set<ExpressionTree> superTypes = new HashSet<ExpressionTree>();

        public ClassModelPopulator(CompilationController cc) {
            this.cc = cc;
            this.positions = cc.getTrees().getSourcePositions();
        }

        @Override
        public Void visitCompilationUnit(UnitTree node, ClassModel p) {
            ExpressionTree pnt = node.getPackageName();
            if (pnt != null) {
                int startFQN = (int)positions.getStartPosition(node, pnt);
                int endFQN = (int)positions.getEndPosition(node, pnt);
                int startPos = startFQN;
                int endPos = endFQN;
                TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
                ts.move(startFQN);
                while (ts.movePrevious()) {
                    Token<JFXTokenId> t = ts.token();
                    startPos -= t.length();
                    if (t.id() == JFXTokenId.PACKAGE) break;
                }
                ts.move(endFQN);
                while (ts.moveNext()) {
                    Token<JFXTokenId> t = ts.token();
                    endPos += t.length();
                    if (t.id() == JFXTokenId.SEMI) break;
                }
                while (ts.moveNext()) {
                    Token<JFXTokenId> t = ts.token();
                    if (t.id() != JFXTokenId.WS) break;
                    endPos += t.length();
                    if (t.text().charAt(0) == '\n') break;
                }
                ElementDef def = new PackageDef(pnt.toString(), startPos, endPos, startFQN, endFQN, p);
                p.setPackageDef(def, startPos, endPos);
            }
            return super.visitCompilationUnit(node, p);
        }

        @Override
        public Void visitClassDeclaration(ClassDeclarationTree node, ClassModel p) {
            if (isSynthetic(node)) return super.visitClassDeclaration(node, p);

            Element e = ((JFXClassDeclaration)node).sym;
            if (((TypeElement)e).getNestingKind() == NestingKind.TOP_LEVEL || (!cc.getElementUtilities().isSynthetic(e) && ((JFXClassDeclaration)node).mods.getGenType() != SynthType.SYNTHETIC)) {
                superTypes.addAll(node.getSupertypeList());
                ElementDef def = getClassDef((TypeElement)e, p);
                if (def != null) {
                    p.addDef(def);
                    if (!def.isSynthetic()) {
                        p.addUsage(new Usage(def.getStartFQN(), def.getEndFQN(), def));
                    }
                    for(ExpressionTree et : superTypes) {

                        Element se = JavafxTreeInfo.symbolFor((JFXTree)et);
                        if (se != null) {
                            Tree t = cc.getTree(se);
                            if (t != null) {
                                def.addOverridenDef(getClassDef((TypeElement)se, p));
                            }
                        } else {
                            // extending a java type
                            ElementHandle eh = ElementHandle.create(e);
                            if (eh != null) {
                                def.addOverridenDef(new GlobalDef(e.getSimpleName().toString(), e.getKind(), ((TypeElement)e).getNestingKind(), -1, -1, -1, -1, RefactoringSupport.getRefId(eh), p));
                            }
                        }
                    }
                }
            }
            return super.visitClassDeclaration(node, p);
        }

        @Override
        public Void visitImport(ImportTree node, ClassModel p) {
            if (isSynthetic(node)) return super.visitImport(node, p);
            
            try {
                inImport = true;
                boolean wildcard = node.getQualifiedIdentifier().toString().endsWith(".*"); // NOI18N
                Tree t = node.getQualifiedIdentifier();
                Element e = null;
                while ((e = ((JCTree)t).type != null ? ((JCTree)t).type.tsym : null) == null && t.getJavaFXKind() == Tree.JavaFXKind.MEMBER_SELECT) {
                    t = ((MemberSelectTree)t).getExpression();
                }
                if (e != null) {
                    int startPos = (int)positions.getStartPosition(cc.getCompilationUnit(), node);
                    int endPos = (int)positions.getEndPosition(cc.getCompilationUnit(), node);
                    int startFQN = (int)positions.getStartPosition(cc.getCompilationUnit(), node.getQualifiedIdentifier());
                    int endFQN = (int)positions.getEndPosition(cc.getCompilationUnit(), node.getQualifiedIdentifier());

                    boolean semiFound = false;
                    TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
                    ts.move(endPos);
                    while (ts.moveNext()) {
                        Token<JFXTokenId> token = ts.token();
                        endPos += token.length();
                        if (!semiFound) {
                            if (token.id() == JFXTokenId.SEMI) {
                                semiFound = true;
                                continue;
                            }
                        } else {
                            if (token.text().charAt(0) == '\n') {
                                break;
                            }
                        }
                    }
                    ImportEntry entry;
                    if (e.getKind() == ElementKind.PACKAGE) {
                        entry = new PackageImportEntry(((PackageElement)e).getQualifiedName().toString(), startPos, endPos, startFQN, endFQN);
                    } else {
                        entry = new TypeImportEntry(cc.getElements().getPackageOf(e).getQualifiedName().toString(), ((TypeElement)e).getQualifiedName().toString(), wildcard, startPos, endPos, startFQN, endFQN);
                    }
                    p.addImport(entry);
                }
                return super.visitImport(node, p);
            } finally {
                inImport = false;
            }
        }

        @Override
        public Void visitIdentifier(IdentifierTree node, ClassModel p) {
            if (isSynthetic(node)) return super.visitIdentifier(node, p);

            if (node != null) {
                int startPos = (int)positions.getStartPosition(cc.getCompilationUnit(), node);
                int endPos = (int)positions.getEndPosition(cc.getCompilationUnit(), node);
                // workaround for bug in javafxc reporting incorrect end position for an overridden variable
                // not reporting it when it can be workaround ... what's the point, anyway?
                int expectedEndPos = startPos + ((JFXIdent)node).getName().length();
                endPos = endPos > expectedEndPos ? endPos : expectedEndPos;
                // *** tada ***

                Element e = getElement((JFXTree)node);
                if (e != null) {
                    Tree t = cc.getTree(e);
                    if (t != null) {
                        switch (e.getKind()) {
                            case CLASS:
                            case INTERFACE: {
                                p.addUsage(new Usage(startPos, endPos, superTypes.contains(node) ? Usage.Kind.SUBTYPE : Usage.Kind.REFERENCE, getClassDef((TypeElement)e, p)));
                                break;
                            }
                            case FIELD:
                            case PARAMETER:
                            case LOCAL_VARIABLE:
                            case METHOD: {
                                if (t.getJavaFXKind() == Tree.JavaFXKind.VARIABLE) {
                                    ElementDef def= getVarDef(e, p);
                                    if (def != null) {
                                        p.addUsage(new Usage(startPos, endPos, def));
                                    }
                                } else if (t.getJavaFXKind() == Tree.JavaFXKind.FUNCTION_DEFINITION) {
                                    ElementDef def = getMethodDef((ExecutableElement)e, p);
                                    if (def != null) {
                                        p.addUsage(new Usage(startPos, endPos, def));
                                    }
                                }
                                break;
                            }
                        }
                    } else {
                        // probably referencing a java type
                        ElementHandle eh = ElementHandle.create(e);
                        if (eh != null) {
                            p.addUsage(new Usage(startPos, endPos, superTypes.contains(node) ? Usage.Kind.SUBTYPE : Usage.Kind.REFERENCE, new GlobalDef(e.getSimpleName().toString(), e.getKind(), startPos, endPos, startPos, endPos, RefactoringSupport.getRefId(eh), p)));
                        }
                    }
                }
            }

            return super.visitIdentifier(node, p);
        }

        @Override
        public Void visitObjectLiteralPart(ObjectLiteralPartTree node, ClassModel p) {
            if (isSynthetic(node)) return super.visitObjectLiteralPart(node, p);

            String name = node.getName().toString();
            int startFQN = findIdentifier(node, null, cc);
            int endFQN = startFQN + name.length();

            Element e = getElement((JFXTree)node);
            if (e != null) {
                Tree t = cc.getTree(e);
                if (t != null) {
                    p.addUsage(new Usage(startFQN, endFQN, getVarDef(e, p)));
                }
            }
            return super.visitObjectLiteralPart(node, p);
        }

        @Override
        public Void visitMemberSelect(MemberSelectTree node, ClassModel p) {
            if (isSynthetic(node)) return super.visitMemberSelect(node, p);

            Element e = getElement((JFXTree)node);
            if (e != null) {
                ElementDef def = ElementDef.NULL;
                switch (e.getKind()) {
                    case METHOD:
                    case CONSTRUCTOR: {
                        def = getMethodDef((ExecutableElement)e, p);
                        break;
                    }
                    case FIELD:
                    case PARAMETER:
                    case LOCAL_VARIABLE: {
                        def = getVarDef(e, p);
                        break;
                    }
                    case CLASS:
                    case INTERFACE: {
                        def = getClassDef((TypeElement)e, p);
                        break;
                    }
                    case PACKAGE: {
                        def = new PackageDef(node.toString());
                    }
                }
                if (def != null) {
                    int startPos = (int)positions.getStartPosition(cc.getCompilationUnit(), node) + (def.getKind() != ElementKind.PACKAGE ? node.getExpression().toString().length() + 1 : 0);
                    int endPos = def.getKind() != ElementKind.PACKAGE ? startPos + node.getIdentifier().length() : (int)positions.getEndPosition(cc.getCompilationUnit(), node);
                    p.addUsage(new Usage(startPos, endPos, def));
                }
            }
            return super.visitMemberSelect(node, p);
        }

        @Override
        public Void visitVariable(VariableTree node, ClassModel p) {
            if (isSynthetic(node)) return super.visitVariable(node, p);

            Element e = getElement((JFXTree)node);
            ElementDef def = getVarDef(e, p);
            if (def != null) {
                p.addDef(def);
                p.addUsage(new Usage(def.getStartFQN(), def.getEndFQN(), def));
            }
            return super.visitVariable(node, p);
        }

        @Override
        public Void visitOnReplace(OnReplaceTree node, ClassModel p) {
            ElementDef firstIndexDef = node.getFirstIndex() != null ? getVarDef(getElement((JFXTree)node.getFirstIndex()), p) : null;
            ElementDef lastIndexDef = node.getLastIndex() != null ? getVarDef(getElement((JFXTree)node.getLastIndex()), p) : null;
            ElementDef oldValueDef = node.getOldValue() != null ? getVarDef(getElement((JFXTree)node.getOldValue()), p) : null;
            ElementDef newValuesDef = node.getNewElements() != null ? getVarDef(getElement((JFXTree)node.getNewElements()), p) : null;

            if (firstIndexDef != null) {
                p.addDef(firstIndexDef);
                p.addUsage(new Usage(firstIndexDef.getStartFQN(), firstIndexDef.getEndFQN(), firstIndexDef));
            }

            if (lastIndexDef != null) {
                p.addDef(lastIndexDef);
                p.addUsage(new Usage(lastIndexDef.getStartFQN(), lastIndexDef.getEndFQN(), lastIndexDef));
            }

            if (oldValueDef != null) {
                p.addDef(oldValueDef);
                p.addUsage(new Usage(oldValueDef.getStartFQN(), oldValueDef.getEndFQN(), oldValueDef));
            }

            if (newValuesDef != null) {
                p.addDef(newValuesDef);
                p.addUsage(new Usage(newValuesDef.getStartFQN(), newValuesDef.getEndFQN(), newValuesDef));
            }

            return super.visitOnReplace(node, p);
        }

        @Override
        public Void visitFunctionDefinition(FunctionDefinitionTree node, ClassModel p) {
            if (isSynthetic(node)) return super.visitFunctionDefinition(node, p);

            ExecutableElement ee = (ExecutableElement)cc.getTrees().getElement(getCurrentPath());
            ElementDef def  = getMethodDef(ee, p);
            if (def != null && !def.getName().equals("javafx$run$")) {
                p.addDef(def);
                p.addUsage(new Usage(def.getStartFQN(), def.getEndFQN(), def));
            }
            return super.visitFunctionDefinition(node, p);
        }

        private void processFunctionDef(ExecutableElement ee, ElementDef def, ClassModel p) {
            for(ExecutableElement overriden : JavaFXSourceUtils.getOverridenMethods(ee, cc)) {
                GlobalDef otherDef = (GlobalDef)getMethodDef(overriden, p);
                if (otherDef != null) {
                    ((GlobalDef)def).addOverridenDef(otherDef);
                    processFunctionDef(overriden, otherDef, p);
                }
            }
        }

        private boolean isSynthetic(Tree tree) {
            int startPos = (int)positions.getStartPosition(cc.getCompilationUnit(), tree);
            int endPos = (int)positions.getEndPosition(cc.getCompilationUnit(), tree);
            return startPos == -1 || startPos == endPos;
        }

        final private Map<Object, ElementDef> defCache = new WeakHashMap<Object, ElementDef>();

        private Element getElement(JFXTree t) {
            Element e = JavafxTreeInfo.symbolFor(t);
            if (e == null) {
                // #JFXC-3789 workaround
                if (t instanceof JFXOverrideClassVar) {
                    e = ((JFXOverrideClassVar)t).sym;
                } else if (t instanceof JFXVar) {
                    // #JFXC-3917 workaround
                    e = ((JFXVar)t).getSymbol();
                }
            }
            return e;
        }

        private ElementDef getVarDef(Element e, ClassModel p) {
            VariableTree node = (VariableTree)cc.getTree(e);

            boolean missingTree = (node == null || node.getName() == null);
            ElementDef edef = defCache.get(node);
            if (edef == null) {
                String name = e.getSimpleName().toString();
                int startPos = missingTree ? -1 : (int)positions.getStartPosition(cc.getCompilationUnit(), node);
                int endPos = missingTree ? -1 : (int)positions.getEndPosition(cc.getCompilationUnit(), node);
                int startFQN = missingTree ? -1 : findIdentifier(node, null, cc);
                int endFQN = missingTree ? -1 : startFQN + name.length();
                // workaround for bug in javafxc reporting incorrect end position for an overridden variable
                // not reporting it when it can be workaround ... what's the point, anyway?
                endPos = endPos < endFQN ? endFQN : endPos;

                

                if (e != null) {
                    if (e.getKind() != ElementKind.FIELD) {
                        edef = new LocalDef(localCounter++, name, e.getKind(), startPos, endPos, startFQN, endFQN, p);
                    } else {
                        edef = new GlobalDef(name, e.getKind(), startPos, endPos, startFQN, endFQN, RefactoringSupport.getRefId(ElementHandle.create(e)), p);
                    }
                }
            }
            return edef;
        }

        private ElementDef getMethodDef(ExecutableElement e, ClassModel p) {
            Tree node = (JFXFunctionDefinition)cc.getTree(e);
            
            boolean missingTree = (node == null || ((JFXFunctionDefinition)node).getName() == null);

            ElementDef edef = missingTree ? null : defCache.get(node);
            if (edef == null) {
                String name = e.getSimpleName().toString();
                int startPos = missingTree ? -1 : (int)positions.getStartPosition(cc.getCompilationUnit(), node);
                int endPos = missingTree ? -1 : (int)positions.getEndPosition(cc.getCompilationUnit(), node);
                int startFQN = missingTree ? -1 : findIdentifier(node, JFXTokenId.FUNCTION, cc);
                int endFQN = missingTree ? -1 : startFQN + name.length();
                if (e != null) {
                    edef = new GlobalDef(name, e.getKind(), startPos, endPos, startFQN, endFQN, RefactoringSupport.getRefId(ElementHandle.create(e)), p);
                }
            }
            if (edef != null && !edef.getName().equals("javafx$run$")) {
                processFunctionDef(e, edef, p);
            }
            return edef;
        }

        private ElementDef getClassDef(TypeElement te, ClassModel p) {
            ClassDeclarationTree node = (ClassDeclarationTree)cc.getTree(te);

            boolean missingTree = (te == null || node == null);
            
            ElementDef edef = missingTree ? null : defCache.get(node);
            if (edef == null) {
                boolean isSynth = missingTree ? cc.getElementUtilities().isSynthetic(te) : ((JFXClassDeclaration)node).mods.getGenType() == SynthType.SYNTHETIC;

                int startPos = missingTree ? -1 : (int)positions.getStartPosition(cc.getCompilationUnit(), node);
                int endPos = missingTree ? -1 : (int)positions.getEndPosition(cc.getCompilationUnit(), node);

                int startFQN = missingTree ? -1 : findIdentifier(node, JFXTokenId.CLASS, cc);
                String name = te.getSimpleName().toString();
                edef = new GlobalDef(name, te.getKind(), te.getNestingKind(), startPos, endPos, startFQN, startFQN + name.length(), RefactoringSupport.getRefId(ElementHandle.create(te)), isSynth, p);
            }
            return edef;
        }

        private int findIdentifier(Tree tree, JFXTokenId after, CompilationController cc) {
            int start = (int) cc.getTrees().getSourcePositions().getStartPosition(cc.getCompilationUnit(), tree);
            TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
            ts.move(start);
            boolean classFound = false;

            while (ts.moveNext()) {
                Token<JFXTokenId> t = ts.token();
                if (t.id() == after) {
                    classFound = true;
                } else {
                    if (after == null || classFound) {
                        if (t.id() == JFXTokenId.IDENTIFIER) {
                            break;
                        }
                    }
                }
                start += t.length();
            }
            return start;
        }
    };

    final private Map<FileObject, ClassModel> classModelCache = new HashMap<FileObject, ClassModel>();

    public ClassModelFactory() {}

//    public static ClassModelFactory getInstance(RefactoringSession session) {
//        synchronized(factories) {
//            ClassModelFactory cmf = factories.get(session);
//            if (cmf == null) {
//                cmf = new ClassModelFactory();
//                factories.put(session, cmf);
//            }
//            return cmf;
//        }
//    }

    public ClassModel classModelFor(FileObject fo) {
        synchronized(classModelCache) {
            ClassModel m = classModelCache.get(fo);
            if (m == null) {
                m = createClassModel(fo);
                classModelCache.put(fo, m);
            }
            return m;
        }
    }

    private static ClassModel createClassModel(FileObject fo) {
        final ClassModel result = new ClassModel(fo);
        JavaFXSource jfxs = JavaFXSource.forFileObject(fo);
        try {
            jfxs.runUserActionTask(new Task<CompilationController>() {

                public void run(CompilationController cc) throws Exception {
                    new ClassModelPopulator(cc).scan(cc.getCompilationUnit(), result);
                }
            }, true);
            return result;
        } catch (IOException iOException) {
        }

        // fail
        return null;
    }
}
