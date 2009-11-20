/*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 *  Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
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
 *  agetCompilationController()ompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 * 
 *  Contributor(s):
 * 
 *  Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.refactoring.impl.scanners;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.FunctionInvocationTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.MemberSelectTree;
import com.sun.javafx.api.tree.ObjectLiteralPartTree;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symbol.TypeSymbol;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javafx.api.JavafxcTrees;
import com.sun.tools.javafx.tree.JFXIdent;
import com.sun.tools.javafx.tree.JFXVarScriptInit;
import java.util.Set;
import java.util.regex.Pattern;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.TreePathHandle;

/**
 *
 * @author Jaroslav Bachorik
 */
public class RenameScanner extends BaseRefactoringScanner<Void, Set<TreePathHandle>> {
    final private static String TYPE_MATCH_PATTERN = "(\\..+)*(\\[\\])*";
    final private String origSimpleName;
    final private String origQualName;

    public RenameScanner(TreePathHandle searchHandle, CompilationController cc) {
        super(searchHandle, cc);
        this.origSimpleName = searchHandle.getSimpleName();
        ElementKind kind = getElementKind();
        this.origQualName = (kind == ElementKind.CLASS || kind == ElementKind.INTERFACE || kind == ElementKind.OTHER) ? getElementHandle().getQualifiedName() : "";
    }

    @Override
    public Void visitClassDeclaration(ClassDeclarationTree node, Set<TreePathHandle> p) {
        long[] namePos = getCompilationController().getTreeUtilities().findNameSpan(node);

        if (namePos == null) return super.visitClassDeclaration(node, p); // the name is not in the source => synthetically generated class declaration

        switch (getElementKind()) {
            case CLASS:
            case INTERFACE: {
                TypeElement te = (TypeElement)getCompilationController().getTrees().getElement(getCurrentPath());
                if (Pattern.matches(origSimpleName + TYPE_MATCH_PATTERN, te.getSimpleName().toString())) {
                    p.add(TreePathHandle.create(getCurrentPath(), getCompilationController()));
                }
                break;
            }
        }
        return super.visitClassDeclaration(node, p);
    }

    @Override
    public Void visitImport(ImportTree node, Set<TreePathHandle> p) {
        switch (getElementKind()) {
            case CLASS:
            case INTERFACE: {
                String qualName = node.getQualifiedIdentifier().toString();
                if (qualName.equals(origQualName)) {
                    p.add(TreePathHandle.create(JavafxcTrees.getPath(getCurrentPath(), node.getQualifiedIdentifier()), getCompilationController()));
                    return null;
                }
                break;
            }
        }

        return super.visitImport(node, p);
    }

    @Override
    public Void visitVariable(VariableTree node, Set<TreePathHandle> p) {
        switch (getElementKind()) {
            case FIELD:
            case LOCAL_VARIABLE:
            case PARAMETER: {
                Element e = getCompilationController().getTrees().getElement(getCurrentPath());

                if (getElementKind() == e.getKind()) {
                    if (isSameElement()) {
                        if (node instanceof JFXVarScriptInit) {
                            p.add(TreePathHandle.create(JavaFXTreePath.getPath(getCompilationController().getCompilationUnit(), ((JFXVarScriptInit)node).getVar()), getCompilationController()));
                        } else {
                            p.add(TreePathHandle.create(getCurrentPath(), getCompilationController()));
                        }
                    }
                }
                break;
            }
        }

        return super.visitVariable(node, p);
    }

    @Override
    public Void visitFunctionDefinition(FunctionDefinitionTree node, Set<TreePathHandle> p) {
        if (getElementKind() != ElementKind.METHOD) return super.visitFunctionDefinition(node, p);
        ExecutableElement e = (ExecutableElement)getCompilationController().getTrees().getElement(getCurrentPath());
        ElementHandle eh = ElementHandle.create(e);

        if (eh != null && (eh.equals(getElementHandle()) || getCompilationController().getElements().overrides(e, (ExecutableElement)getElementHandle().resolve(getCompilationController()), (TypeElement)e.getEnclosingElement()))) {
            p.add(TreePathHandle.create(getCurrentPath(), getCompilationController()));
        }
        return super.visitFunctionDefinition(node, p);
    }

    @Override
    public Void visitMethodInvocation(FunctionInvocationTree node, Set<TreePathHandle> p) {
        if (getElementKind() != ElementKind.METHOD) return super.visitMethodInvocation(node, p);
        if (isSameElement()) {
            p.add(TreePathHandle.create(getCurrentPath(), getCompilationController()));
        }

        return super.visitMethodInvocation(node, p);
    }

    @Override
    public Void visitMemberSelect(MemberSelectTree node, Set<TreePathHandle> p) {
        if (!node.getIdentifier().contentEquals(origSimpleName)) return super.visitMemberSelect(node, p);
        if (getElementKind() == ElementKind.CLASS || getElementKind() == ElementKind.INTERFACE) {
            Element e = getCompilationController().getTrees().getElement(getCurrentPath());
            if (e.getKind() == getElementKind()) {
                if (((TypeElement)e).getQualifiedName().contentEquals(origQualName)) {
                    p.add(TreePathHandle.create(getCurrentPath(), getCompilationController()));
                }
            }
        } else {
            ExpressionTree expression = node.getExpression();
            if (expression instanceof JFXIdent) {
                Type type = ((JFXIdent)expression).type;
                if (type == null) return super.visitMemberSelect(node, p);
                TypeSymbol ts = type.asElement();
                if (ts.getKind() != ElementKind.CLASS) return super.visitMemberSelect(node, p);
                for(Symbol sy : ts.getEnclosedElements()) {
                    if (sy.getKind() == ElementKind.FIELD) {
                        if (getElementHandle() != null && getElementHandle().equals(ElementHandle.create(sy))) {
                            p.add(TreePathHandle.create(getCurrentPath(), getCompilationController()));
                            return null;
                        }
                    }
                }
            }
        }
        return super.visitMemberSelect(node, p);
    }

    @Override
    public Void visitIdentifier(IdentifierTree node, Set<TreePathHandle> p) {
        switch (getElementKind()) {
            case FIELD:
            case PARAMETER:
            case LOCAL_VARIABLE:
            case CLASS:
            case INTERFACE: {
                if (isSameElement()) {
                    p.add(TreePathHandle.create(getCurrentPath(), getCompilationController()));
                }
                break;
            }
        }
        return super.visitIdentifier(node, p);
    }

    @Override
    public Void visitObjectLiteralPart(ObjectLiteralPartTree node, Set<TreePathHandle> p) {
        switch (getElementKind()) {
            case FIELD:
            case PARAMETER:
            case LOCAL_VARIABLE: {
                if (isSameElement()) {
                    p.add(TreePathHandle.create(getCurrentPath(), getCompilationController()));
                }
            }
        }
        return super.visitObjectLiteralPart(node, p);
    }
}
