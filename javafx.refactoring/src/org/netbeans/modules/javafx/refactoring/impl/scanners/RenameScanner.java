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
 *  accompanied this code. If applicable, add the following below the
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
import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.InstantiateTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.MemberSelectTree;
import com.sun.javafx.api.tree.TypeClassTree;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symbol.TypeSymbol;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javafx.api.JavafxcTrees;
import com.sun.tools.javafx.tree.JFXIdent;
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
public class RenameScanner extends JavaFXTreePathScanner<Void, Set<TreePathHandle>> {
    final private static String TYPE_MATCH_PATTERN = "(\\..+)*(\\[\\])*";
    final private String origSimpleName;
    final private String origQualName;
    final private ElementHandle origHandle;
    final private CompilationController cc;

    public RenameScanner(ElementHandle origElementHandle, CompilationController cc) {
        this.origHandle = origElementHandle;
        this.origSimpleName = origElementHandle.resolve(cc).getSimpleName().toString();
        this.origQualName = (origHandle.getKind() == ElementKind.CLASS || origHandle.getKind() == ElementKind.INTERFACE) ? origHandle.getQualifiedName() : "";
        this.cc = cc;
    }

    @Override
    public Void visitClassDeclaration(ClassDeclarationTree node, Set<TreePathHandle> p) {
        switch (origHandle.getKind()) {
            case CLASS:
            case INTERFACE: {
                TypeElement te = (TypeElement)cc.getTrees().getElement(getCurrentPath());
                if (Pattern.matches(origSimpleName + TYPE_MATCH_PATTERN, te.getSimpleName().toString())) {
                    p.add(TreePathHandle.create(getCurrentPath(), cc));
                }
            }
        }
        return super.visitClassDeclaration(node, p);
    }

    @Override
    public Void visitInstantiate(InstantiateTree node, Set<TreePathHandle> p) {
        switch (origHandle.getKind()) {
            case CLASS:
            case INTERFACE: {
                TypeElement te = (TypeElement)cc.getTrees().getElement(JavafxcTrees.getPath(getCurrentPath(), node.getIdentifier()));
                String typeName = te.getQualifiedName().toString();
                if (Pattern.matches(origQualName + TYPE_MATCH_PATTERN, typeName)) {
                    p.add(TreePathHandle.create(JavafxcTrees.getPath(getCurrentPath(), node.getIdentifier()), cc));
                }
                break;
            }
        }
        return super.visitInstantiate(node, p);
    }

    @Override
    public Void visitTypeClass(TypeClassTree node, Set<TreePathHandle> p) {
        switch(origHandle.getKind()) {
            case CLASS:
            case INTERFACE: {
                TypeElement te = (TypeElement)cc.getTrees().getElement(getCurrentPath());
                String typeName = te.getQualifiedName().toString();
                if (Pattern.matches(origQualName + TYPE_MATCH_PATTERN, typeName)) {
                    p.add(TreePathHandle.create(getCurrentPath(), cc));
                }
                break;
            }
        }
        return super.visitTypeClass(node, p);
    }

    @Override
    public Void visitImport(ImportTree node, Set<TreePathHandle> p) {
        switch (origHandle.getKind()) {
            case CLASS:
            case INTERFACE: {
                String qualName = node.getQualifiedIdentifier().toString();
                if (qualName.equals(origQualName)) {
                    p.add(TreePathHandle.create(JavafxcTrees.getPath(getCurrentPath(), node.getQualifiedIdentifier()), cc));
                }
                break;
            }
        }

        return super.visitImport(node, p);
    }

    @Override
    public Void visitVariable(VariableTree node, Set<TreePathHandle> p) {
        Element e = cc.getTrees().getElement(getCurrentPath());
        if (e.getKind() != ElementKind.FIELD) return super.visitVariable(node, p);
        
        ElementHandle eh = ElementHandle.create(cc.getTrees().getElement(getCurrentPath()));
        if (origHandle.equals(eh)) {
            p.add(TreePathHandle.create(getCurrentPath(), cc));
        }
        return super.visitVariable(node, p);
    }

    @Override
    public Void visitFunctionDefinition(FunctionDefinitionTree node, Set<TreePathHandle> p) {
        if (origHandle.getKind() != ElementKind.METHOD) return super.visitFunctionDefinition(node, p);
        Element e = cc.getTrees().getElement(getCurrentPath());
        ElementHandle eh = ElementHandle.create(e);

        if (origHandle.equals(eh)) {
            p.add(TreePathHandle.create(getCurrentPath(), cc));
        }
        return super.visitFunctionDefinition(node, p);
    }

    @Override
    public Void visitMethodInvocation(FunctionInvocationTree node, Set<TreePathHandle> p) {
        if (origHandle.getKind() != ElementKind.METHOD) return super.visitMethodInvocation(node, p);
        Element e = cc.getTrees().getElement(getCurrentPath());
        ExecutableElement ee = (ExecutableElement)e;
        System.err.println("Return type of " + e + " : " + ee.getReturnType());
        ElementHandle eh = ElementHandle.create(e);

        if (origHandle.equals(eh)) {
            p.add(TreePathHandle.create(getCurrentPath(), cc));
        }
        
        return super.visitMethodInvocation(node, p);
    }

    @Override
    public Void visitMemberSelect(MemberSelectTree node, Set<TreePathHandle> p) {
        if (!node.getIdentifier().contentEquals(origSimpleName)) return super.visitMemberSelect(node, p);

        ExpressionTree expression = node.getExpression();
        if (expression instanceof JFXIdent) {
            Type type = ((JFXIdent)expression).type;
            if (type == null) return super.visitMemberSelect(node, p);
            TypeSymbol ts = type.asElement();
            if (ts.getKind() != ElementKind.CLASS) return super.visitMemberSelect(node, p);
            for(Symbol sy : ts.getEnclosedElements()) {
                if (sy.getKind() == ElementKind.FIELD) {
                    ElementHandle eh = ElementHandle.create(sy);
                    if (origHandle.equals(eh)) {
                        p.add(TreePathHandle.create(getCurrentPath(), cc));
                    }
                }
            }
        }
        return super.visitMemberSelect(node, p);
    }


}
