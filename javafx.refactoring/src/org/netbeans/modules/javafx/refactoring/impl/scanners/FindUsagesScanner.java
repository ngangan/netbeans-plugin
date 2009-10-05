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
import com.sun.javafx.api.tree.FunctionInvocationTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.InstantiateTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.MemberSelectTree;
import com.sun.javafx.api.tree.ObjectLiteralPartTree;
import com.sun.javafx.api.tree.TypeClassTree;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symbol.TypeSymbol;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javafx.api.JavafxcTrees;
import com.sun.tools.javafx.tree.JFXIdent;
import java.util.regex.Pattern;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.refactoring.impl.WhereUsedElement;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.TreePathHandle;
import org.netbeans.modules.refactoring.api.AbstractRefactoring;
import org.netbeans.modules.refactoring.api.WhereUsedQuery;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
import org.openide.util.lookup.Lookups;

/**
 *
 * @author Jaroslav Bachorik
 */
public class FindUsagesScanner extends BaseRefactoringScanner<Void, RefactoringElementsBag> {
    final private static String TYPE_MATCH_PATTERN = "(\\[\\])*";

    private ElementHandle elementHandle;
    private String targetName;
    private AbstractRefactoring refactoring;

    public FindUsagesScanner(WhereUsedQuery refactoring, TreePathHandle handle, CompilationController cc) {
        this(refactoring, handle, ElementHandle.create(handle.resolveElement(cc)), cc);
    }

    public FindUsagesScanner(WhereUsedQuery refactoring, TreePathHandle handle, ElementHandle elementHandle, CompilationController cc) {
        super(handle, elementHandle, cc);
        this.elementHandle = elementHandle;
        this.targetName = handle.getSimpleName();
        this.refactoring = refactoring;
    }

    @Override
    public Void visitClassDeclaration(ClassDeclarationTree node, RefactoringElementsBag elements) {
        switch (elementHandle.getKind()) {
            case CLASS:
            case INTERFACE: {
                TypeElement te = (TypeElement)getCompilationController().getTrees().getElement(getCurrentPath());
                if (Pattern.matches(targetName + TYPE_MATCH_PATTERN, te.getSimpleName().toString())) {
                    elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), getCompilationController()), Lookups.singleton(getTreePathHandle())));
                }
                for(ExpressionTree et : node.getSupertypeList()) {
                    JavaFXTreePath path = JavafxcTrees.getPath(getCurrentPath(), et);
                    te = (TypeElement)getCompilationController().getTrees().getElement(path);
                    if (Pattern.matches(elementHandle.getQualifiedName() + TYPE_MATCH_PATTERN, te.getQualifiedName().toString())) {
                        elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(path, getCompilationController()), Lookups.singleton(getTreePathHandle())));
                    }
                }
            }
        }
        return super.visitClassDeclaration(node, elements);
    }

    @Override
    public Void visitInstantiate(InstantiateTree node, RefactoringElementsBag elements) {
        switch (elementHandle.getKind()) {
            case CLASS:
            case INTERFACE: {
                TypeElement te = (TypeElement)getCompilationController().getTrees().getElement(JavafxcTrees.getPath(getCurrentPath(), node.getIdentifier()));
                String typeName = te.getQualifiedName().toString();
                if (Pattern.matches(elementHandle.getQualifiedName() + TYPE_MATCH_PATTERN, typeName)) {
                    elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(JavafxcTrees.getPath(getCurrentPath(), node.getIdentifier()), getCompilationController()), Lookups.singleton(getTreePathHandle())));
                }
                break;
            }
        }
        return super.visitInstantiate(node, elements);
    }

    @Override
    public Void visitTypeClass(TypeClassTree node, RefactoringElementsBag elements) {
        switch(elementHandle.getKind()) {
            case CLASS:
            case INTERFACE: {
                TypeElement te = (TypeElement)getCompilationController().getTrees().getElement(getCurrentPath());
                String typeName = te.getQualifiedName().toString();
                if (Pattern.matches(elementHandle.getQualifiedName() + TYPE_MATCH_PATTERN, typeName)) {
                    elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), getCompilationController()), Lookups.singleton(getTreePathHandle())));
                }
                break;
            }
        }
        return super.visitTypeClass(node, elements);
    }

    @Override
    public Void visitVariable(VariableTree node, RefactoringElementsBag elements) {        
        switch (elementHandle.getKind()) {
            case FIELD:
            case LOCAL_VARIABLE:
            case PARAMETER: {
                if (isSameElement()) {
                    elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), getCompilationController()), Lookups.singleton(getTreePathHandle())));
                }
            }
        }

        return super.visitVariable(node, elements);
    }

    @Override
    public Void visitMethodInvocation(FunctionInvocationTree node, RefactoringElementsBag elements) { 
        if (elementHandle.getKind() == ElementKind.METHOD) {
            if (isSameElement()) {
                elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), getCompilationController()), Lookups.singleton(getTreePathHandle())));
            }
        }

        return super.visitMethodInvocation(node, elements);
    }


    @Override
    public Void visitMemberSelect(MemberSelectTree node, RefactoringElementsBag elements) {
        if (node.getIdentifier().contentEquals(targetName)) {
            ExpressionTree expression = node.getExpression();
            if (expression instanceof JFXIdent) {
                Type type = ((JFXIdent)expression).type;
                if (type == null) return super.visitMemberSelect(node, elements);
                TypeSymbol ts = type.asElement();
                if (ts.getKind() != ElementKind.CLASS) return super.visitMemberSelect(node, elements);
                for(Symbol sy : ts.getEnclosedElements()) {
                    if (sy.getKind() == ElementKind.FIELD) {
                        if (elementHandle != null && elementHandle.equals(ElementHandle.create(sy))) {
                            elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), getCompilationController()), Lookups.singleton(getTreePathHandle())));
                            return null;
                        }
                    }
                }
            }
        }
        return super.visitMemberSelect(node, elements);
    }

    @Override
    public Void visitIdentifier(IdentifierTree node, RefactoringElementsBag elements) {
        switch (elementHandle.getKind()) {
            case FIELD:
            case PARAMETER:
            case LOCAL_VARIABLE: {
                if (isSameElement()) {
                    elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), getCompilationController()), Lookups.singleton(getTreePathHandle())));
                }
                break;
            }
        }
        return super.visitIdentifier(node, elements);
    }

    @Override
    public Void visitObjectLiteralPart(ObjectLiteralPartTree node, RefactoringElementsBag elements) {
        if (isSameElement()) {
            elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), getCompilationController()), Lookups.singleton(getTreePathHandle())));
        }
        return super.visitObjectLiteralPart(node, elements);
    }
}
