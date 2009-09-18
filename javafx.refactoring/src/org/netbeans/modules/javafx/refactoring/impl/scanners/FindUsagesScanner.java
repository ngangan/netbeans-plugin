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
import com.sun.javafx.api.tree.FunctionInvocationTree;
import com.sun.javafx.api.tree.InstantiateTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.MemberSelectTree;
import com.sun.javafx.api.tree.TypeClassTree;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javafx.api.JavafxcTrees;
import java.util.regex.Pattern;
import javax.lang.model.element.Element;
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
public class FindUsagesScanner extends JavaFXTreePathScanner<Void, RefactoringElementsBag> {
    final private static String TYPE_MATCH_PATTERN = "(\\[\\])*";

    private TreePathHandle searchHandle;
    private ElementHandle elementHandle;
    private String targetName;
    private AbstractRefactoring refactoring;

    private CompilationController cc;

    public FindUsagesScanner(WhereUsedQuery refactoring, TreePathHandle handle, CompilationController cc) {
        this(refactoring, handle, ElementHandle.create(handle.resolveElement(cc)), cc);
    }

    public FindUsagesScanner(WhereUsedQuery refactoring, TreePathHandle handle, ElementHandle elementHandle, CompilationController cc) {
        this.searchHandle = handle;
        this.elementHandle = elementHandle;
        this.targetName = handle.getSimpleName();
        this.refactoring = refactoring;
        this.cc = cc;
    }

    @Override
    public Void visitClassDeclaration(ClassDeclarationTree node, RefactoringElementsBag elements) {
        switch (elementHandle.getKind()) {
            case CLASS:
            case INTERFACE: {
                TypeElement te = (TypeElement)cc.getTrees().getElement(getCurrentPath());
                if (Pattern.matches(targetName + TYPE_MATCH_PATTERN, te.getSimpleName().toString())) {
                    elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), cc), Lookups.singleton(searchHandle)));
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
                TypeElement te = (TypeElement)cc.getTrees().getElement(JavafxcTrees.getPath(getCurrentPath(), node.getIdentifier()));
                String typeName = te.getQualifiedName().toString();
                if (Pattern.matches(elementHandle.getQualifiedName() + TYPE_MATCH_PATTERN, typeName)) {
                    elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(JavafxcTrees.getPath(getCurrentPath(), node.getIdentifier()), cc), Lookups.singleton(searchHandle)));
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
                TypeElement te = (TypeElement)cc.getTrees().getElement(getCurrentPath());
                String typeName = te.getQualifiedName().toString();
                if (Pattern.matches(elementHandle.getQualifiedName() + TYPE_MATCH_PATTERN, typeName)) {
                    elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), cc), Lookups.singleton(searchHandle)));
                }
                break;
            }
        }
        return super.visitTypeClass(node, elements);
    }

    @Override
    public Void visitVariable(VariableTree node, RefactoringElementsBag elements) {        
        if (elementHandle.getKind() == ElementKind.FIELD) {
            Element e = cc.getTrees().getElement(getCurrentPath());
            if (e.getKind() == ElementKind.FIELD) {
                ElementHandle eh = ElementHandle.create(e);
                if (eh != null && elementHandle.equals(eh)) {
                    elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), cc), Lookups.singleton(searchHandle)));
                    return null;
                }
            }
        }

        return super.visitVariable(node, elements);
    }


    volatile private boolean inMemberSelect = false;

    @Override
    public Void visitMethodInvocation(FunctionInvocationTree node, RefactoringElementsBag elements) { 
        if (!inMemberSelect) {
            if (elementHandle.getKind() == ElementKind.METHOD) {
                ElementHandle eh = ElementHandle.create(cc.getTrees().getElement(getCurrentPath()));
                if (eh != null && elementHandle.equals(eh)) {
                    elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), cc), Lookups.singleton(searchHandle)));
                    return null;
                }
        }
        }

        return super.visitMethodInvocation(node, elements);
    }


    @Override
    public Void visitMemberSelect(MemberSelectTree node, RefactoringElementsBag elements) {
        try {
            inMemberSelect = true;
            if (node.getIdentifier().contentEquals(targetName)) {
                Element e = cc.getTrees().getElement(JavafxcTrees.getPath(getCurrentPath(), node.getExpression()));
                if (e.asType().toString().equals(elementHandle.getSignatures()[0])) {
                    elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), cc), Lookups.singleton(searchHandle)));
                }
            }

            return super.visitMemberSelect(node, elements);
        } finally {
            inMemberSelect = false;
        }
    }

}
