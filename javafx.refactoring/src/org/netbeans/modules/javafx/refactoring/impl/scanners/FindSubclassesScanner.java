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
public class FindSubclassesScanner extends BaseRefactoringScanner<Void, RefactoringElementsBag> {
    private ElementHandle elementHandle;
    private String targetName;
    private AbstractRefactoring refactoring;

    public FindSubclassesScanner(WhereUsedQuery refactoring, TreePathHandle handle, CompilationController cc) {
        this(refactoring, handle, ElementHandle.create(handle.resolveElement(cc)), cc);
    }

    public FindSubclassesScanner(WhereUsedQuery refactoring, TreePathHandle handle, ElementHandle elementHandle, CompilationController cc) {
        super(handle, cc);
        this.elementHandle = elementHandle;
        this.targetName = elementHandle.getQualifiedName();
        this.refactoring = refactoring;
    }

    @Override
    public Void visitClassDeclaration(ClassDeclarationTree node, RefactoringElementsBag elements) {
        switch (elementHandle.getKind()) {
            case CLASS:
            case INTERFACE: {
                TypeElement te = (TypeElement)getCompilationController().getTrees().getElement(getCurrentPath());
                if (targetName.equals(te.getQualifiedName().toString())) {
                    elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(getCurrentPath(), getCompilationController()), Lookups.singleton(getSearchHandle())));
                }
            }
        }
        return super.visitClassDeclaration(node, elements);
    }
}
