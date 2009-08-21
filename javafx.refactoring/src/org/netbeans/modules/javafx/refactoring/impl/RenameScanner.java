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

package org.netbeans.modules.javafx.refactoring.impl;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.InstantiateTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.TypeClassTree;
import com.sun.tools.javafx.api.JavafxcTrees;
import java.util.Set;
import java.util.regex.Pattern;
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
        this.origQualName = origHandle.getQualifiedName();
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
}
