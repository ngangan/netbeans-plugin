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

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.UnitTree;
import com.sun.tools.javafx.api.JavafxcTrees;
import com.sun.tools.javafx.tree.JFXFunctionDefinition;
import com.sun.tools.javafx.tree.JFXScript;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.refactoring.impl.ElementLocation;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.TreePathHandle;

/**
 *
 * @author Jaroslav Bachorik
 */
public class BaseRefactoringScanner extends JavaFXTreePathScanner<Void, Set<ElementLocation>> {
    final private ElementLocation location;
    final private Element origElement;
    final private CompilationController cc;
    final private SourcePositions positions;
    
    /**
     * 
     * @param location A {@linkplain TreePathHandle} instance to rename from (if "eh" not provided it is resolved from "searchHandle")
     * @param eh If provided (not NULL) this takes precedence over the searchHandle
     * @param cc {@linkplain CompilationController} instance
     */
    public BaseRefactoringScanner(ElementLocation location, CompilationController cc) {
        this.location = location;
        this.origElement = location.getElement(cc);
        this.cc = cc;
        this.positions = cc.getTrees().getSourcePositions();
    }
    

    @Override
    final public Void scan(Tree tree, Set<ElementLocation> p) {
        JavaFXTreePath oldPath = getCurrentPath();
        super.scan(tree, p);

        if (tree != null && (tree.getJavaFXKind() != Tree.JavaFXKind.STRING_LITERAL || !(tree.toString().equals("\"\"") || tree.toString().equals("")))) {
            int start = (int)positions.getStartPosition(getCC().getCompilationUnit(), tree);
            int end = (int)positions.getEndPosition(getCC().getCompilationUnit(), tree);
            if (tree.getJavaFXKind() != Tree.JavaFXKind.MODIFIERS && start != -1 && start != end) {
                // check for javafx$run$ magic
                if (!(tree.getJavaFXKind() == Tree.JavaFXKind.FUNCTION_DEFINITION && ((JFXFunctionDefinition)tree).getName().contentEquals("javafx$run$"))) {
                    JavaFXTreePath path = (tree != null && oldPath != null) ? JavafxcTrees.getPath(oldPath, tree) : null;
                    Element scannedElement = path != null ? getCC().getTrees().getElement(path) : null;
                    if (scannedElement == null && (tree.getJavaFXKind() == Tree.JavaFXKind.MEMBER_SELECT || tree.getJavaFXKind() == Tree.JavaFXKind.IDENTIFIER)) {
                        scannedElement = getCC().getElementUtilities().getPackageElement(tree.toString());
                        if (ElementHandle.create(origElement).equals(ElementHandle.create(scannedElement))) {
                            p.add(new ElementLocation(scannedElement, (int)getCC().getTrees().getSourcePositions().getStartPosition(getCC().getCompilationUnit(), tree), getCC()));
                        }
                    } else if (origElement == scannedElement && isChecked(path, scannedElement)) {
                        p.add(ElementLocation.forPath(path, getCC()));
                    }
                }
            }
        }

        return null;
    }

    final protected ElementLocation getElementLocation() {
        return location;
    }

    final protected Element getElement() {
        return origElement;
    }

    final protected ElementKind getElementKind() {
        return origElement != null ? origElement.getKind() : ElementKind.OTHER;
    }

    final protected CompilationController getCC() {
        return cc;
    }

    protected boolean isChecked(JavaFXTreePath path, Element element) {return true;}
}
