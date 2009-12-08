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
import com.sun.javafx.api.tree.Tree.JavaFXKind;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.modules.javafx.refactoring.impl.ElementLocation;

/**
 *
 * @author Jaroslav Bachorik
 */
public class FindSubclassesScanner extends BaseRefactoringScanner {

    public FindSubclassesScanner(ElementLocation location, CompilationController cc) {
        super(location, cc);
    }

    @Override
    protected boolean isChecked(JavaFXTreePath path, Element element) {
        return path.getLeaf().getJavaFXKind() == JavaFXKind.CLASS_DECLARATION;
    }


//    @Override
//    public Void visitClassDeclaration(ClassDeclarationTree node, Set<ElementLocation> locations) {
//        switch (getElementKind()) {
//            case CLASS:
//            case INTERFACE: {
//                if (isSameElement()) {
////                TypeElement te = (TypeElement)getCompilationController().getTrees().getElement(getCurrentPath());
////                if (targetName.equals(te.getQualifiedName().toString())) {
//                    locations.add(ElementLocation.locationFor(getCurrentPath(), getCC()));
//                }
//            }
//        }
//        return super.visitClassDeclaration(node, locations);
//    }
}
