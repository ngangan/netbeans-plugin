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
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.UnitTree;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.PackageElement;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.modules.javafx.refactoring.impl.ElementLocation;
import org.netbeans.modules.refactoring.spi.ProgressProvider;

/**
 *
 * @author Jaroslav Bachorik
 */
public class RenamePackageScanner extends BaseRefactoringScanner {
    final private static String TYPE_MATCH_PATTERN = "(\\..+)*(\\[\\])*";

    final private ProgressProvider pp;
    final private CompilationController cc;

    public RenamePackageScanner(ElementLocation location, ProgressProvider pp, CompilationController cc) {
        super(location, cc);
        this.cc = cc;
        this.pp = pp;
    }

    public RenamePackageScanner(String pkgName, ProgressProvider pp, CompilationController cc) {
        this(inferLocation(pkgName, cc), pp, cc);
    }

    private static ElementLocation inferLocation(final String pkgName, final CompilationController cc) {
        final ElementLocation[] location = new ElementLocation[1];
        new JavaFXTreePathScanner<Void, Void>() {
            private boolean guessed = false;
            @Override
            public Void scan(Tree tree, Void p) {
                super.scan(tree, p);
                if (location[0] == null || guessed) {
                    if (tree != null) {
                        checkLocation(tree);
                    }
                }
                return null;
            }

            private void checkLocation(Tree tree) {
                Element e = null;
                JavaFXTreePath path = getCurrentPath() != null ? JavaFXTreePath.getPath(getCurrentPath(), tree) : null;
                if (path != null) {
                    e = cc.getTrees().getElement(path);
                }
                if (e == null && (tree.getJavaFXKind() == Tree.JavaFXKind.MEMBER_SELECT || tree.getJavaFXKind() == Tree.JavaFXKind.IDENTIFIER)) {
                    e = cc.getElementUtilities().getPackageElement(tree.toString());
                    if (((PackageElement)e).getQualifiedName().contentEquals(pkgName)) {
                        location[0] = new ElementLocation(e, (int)cc.getTrees().getSourcePositions().getStartPosition(cc.getCompilationUnit(), tree),cc);
                        guessed = true;
                    }
                } else if (e != null && e.getKind() == ElementKind.PACKAGE) {
                    if (((PackageElement) e).getQualifiedName().contentEquals(pkgName)) {
                        location[0] = ElementLocation.forPath(path, cc);
                    }
                }
            }
        }.scan(cc.getCompilationUnit(), null);
        return location[0];
    }
//
//    @Override
//    public Void visitCompilationUnit(UnitTree node, Set<ElementLocation> p) {
//        ExpressionTree packageNameTree = node.getPackageName();
//        String packageName = packageNameTree != null ? packageNameTree.toString() : "";
//        if (packageName.equals(origQualName)) {
//            JavaFXTreePath packageNameTp = cc.getTrees().getPath(node, node.getPackageName());
//            p.add(ElementLocation.locationFor(packageNameTp, cc));
//        }
//        return super.visitCompilationUnit(node, p);
//    }
//
//    @Override
//    public Void visitMemberSelect(MemberSelectTree node, Set<ElementLocation> p) {
//        if (Pattern.matches(origQualName + TYPE_MATCH_PATTERN, node.getExpression().toString())) {
//            p.add(ElementLocation.locationFor(JavafxcTrees.getPath(getCurrentPath(), node.getExpression()), cc));
//            return null;
//        }
//        return super.visitMemberSelect(node, p);
//    }
}
