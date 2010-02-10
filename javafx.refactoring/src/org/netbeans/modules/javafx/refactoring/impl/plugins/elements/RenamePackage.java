/*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 *  Copyright 1997-2010 Sun Microsystems, Inc. All rights reserved.
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

package org.netbeans.modules.javafx.refactoring.impl.plugins.elements;

import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.UnitTree;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.javafx.refactoring.impl.plugins.BaseRefactoringElementImplementation;
import org.netbeans.modules.javafx.refactoring.transformations.InsertTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.RemoveTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.ReplaceTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.Transformation;
import org.netbeans.modules.refactoring.api.RefactoringSession;
import org.openide.filesystems.FileObject;

public class RenamePackage extends BaseRefactoringElementImplementation {

    private Map<String, String> renameMap;

    public RenamePackage(FileObject srcFO, Map<String, String> renameMap, RefactoringSession session) {
        super(srcFO, session);
        this.renameMap = renameMap;
    }

    @Override
    protected Set<Transformation> prepareTransformations(final CompilationController cc) {
        final Set<Transformation> transformations = new HashSet<Transformation>();

        JavaFXTreePathScanner<Void, Void> scanner = new JavaFXTreePathScanner<Void, Void>() {

            @Override
            public Void visitCompilationUnit(UnitTree node, Void p) {
                ExpressionTree packageNameTree = node.getPackageName();
                String packageName = packageNameTree != null ? packageNameTree.toString() : ""; // NOI18N
                String targetPkg = renameMap.get(packageName);
                if (targetPkg != null) {
                    if (targetPkg.equals(packageName)) {
                        return null; // the same package
                    }
                    if (targetPkg.length() > 0) { // non-default target package
                        if (packageName.length() > 0) { // from non-default package
                            int pos = (int) cc.getTrees().getSourcePositions().getStartPosition(node, node.getPackageName());
                            transformations.add(new ReplaceTextTransformation(pos, packageName, targetPkg));
                        } else {
                            moveFromDefault(targetPkg);
                        }
                    } else { // default target package
                        moveToDefault(packageNameTree);
                    }
                }
                return super.visitCompilationUnit(node, p);
            }

            private void moveToDefault(ExpressionTree packageNameTree) {
                // default package
                TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
                // set position to the first character of the package name
                int startPos = (int) cc.getTrees().getSourcePositions().getStartPosition(cc.getCompilationUnit(), packageNameTree);
                ts.move(startPos);
                // search the "package" keyword (backwards)
                ts.movePrevious();
                Token<JFXTokenId> t = ts.token();
                while (t.id() != JFXTokenId.PACKAGE && ts.movePrevious()) {
                    t = ts.token();
                    startPos -= t.length();
                }
                if (t.id() == JFXTokenId.PACKAGE && ts.movePrevious()) {
                    t = ts.token();
                    while (t.id() == JFXTokenId.WS && ts.movePrevious()) {
                        startPos -= t.length();
                        t = ts.token();
                    }
                    // set position to the last character of the package name
                    int endPos = (int) cc.getTrees().getSourcePositions().getEndPosition(cc.getCompilationUnit(), packageNameTree);
                    ts.move(endPos);
                    ts.moveNext();
                    t = ts.token();
                    while (t.id() != JFXTokenId.SEMI && ts.moveNext()) {
                        endPos += t.length();
                        t = ts.token();
                    }
                    transformations.add(new RemoveTextTransformation(startPos, endPos - startPos + 1));
                }
            }

            private void moveFromDefault(String targetPkg) {
                TokenSequence<JFXTokenId> ts = cc.getTokenHierarchy().tokenSequence();
                // skip comments; possibly license and other headers
                int startPos = 0;
                ts.moveStart();
                ts.moveNext();
                Token<JFXTokenId> t = ts.token();
                while (t.id() == JFXTokenId.COMMENT && ts.moveNext()) {
                    startPos += t.length();
                    t = ts.token();
                }
                boolean appendCRLF = false;
                if (ts.moveNext()) {
                    t = ts.token();
                    appendCRLF = !t.text().toString().equals("\n"); // NOI18N
                }
                transformations.add(new InsertTextTransformation(startPos, "package " + targetPkg + ";" + (appendCRLF ? "\n" : ""))); // NOI18N
            }
        };
        scanner.scan(cc.getCompilationUnit(), null);
        return transformations;
    }

    public String getDisplayText() {
        return "Rename package";
    }
}
