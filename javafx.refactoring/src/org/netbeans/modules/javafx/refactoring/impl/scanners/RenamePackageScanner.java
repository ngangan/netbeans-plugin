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

import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.MemberSelectTree;
import com.sun.javafx.api.tree.UnitTree;
import com.sun.tools.javafx.api.JavafxcTrees;
import java.util.Set;
import java.util.regex.Pattern;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.TreePathHandle;
import org.netbeans.modules.refactoring.spi.ProgressProvider;

/**
 *
 * @author Jaroslav Bachorik
 */
public class RenamePackageScanner extends BaseRefactoringScanner<Void, Set<TreePathHandle>> {
    final private static String TYPE_MATCH_PATTERN = "(\\..+)*(\\[\\])*";
    
    final private String origQualName;
    final private ProgressProvider pp;

    public RenamePackageScanner(String origName, ProgressProvider pp, CompilationController cc) {
        super(null, cc);
        this.origQualName = origName;
        this.pp = pp;
    }

    @Override
    public Void visitCompilationUnit(UnitTree node, Set<TreePathHandle> p) {
        ExpressionTree packageNameTree = node.getPackageName();
        String packageName = packageNameTree != null ? packageNameTree.toString() : "";
        if (packageName.equals(origQualName)) {
            JavaFXTreePath packageNameTp = getCompilationController().getTrees().getPath(node, node.getPackageName());
            p.add(TreePathHandle.create(packageNameTp, getCompilationController()));
        }
        return super.visitCompilationUnit(node, p);
    }

    @Override
    public Void visitMemberSelect(MemberSelectTree node, Set<TreePathHandle> p) {
        if (Pattern.matches(origQualName + TYPE_MATCH_PATTERN, node.getExpression().toString())) {
            p.add(TreePathHandle.create(JavafxcTrees.getPath(getCurrentPath(), node.getExpression()), getCompilationController()));
        }
        return super.visitMemberSelect(node, p);
    }
}
