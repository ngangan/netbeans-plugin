/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2008 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 */
package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.CatchTree;
import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.TryTree;
import java.util.ArrayList;
import org.netbeans.modules.javafx.editor.hints.UncaughtExceptionsModel.Hint;
import com.sun.javafx.api.tree.Tree;
import com.sun.tools.javac.code.Type;
import java.util.Collection;
import java.util.HashSet;

/**
 *
 * @author karol harezlak
 */
final class UncaughtExceptionsVisitorResolver extends JavaFXTreePathScanner<Void, UncaughtExceptionsModel> {

    @Override
    public Void visitTry(TryTree node, UncaughtExceptionsModel model) {
        Collection<Hint> hints = new HashSet<Hint>(model.getThrowHints());
        Collection<ExpressionTree> nodes = new ArrayList<ExpressionTree>(node.getBlock().getStatements());
        nodes.add(node.getBlock().getValue());
        for (Tree node_ : nodes) {
            for (Hint hint : hints) {
                if (hint.getTree() == node_) {
                    Collection<Type> hintTypes = new ArrayList(hint.getExceptions());
                    for (Type hintType : hintTypes) {
                        //TODO JavaFXTypeClass does not provide full class name, it should use full class names not simple names
                        for (CatchTree catchType : node.getCatches()) {
                            String hintTypeName = UncaughtExceptionsVisitor.getMethodName(hintType.toString());
                            if (catchType.getParameter() == null) {
                                continue;
                            }
                            String catchTypeName = UncaughtExceptionsVisitor.getMethodName(catchType.getParameter().getType().toString());
                            if (hintTypeName.equals(catchTypeName)) {
                                hint.removeException(hintType);
                            }
                        }
                    }
                    if (hint.getExceptions().size() == 0) {
                        model.removeHint(hint);
                    } else {
                        model.addCatchTree(hint, node);
                    }
                }
            }
        }
        return super.visitTry(node, model);
    }
}
