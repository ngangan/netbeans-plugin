/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
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
package org.netbeans.modules.javafx.editor.format;

import com.sun.javafx.api.tree.*;
import com.sun.javafx.api.tree.Tree.JavaFXKind;
import java.util.EnumSet;
import org.netbeans.api.javafx.lexer.JFXTokenId;

/**
 * Reformatter utilities.
 *
 * @author Anton Chechel
 */
public final class ReformatUtils {

    public static EnumSet<JFXTokenId> NON_RESERVER_KEYWORDS = EnumSet.of(JFXTokenId.FIRST,
            JFXTokenId.IN, JFXTokenId.INIT, JFXTokenId.INTO, JFXTokenId.INVERSE,
            JFXTokenId.LAST, JFXTokenId.ON, JFXTokenId.POSTINIT, JFXTokenId.REPLACE,
            JFXTokenId.STEP, JFXTokenId.TRIGGER, JFXTokenId.TWEEN, JFXTokenId.WHERE,
            JFXTokenId.WITH, JFXTokenId.INVALIDC);

    public static EnumSet<JFXTokenId> MODIFIER_KEYWORDS = EnumSet.of(JFXTokenId.PRIVATE,
            JFXTokenId.PACKAGE, JFXTokenId.PROTECTED, JFXTokenId.PUBLIC,
            JFXTokenId.PUBLIC_READ, JFXTokenId.PUBLIC_INIT, JFXTokenId.STATIC,
            JFXTokenId.ABSTRACT, JFXTokenId.NATIVEARRAY, JFXTokenId.MIXIN,
            JFXTokenId.OVERRIDE);

    public static boolean containsOneExpressionOnly(ExpressionTree tree) {
        if (tree == null) {
            return true;
        }

        if (tree.getJavaFXKind() == JavaFXKind.BLOCK_EXPRESSION) {
            BlockExpressionTree bet = (BlockExpressionTree) tree;
//                boolean hasValue = bet.getValue() != null;
            return bet.getStatements().size() == 1;
        }
        return true;
    }

    public static boolean isTreeInsideVar(JavaFXTreePath currentPath) {
        if (currentPath == null) {
            return false;
        }

        boolean insideVar = false;
        JavaFXTreePath parentPath = currentPath.getParentPath();
        Tree leaf = parentPath.getLeaf();
        while (leaf.getJavaFXKind() != JavaFXKind.COMPILATION_UNIT) {
            if (leaf.getJavaFXKind() == JavaFXKind.VARIABLE || leaf.getJavaFXKind() == JavaFXKind.SEQUENCE_EXPLICIT) {
                insideVar = true;
                break;
            }
            parentPath = parentPath.getParentPath();
            leaf = parentPath.getLeaf();
        }
        return insideVar;
    }

    // TODO check flags when it will work
    // TODO create issue for compiler
    public static boolean hasModifiers(ModifiersTree mods) {
        if (mods == null) {
            return false;
        }
        final String p1 = "synthetic"; // NOI18N
        final String p2 = "script only (default)"; // NOI18N
        final String p3 = "static script only (default)"; // NOI18N
        final String m = mods.toString().trim();
        return m.indexOf(p1) == -1 && !m.contentEquals(p2) && !m.contentEquals(p3);
    }
}
