/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
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
package org.netbeans.modules.visage.editor.format;

import java.util.EnumSet;
import org.netbeans.api.visage.lexer.VisageTokenId;
import org.visage.api.tree.BlockExpressionTree;
import org.visage.api.tree.ExpressionTree;
import org.visage.api.tree.ModifiersTree;
import org.visage.api.tree.Tree;
import org.visage.api.tree.Tree.VisageKind;
import org.visage.api.tree.VisageTreePath;

/**
 * Reformatter utilities.
 *
 * @author Anton Chechel
 */
public final class ReformatUtils {

    public static EnumSet<VisageTokenId> RESERVED_KEYWORDS = EnumSet.of(VisageTokenId.ABSTRACT,
            VisageTokenId.AFTER, VisageTokenId.AND, VisageTokenId.AS, VisageTokenId.ASSERT,
            VisageTokenId.AT, VisageTokenId.ATTRIBUTE, VisageTokenId.BEFORE, VisageTokenId.BIND,
            VisageTokenId.BOUND, VisageTokenId.BREAK, VisageTokenId.CATCH, VisageTokenId.CLASS,
            VisageTokenId.CONTINUE, VisageTokenId.DEF, VisageTokenId.DELETE, VisageTokenId.ELSE,
            VisageTokenId.EXCLUSIVE, VisageTokenId.EXTENDS, VisageTokenId.FALSE, VisageTokenId.FINALLY,
            VisageTokenId.FOR, VisageTokenId.FROM, VisageTokenId.FUNCTION, VisageTokenId.IF,
            VisageTokenId.IMPORT, VisageTokenId.INDEXOF, VisageTokenId.INSERT, VisageTokenId.INSTANCEOF,
            VisageTokenId.LAZY, VisageTokenId.MIXIN, VisageTokenId.MOD, VisageTokenId.NATIVEARRAY,
            VisageTokenId.NEW, VisageTokenId.NOT, VisageTokenId.NULL, VisageTokenId.OR,
            VisageTokenId.OVERRIDE, VisageTokenId.PACKAGE, VisageTokenId.PRIVATE, VisageTokenId.PROTECTED,
            VisageTokenId.PUBLIC, VisageTokenId.PUBLIC_INIT, VisageTokenId.PUBLIC_READ, VisageTokenId.RETURN,
            VisageTokenId.REVERSE, VisageTokenId.SIZEOF, VisageTokenId.STATIC, VisageTokenId.SUPER,
            VisageTokenId.THEN, VisageTokenId.THIS, VisageTokenId.THROW, VisageTokenId.TRUE,
            VisageTokenId.TRY, VisageTokenId.TYPEOF, VisageTokenId.VAR, VisageTokenId.WHILE);

    public static EnumSet<VisageTokenId> NON_RESERVED_KEYWORDS = EnumSet.of(VisageTokenId.FIRST,
            VisageTokenId.IN, VisageTokenId.INIT, VisageTokenId.INTO, VisageTokenId.INVERSE,
            VisageTokenId.LAST, VisageTokenId.ON, VisageTokenId.POSTINIT, VisageTokenId.REPLACE,
            VisageTokenId.STEP, VisageTokenId.TRIGGER, VisageTokenId.TWEEN, VisageTokenId.WHERE,
            VisageTokenId.WITH, VisageTokenId.INVALIDC);

    public static EnumSet<VisageTokenId> MODIFIER_KEYWORDS = EnumSet.of(VisageTokenId.PRIVATE,
            VisageTokenId.PACKAGE, VisageTokenId.PROTECTED, VisageTokenId.PUBLIC,
            VisageTokenId.PUBLIC_READ, VisageTokenId.PUBLIC_INIT, VisageTokenId.STATIC,
            VisageTokenId.ABSTRACT, VisageTokenId.NATIVEARRAY, VisageTokenId.MIXIN,
            VisageTokenId.OVERRIDE, VisageTokenId.BOUND);

    public static EnumSet<VisageTokenId> NON_STRING_LITERALS = EnumSet.of(VisageTokenId.TRUE,
            VisageTokenId.FALSE, VisageTokenId.NULL, VisageTokenId.DECIMAL_LITERAL,
            VisageTokenId.FLOATING_POINT_LITERAL, VisageTokenId.HEX_LITERAL,
            VisageTokenId.OCTAL_LITERAL);

    public static EnumSet<VisageTokenId> STRING_LITERALS = EnumSet.of(VisageTokenId.STRING_LITERAL,
            VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, VisageTokenId.RBRACE_LBRACE_STRING_LITERAL,
            VisageTokenId.RBRACE_QUOTE_STRING_LITERAL);

    public static EnumSet<VisageTokenId> VARIABLE_KEYWORDS = EnumSet.of(VisageTokenId.VAR,
            VisageTokenId.DEF, VisageTokenId.ATTRIBUTE);

    public static EnumSet<VisageTokenId> OPERATOR_KEYWORDS = EnumSet.of(VisageTokenId.AND,
            VisageTokenId.OR, VisageTokenId.MOD);

    public static boolean containsOneExpressionOnly(ExpressionTree tree) {
        if (tree == null) {
            return true;
        }

        if (tree.getVisageKind() == VisageKind.BLOCK_EXPRESSION) {
            BlockExpressionTree bet = (BlockExpressionTree) tree;
//                boolean hasValue = bet.getValue() != null;
            return bet.getStatements().size() == 1;
        }
        return true;
    }

    public static boolean isTreeInsideVar(VisageTreePath currentPath) {
        if (currentPath == null) {
            return false;
        }

        boolean insideVar = false;
        VisageTreePath parentPath = currentPath.getParentPath();
        Tree leaf = parentPath.getLeaf();
        while (leaf.getVisageKind() != VisageKind.COMPILATION_UNIT) {
            if (leaf.getVisageKind() == VisageKind.VARIABLE || leaf.getVisageKind() == VisageKind.SEQUENCE_EXPLICIT) {
                insideVar = true;
                break;
            }
            parentPath = parentPath.getParentPath();
            leaf = parentPath.getLeaf();
        }
        return insideVar;
    }

    // TODO check flags when it will work
    public static boolean hasModifiers(ModifiersTree mods, VisageTokenId firstToken) {
        if (!MODIFIER_KEYWORDS.contains(firstToken)) { //#179502
            return false;
        }
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
