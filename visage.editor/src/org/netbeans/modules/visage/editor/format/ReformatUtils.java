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

import com.sun.visage.api.tree.*;
import com.sun.visage.api.tree.Tree.VisageKind;
import java.util.EnumSet;
import org.netbeans.api.visage.lexer.VSGTokenId;

/**
 * Reformatter utilities.
 *
 * @author Anton Chechel
 */
public final class ReformatUtils {

    public static EnumSet<VSGTokenId> RESERVED_KEYWORDS = EnumSet.of(VSGTokenId.ABSTRACT,
            VSGTokenId.AFTER, VSGTokenId.AND, VSGTokenId.AS, VSGTokenId.ASSERT,
            VSGTokenId.AT, VSGTokenId.ATTRIBUTE, VSGTokenId.BEFORE, VSGTokenId.BIND,
            VSGTokenId.BOUND, VSGTokenId.BREAK, VSGTokenId.CATCH, VSGTokenId.CLASS,
            VSGTokenId.CONTINUE, VSGTokenId.DEF, VSGTokenId.DELETE, VSGTokenId.ELSE,
            VSGTokenId.EXCLUSIVE, VSGTokenId.EXTENDS, VSGTokenId.FALSE, VSGTokenId.FINALLY,
            VSGTokenId.FOR, VSGTokenId.FROM, VSGTokenId.FUNCTION, VSGTokenId.IF,
            VSGTokenId.IMPORT, VSGTokenId.INDEXOF, VSGTokenId.INSERT, VSGTokenId.INSTANCEOF,
            VSGTokenId.LAZY, VSGTokenId.MIXIN, VSGTokenId.MOD, VSGTokenId.NATIVEARRAY,
            VSGTokenId.NEW, VSGTokenId.NOT, VSGTokenId.NULL, VSGTokenId.OR,
            VSGTokenId.OVERRIDE, VSGTokenId.PACKAGE, VSGTokenId.PRIVATE, VSGTokenId.PROTECTED,
            VSGTokenId.PUBLIC, VSGTokenId.PUBLIC_INIT, VSGTokenId.PUBLIC_READ, VSGTokenId.RETURN,
            VSGTokenId.REVERSE, VSGTokenId.SIZEOF, VSGTokenId.STATIC, VSGTokenId.SUPER,
            VSGTokenId.THEN, VSGTokenId.THIS, VSGTokenId.THROW, VSGTokenId.TRUE,
            VSGTokenId.TRY, VSGTokenId.TYPEOF, VSGTokenId.VAR, VSGTokenId.WHILE);

    public static EnumSet<VSGTokenId> NON_RESERVED_KEYWORDS = EnumSet.of(VSGTokenId.FIRST,
            VSGTokenId.IN, VSGTokenId.INIT, VSGTokenId.INTO, VSGTokenId.INVERSE,
            VSGTokenId.LAST, VSGTokenId.ON, VSGTokenId.POSTINIT, VSGTokenId.REPLACE,
            VSGTokenId.STEP, VSGTokenId.TRIGGER, VSGTokenId.TWEEN, VSGTokenId.WHERE,
            VSGTokenId.WITH, VSGTokenId.INVALIDC);

    public static EnumSet<VSGTokenId> MODIFIER_KEYWORDS = EnumSet.of(VSGTokenId.PRIVATE,
            VSGTokenId.PACKAGE, VSGTokenId.PROTECTED, VSGTokenId.PUBLIC,
            VSGTokenId.PUBLIC_READ, VSGTokenId.PUBLIC_INIT, VSGTokenId.STATIC,
            VSGTokenId.ABSTRACT, VSGTokenId.NATIVEARRAY, VSGTokenId.MIXIN,
            VSGTokenId.OVERRIDE, VSGTokenId.BOUND);

    public static EnumSet<VSGTokenId> NON_STRING_LITERALS = EnumSet.of(VSGTokenId.TRUE,
            VSGTokenId.FALSE, VSGTokenId.NULL, VSGTokenId.DECIMAL_LITERAL,
            VSGTokenId.FLOATING_POINT_LITERAL, VSGTokenId.HEX_LITERAL,
            VSGTokenId.OCTAL_LITERAL);

    public static EnumSet<VSGTokenId> STRING_LITERALS = EnumSet.of(VSGTokenId.STRING_LITERAL,
            VSGTokenId.QUOTE_LBRACE_STRING_LITERAL, VSGTokenId.RBRACE_LBRACE_STRING_LITERAL,
            VSGTokenId.RBRACE_QUOTE_STRING_LITERAL);

    public static EnumSet<VSGTokenId> VARIABLE_KEYWORDS = EnumSet.of(VSGTokenId.VAR,
            VSGTokenId.DEF, VSGTokenId.ATTRIBUTE);

    public static EnumSet<VSGTokenId> OPERATOR_KEYWORDS = EnumSet.of(VSGTokenId.AND,
            VSGTokenId.OR, VSGTokenId.MOD);

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
    public static boolean hasModifiers(ModifiersTree mods, VSGTokenId firstToken) {
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
