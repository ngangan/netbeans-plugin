/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2008-2010 Oracle and/or its affiliates. All rights reserved.
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
 * 
 * Contributor(s):
 * 
 * Portions Copyrighted 2008-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.visage.editor.completion.environment;

import com.sun.tools.mjavac.code.Type;

import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.api.visage.lexer.VisageTokenId;
import org.netbeans.modules.visage.editor.completion.VisageCompletionEnvironment;
import static org.netbeans.modules.visage.editor.completion.VisageCompletionQuery.LAZY_KEYWORD;

import javax.lang.model.type.TypeMirror;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.TypeElement;
import org.visage.api.tree.ClassDeclarationTree;
import org.visage.api.tree.ExpressionTree;
import org.visage.api.tree.OverrideClassVarTree;
import org.visage.api.tree.Tree;
import org.visage.api.tree.VariableTree;
import org.visage.api.tree.VisageTreePath;
import org.visage.tools.code.VisageTypes;
import org.visage.tools.tree.VisageErroneousType;
import org.visage.tools.tree.VisageSelect;

/**
 * @author David Strupl
 */
public class VariableTreeEnvironment extends VisageCompletionEnvironment<VariableTree> {

    private static final Logger logger = Logger.getLogger(VariableTreeEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    @Override
    protected void inside(VariableTree t) throws IOException {
        if (LOGGABLE) log("inside VariableTree " + t + "  offset == " + offset); // NOI18N
        boolean isLocal = path.getParentPath().getLeaf().getVisageKind() != Tree.VisageKind.CLASS_DECLARATION;
        boolean isOverride = t instanceof OverrideClassVarTree;

        if (isOverride & !isLocal) {
            ClassDeclarationTree cdt = (ClassDeclarationTree)path.getParentPath().getLeaf();
            System.err.println("cdt=" + cdt);
            TypeMirror type = controller.getTrees().getTypeMirror(path.getParentPath());
            TypeElement te = (TypeElement) ((Type.ClassType)type).asElement();
            TypeMirror parent = te.getSuperclass();
            addMembers(parent, false, true);
        }
        Tree type = t.getType();
        int typePos;
        // for overiden expressions, getType returns null tree, because
        // you can't override the type of the variable.
        if (type == null) {
            typePos = 0;
        } else {
            typePos = type.getVisageKind() == Tree.VisageKind.ERRONEOUS && ((VisageErroneousType) type).getErrorTrees().isEmpty() ? (int) sourcePositions.getEndPosition(root, type) : (int) sourcePositions.getStartPosition(root, type);
        }
        if (LOGGABLE) log("  isLocal == " + isLocal + "  type == " + type + "  typePos == " + typePos); // NOI18N
        if (offset <= typePos) {
            TokenSequence<VisageTokenId> last = findLastNonWhitespaceToken((int) sourcePositions.getStartPosition(root, t), offset);
            if (LOGGABLE) log("    last(1) == " + (last == null ? "null" : last.token().id())); // NOI18N
            if ((last != null) && (last.token().id() == VisageTokenId.COLON)){
                addLocalAndImportedTypes(null, null, null, false, getSmartType(t));
                addBasicTypes();
            }
            return;
        }
        TokenSequence<VisageTokenId> last = findLastNonWhitespaceToken((int) sourcePositions.getEndPosition(root, type), offset);
        if (LOGGABLE) log("    last(2) == " + (last == null ? "null" : last.token().id())); // NOI18N
        if ((last != null) && (last.token().id() == VisageTokenId.EQ ||
                last.token().id() == VisageTokenId.BIND  ||
                last.token().id() == VisageTokenId.LAZY)) {
            localResult(getSmartType(t));
            addValueKeywords();
            if (last.token().id() == VisageTokenId.BIND) {
                addKeyword(LAZY_KEYWORD, null, false);
            }
        }
        ExpressionTree initializer = t.getInitializer();
        if (initializer instanceof VisageSelect) {
            String typeS = ((VisageSelect) initializer).getExpression().toString();
            addAllTypes(null, false, typeS);
        }
        addLocalMembersAndVars(getSmartType(t));
        addValueKeywords();
        addLocalAndImportedTypes(null, null, null, false, null);
    }

    private TypeMirror getSmartType(VariableTree t) throws IOException {
        if (t.getInitializer() == null) {
            if (LOGGABLE) log("  getSmartType no initializer"); // NOI18N
            return null;
        }
        final VisageTreePath treePath = new VisageTreePath(path, t.getInitializer());
        TypeMirror type = controller.getTrees().getTypeMirror(treePath);
        if (LOGGABLE) log("getSmartType path == " + path.getLeaf() + "  type == " + type); // NOI18N
        if (type == null) {
            return null;
        }
        
        // handle sequences as their element type
        VisageTypes types = controller.getVisageTypes();
        if (types.isSequence((Type) type)) {
            type = types.elementType((Type) type);
        } 
        if (LOGGABLE) log("getSmartType path == " + path.getLeaf() + "  type(2) == " + type); // NOI18N
        return type;
    }

    private static void log(String s) {
        if (LOGGABLE) {
            logger.fine(s);
        }
    }
}
