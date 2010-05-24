/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Oracle and/or its affiliates. All rights reserved.
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
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.editor.completion.environment;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.tools.javafx.tree.JFXFunctionDefinition;
import com.sun.tools.javafx.tree.JFXType;
import org.netbeans.modules.javafx.editor.completion.JavaFXCompletionEnvironment;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.netbeans.api.lexer.TokenUtilities;

/**
 *
 * @author David Strupl
 */
public class FunctionDefinitionEnvironment extends JavaFXCompletionEnvironment<JFXFunctionDefinition> {
    
    private static final Logger logger = Logger.getLogger(FunctionDefinitionEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    @Override
    protected void inside(JFXFunctionDefinition t) throws IOException {
        if (LOGGABLE) log("inside JFXFunctionDefinition " + t); // NOI18N
        JFXFunctionDefinition def = t;
        int startPos = (int) sourcePositions.getStartPosition(root, def);
        JFXType retType = def.getJFXReturnType();
        if (LOGGABLE) log("  offset == " + offset + "  startPos == " + startPos + " retType == " + retType); // NOI18N
        CharSequence headerText = controller.getText().subSequence(startPos, offset > startPos ? offset : startPos);
        if (LOGGABLE) log("  headerText(1) == " + headerText); // NOI18N
        int parStart = TokenUtilities.indexOf(headerText, '('); // NOI18N
        if (LOGGABLE) log("  parStart: " + parStart); // NOI18N
        if (parStart >= 0) {
            int parEnd = TokenUtilities.indexOf(headerText, ')', parStart); // NOI18N
            if (parEnd > parStart) {
                headerText = TokenUtilities.trim(headerText.subSequence(parEnd + 1, headerText.length()));
            } else {
//                for (JFXVar param : def.getParams()) {
//                    int parPos = (int) sourcePositions.getEndPosition(root, param);
//                    if (parPos == Diagnostic.NOPOS || offset <= parPos) {
//                        break;
//                    }
//                    parStart = parPos - startPos;
//                }
                headerText = TokenUtilities.trim(headerText.subSequence(parStart, headerText.length()));
            }
            if (LOGGABLE) log("  headerText(2) ==" + headerText); // NOI18N
            if (":".equals(headerText)) { // NOI18N
                addLocalAndImportedTypes(null, null, null, false, null);
                addBasicTypes();
                return;
            }
        } else if (retType != null && TokenUtilities.trim(headerText).length() == 0) {
            if (LOGGABLE) log("  insideExpression for retType:"); // NOI18N
            insideExpression(new JavaFXTreePath(path, retType));
            return;
        }
        int bodyPos = (int) sourcePositions.getStartPosition(root, def.getBodyExpression());
        if (LOGGABLE) log("  bodyPos: " + bodyPos); // NOI18N
        if ((bodyPos >=0) && (offset > bodyPos)) {
            if (LOGGABLE) log(" we are inside body of the function:"); // NOI18N
            insideFunctionBlock(def.getBodyExpression().getStatements());
        } 
    }

    private static void log(String s) {
        if (LOGGABLE) {
            logger.fine(s);
        }
    }
}
