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

package org.netbeans.modules.visage.editor.completion.environment;

import com.sun.tools.visage.tree.VSGBlock;
import com.sun.tools.visage.tree.VSGFunctionValue;
import com.sun.tools.visage.tree.VSGType;
import org.netbeans.modules.visage.editor.completion.VisageCompletionEnvironment;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author David Strupl
 */
public class FunctionValueEnvironment extends VisageCompletionEnvironment<VSGFunctionValue> {

    private static final Logger logger = Logger.getLogger(FunctionValueEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    @Override
    protected void inside(VSGFunctionValue val) throws IOException {
        if (LOGGABLE) log("inside VSGFunctionValue " + val); // NOI18N
        int startPos = (int) sourcePositions.getStartPosition(root, val);
        VSGType retType = val.getVSGReturnType();
        if (LOGGABLE) log("  offset == " + offset + "  startPos == " + startPos + " retType == " + retType); // NOI18N
        if ((offset < startPos) || (startPos == 0)) {
            if (LOGGABLE) log("  before block: return types"); // NOI18N
            addLocalAndImportedTypes(null, null, null, false, null);
            addBasicTypes();
            return;
        }
        VSGBlock bl = val.getBodyExpression();
        if (bl != null) {
            insideFunctionBlock(bl.getStatements());
        }
    }

    private static void log(String s) {
        if (LOGGABLE) {
            logger.fine(s);
        }
    }
}
