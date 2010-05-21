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
import com.sun.javafx.api.tree.ReturnTree;
import com.sun.javafx.api.tree.Tree;
import com.sun.tools.javafx.tree.JFXFunctionDefinition;
import com.sun.tools.javafx.tree.JFXType;
import org.netbeans.modules.javafx.editor.completion.JavaFXCompletionEnvironment;

import javax.lang.model.type.TypeMirror;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author David Strupl
 */
public class ReturnEnvironment extends JavaFXCompletionEnvironment<ReturnTree> {
    
    private static final Logger logger = Logger.getLogger(ReturnEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    @Override
    protected void inside(ReturnTree t) throws IOException {
        if (LOGGABLE) log("inside ReturnTree " + t); // NOI18N
        localResult(getSmartType(t));
        addValueKeywords();
    }

    private static void log(String s) {
        if (LOGGABLE) {
            logger.fine(s);
        }
    }

    private TypeMirror getSmartType(ReturnTree ui) {
        JFXType r = null;
        for (JavaFXTreePath tp = path; tp != null; tp = tp.getParentPath()) {
            Tree t = tp.getLeaf();
            if (LOGGABLE) log("  tree kind: " + t.getJavaFXKind()); // NOI18N
            if (t instanceof JFXFunctionDefinition) {
                JFXFunctionDefinition fDefTree = (JFXFunctionDefinition)t;
                r = fDefTree.getJFXReturnType();
                if (LOGGABLE) log("   return type == " + r); // NOI18N
            }
        }
        final JavaFXTreePath treePath = new JavaFXTreePath(path, r == null ? ui.getExpression() : r);
        TypeMirror type = controller.getTrees().getTypeMirror(treePath);
        if (LOGGABLE) log("getSmartType path == " + treePath.getLeaf() + "  type == " + type); // NOI18N
        return type;
    }
}
