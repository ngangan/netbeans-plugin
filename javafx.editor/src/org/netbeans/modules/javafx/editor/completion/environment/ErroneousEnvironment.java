/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 2008-2009 Sun Microsystems, Inc. All rights reserved.
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

package org.netbeans.modules.javafx.editor.completion.environment;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.tools.javafx.tree.JFXErroneous;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.netbeans.modules.javafx.editor.completion.JavaFXCompletionEnvironment;
import static org.netbeans.modules.javafx.editor.completion.JavaFXCompletionQuery.*;

/**
 * @author David Strupl
 */
public class ErroneousEnvironment extends JavaFXCompletionEnvironment<JFXErroneous> {
    
    private static final Logger logger = Logger.getLogger(ErroneousEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    @Override
    protected void inside(JFXErroneous t) {
        if (LOGGABLE) log("inside JFXErroneous " + t); // NOI18N
        SourcePositions pos = controller.getTrees().getSourcePositions();
        long s = pos.getStartPosition(root, t);
        long e = pos.getEndPosition(root, t);
        if (LOGGABLE) log("   s = " + s + "  e == " + e); // NOI18N
        JavaFXTreePath p = JavaFXTreePath.getPath(root, t);
        
        if (t.getErrorTrees().isEmpty()) {
            tryToSanitizeSource();
            return;
        }

        for (Tree tt : t.getErrorTrees()) {
            if (LOGGABLE) log("    tt == " + tt); // NOI18N
            if (LOGGABLE) log("    tt.getClass() == " + tt.getClass()); // NOI18N
            long st = pos.getStartPosition(root, tt);
            long et = pos.getEndPosition(root, tt);
            if (LOGGABLE) log("   st = " + st + "  et == " + et); // NOI18N
            if (et == offset-1) {
                tryToSanitizeSource();
            }
        }
    }

    private void tryToSanitizeSource() {
        try {
            Document d = controller.getDocument();
            String start = d.getText(0, offset);
            if (LOGGABLE) log("  start = " + start); // NOI18N
            String end = d.getText(offset, d.getLength()-offset);
            if (LOGGABLE) log("  end = " + end); // NOI18N
            useSanitizedSource(start+" x"+end, offset); // NOI18N
        } catch (BadLocationException ble) {
            if (LOGGABLE) logger.log(Level.FINER, "ble", ble); // NOI18N
        }
    }

    private static void log(String s) {
        if (LOGGABLE) {
            logger.fine(s);
        }
    }
}
