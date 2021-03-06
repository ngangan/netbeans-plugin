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

import org.visage.api.tree.SourcePositions;
import org.visage.api.tree.Tree;
import org.visage.api.tree.VisageTreePath;
import org.visage.tools.tree.VisageErroneous;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.netbeans.modules.visage.editor.completion.VisageCompletionEnvironment;
import static org.netbeans.modules.visage.editor.completion.VisageCompletionQuery.*;

/**
 * @author David Strupl
 */
public class ErroneousEnvironment extends VisageCompletionEnvironment<VisageErroneous> {
    
    private static final Logger logger = Logger.getLogger(ErroneousEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    @Override
    protected void inside(VisageErroneous t) {
        if (LOGGABLE) log("inside VisageErroneous " + t); // NOI18N
        SourcePositions pos = controller.getTrees().getSourcePositions();
        long s = pos.getStartPosition(root, t);
        long e = pos.getEndPosition(root, t);
        if (LOGGABLE) log("   s = " + s + "  e == " + e); // NOI18N
        VisageTreePath p = VisageTreePath.getPath(root, t);
        
        if (t.getErrorTrees().isEmpty()) {
            useSanitizedSource();
            return;
        }

        for (Tree tt : t.getErrorTrees()) {
            if (LOGGABLE) log("    tt == " + tt); // NOI18N
            if (LOGGABLE) log("    tt.getClass() == " + tt.getClass()); // NOI18N
            long st = pos.getStartPosition(root, tt);
            long et = pos.getEndPosition(root, tt);
            if (LOGGABLE) log("   st = " + st + "  et == " + et); // NOI18N
            if (et == offset-1) {
                useSanitizedSource();
            }
        }
    }

    private void useSanitizedSource() {
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
