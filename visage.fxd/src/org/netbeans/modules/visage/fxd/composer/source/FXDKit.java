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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
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

package org.netbeans.modules.visage.fxd.composer.source;

import org.netbeans.modules.visage.fxd.composer.editor.FXDKeyTypedAction;
import java.util.Map;
import javax.swing.Action;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.TextAction;
import org.netbeans.editor.ActionFactory.FormatAction;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.BaseKit;
import org.netbeans.modules.editor.NbEditorKit;
import org.netbeans.modules.visage.fxd.composer.editor.BracketCompletion;
import org.netbeans.modules.visage.fxd.composer.editor.FXDInsertBreakAction;

/**
 *
 * @author Pavel Benes
 */
public class FXDKit extends NbEditorKit implements org.openide.util.HelpCtx.Provider {

    /** Serial Version UID */
    private static final long serialVersionUID =2L;
    
    /** Default FXD Mime Type. */
    public static final String MIME_TYPE = "text/x-fxd"; // NOI18N

    
    // hack to be settings browseable //??? more info needed
    public static Map settings;
    
    
    public org.openide.util.HelpCtx getHelpCtx() {
        return new org.openide.util.HelpCtx(FXDKit.class);
    }
    
    // hack to be settings browseable //??? more info needed    
    public static void setMap(Map map) {
        settings = map;
    }

    // hack to be settings browseable //??? more info needed        
    public Map getMap() {
        return settings;
    }

    //??? +xml handling
    public @Override String getContentType() {
        return MIME_TYPE;
    }

    /**
     * Provide XML related actions.
     */
    protected @Override Action[] createActions() {
        Action[] actions = new Action[]{
            new CommentAction("//"), //NOI18N
            new UncommentAction("//"), //NOI18N
            new FXDFormatAction(),
            new FXDKeyTypedAction(),
            new FXDDeleteCharAction(deletePrevCharAction, false),
            new FXDInsertBreakAction()
        };
        return TextAction.augmentList(super.createActions(), actions);
    }
    
    private static class FXDDeleteCharAction extends ExtDeleteCharAction {

        public FXDDeleteCharAction(String nm, boolean nextChar) {
            super(nm, nextChar);
        }

        @Override
        protected void charBackspaced(BaseDocument doc, int dotPos, Caret caret, char ch)
                throws BadLocationException {
            BracketCompletion.charBackspaced(doc, dotPos, ch);
        }
    }

    private static class FXDFormatAction extends FormatAction {
        public FXDFormatAction() {
            putValue(Action.NAME, BaseKit.formatAction);
            setEnabled(false);
        }
    }

}
