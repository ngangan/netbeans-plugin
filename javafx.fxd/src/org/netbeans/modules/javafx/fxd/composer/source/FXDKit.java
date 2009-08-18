/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.source;

import org.netbeans.modules.javafx.fxd.composer.editor.FXDKeyTypedAction;
import java.util.Map;
import javax.swing.Action;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.TextAction;
import org.netbeans.editor.ActionFactory.FormatAction;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.BaseKit;
import org.netbeans.modules.editor.NbEditorKit;
import org.netbeans.modules.javafx.fxd.composer.editor.BracketCompletion;
import org.netbeans.modules.javafx.fxd.composer.editor.FXDInsertBreakAction;

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
