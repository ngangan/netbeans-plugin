package org.netbeans.modules.javafx.fxd.composer.editor;

import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.ext.ExtKit.ExtDefaultKeyTypedAction;
import org.netbeans.modules.javafx.fxd.composer.editor.BracketCompletion;

public class FXDKeyTypedAction extends ExtDefaultKeyTypedAction {

    @Override
    protected void insertString(BaseDocument doc, int dotPos, Caret caret, String str, boolean overwrite) throws BadLocationException {
        char insertedChar = str.charAt(0);
        if (insertedChar == '\"' || insertedChar == '\'') {
            boolean inserted = BracketCompletion.completeQuote(doc, dotPos, caret, insertedChar);
            if (inserted) {
                caret.setDot(dotPos + 1);
            } else {
                super.insertString(doc, dotPos, caret, str, overwrite);
            }
        } else {
            super.insertString(doc, dotPos, caret, str, overwrite);
            // we do support pair completion for []
            //do not support pair for {( and moving semicolon
            BracketCompletion.charInserted(doc, dotPos, caret, insertedChar);
        }
    }

    @Override
    protected void replaceSelection(JTextComponent target, int dotPos, Caret caret, String str, boolean overwrite) throws BadLocationException {
        char insertedChar = str.charAt(0);
        Document doc = target.getDocument();
        if (insertedChar == '\"' || insertedChar == '\'') {
            if (doc != null) {
                try {
                    boolean inserted = false;
                    int p0 = Math.min(caret.getDot(), caret.getMark());
                    int p1 = Math.max(caret.getDot(), caret.getMark());
                    if (p0 != p1) {
                        doc.remove(p0, p1 - p0);
                    }
                    int caretPosition = caret.getDot();
                    if (doc instanceof BaseDocument) {
                        inserted = BracketCompletion.completeQuote((BaseDocument) doc, caretPosition, caret, insertedChar);
                    }
                    if (inserted) {
                        caret.setDot(caretPosition + 1);
                    } else {
                        if (str != null && str.length() > 0) {
                            doc.insertString(p0, str, null);
                        }
                    }
                } catch (BadLocationException e) {
                    e.printStackTrace();
                }
            }
        } else {
            super.replaceSelection(target, dotPos, caret, str, overwrite);
            // we do support pair completion for []
            //do not support pair for {( and moving semicolon
            if (doc instanceof BaseDocument) {
                BracketCompletion.charInserted((BaseDocument) doc, caret.getDot() - 1, caret, insertedChar);
            }
        }
    }
}
