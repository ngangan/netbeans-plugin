/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.transformations;

import javax.swing.SwingUtilities;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import org.netbeans.editor.GuardedDocument;

/**
 *
 * @author Jaroslav Bachorik <jaroslav.bachorik@sun.com>
 */
public class DocumentTransformer extends Transformer {
    final private AbstractDocument doc;

    public DocumentTransformer(AbstractDocument doc) {
        super(getText(doc));
        this.doc = doc;
    }
    
    @Override
    public Transformer newClone() {
        return new DocumentTransformer(doc);
    }

    @Override
    public synchronized void transform() {
        final String modified = preview();
        Runnable r = new Runnable() {
            public void run() {
                try {
                    doc.remove(0, doc.getLength());
                    doc.insertString(0, modified, null);
                } catch (BadLocationException badLocationException) {
                }
            }
        };
        if (doc instanceof GuardedDocument) {
            ((GuardedDocument)doc).runAtomic(r);
        } else {
            SwingUtilities.invokeLater(r);
        }
    }

    private static String getText(AbstractDocument doc) {
        try {
            return doc.getText(0, doc.getLength());
        } catch (BadLocationException badLocationException) {
            return "";
        }
    }
}
