/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.transformations;

/**
 *
 * @author Jaroslav Bachorik <jaroslav.bachorik@sun.com>
 */
final public class InsertTextTransformation extends Transformation {
    final private int pos;
    final private String newText;

    public InsertTextTransformation(int pos, String newText) {
        this.pos = pos;
        this.newText = newText;
    }

    @Override
    public void perform(Transformer t) {
        insertText(pos, newText, t);
    }

}
