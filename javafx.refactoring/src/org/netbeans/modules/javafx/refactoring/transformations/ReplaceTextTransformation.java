/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.transformations;

/**
 *
 * @author Jaroslav Bachorik <jaroslav.bachorik@sun.com>
 */
final public class ReplaceTextTransformation extends Transformation {
    final private int pos;
    final private String oldText, newText;

    public ReplaceTextTransformation(int pos, String oldText, String newText) {
        this.pos = pos;
        this.oldText = oldText;
        this.newText = newText;
    }

    @Override
    public void perform(Transformer t) {
        replaceText(pos, oldText, newText, t);
    }

}
