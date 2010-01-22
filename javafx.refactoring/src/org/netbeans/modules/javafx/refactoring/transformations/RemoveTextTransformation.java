/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.transformations;

/**
 *
 * @author Jaroslav Bachorik <jaroslav.bachorik@sun.com>
 */
final public class RemoveTextTransformation extends Transformation {
    final private int pos, len;

    public RemoveTextTransformation(int pos, int len) {
        this.pos = pos;
        this.len = len;
    }

    @Override
    public void perform(Transformer t) {
        removeText(pos, len, t);
    }

}
