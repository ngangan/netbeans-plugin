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
    private String oldText;

    public RemoveTextTransformation(int pos, int len) {
        this.pos = pos;
        this.len = len;
    }

    @Override
    public void perform(Transformer t) {
        oldText = removeText(pos, len, t);
    }

    @Override
    public void revert(Transformer t) {
        if (oldText != null) {
            insertText(pos, oldText, t);
            oldText = null;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final RemoveTextTransformation other = (RemoveTextTransformation) obj;
        if (this.pos != other.pos) {
            return false;
        }
        if (this.len != other.len) {
            return false;
        }
        if ((this.oldText == null) ? (other.oldText != null) : !this.oldText.equals(other.oldText)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 5;
        hash = 59 * hash + this.pos;
        hash = 59 * hash + this.len;
        hash = 59 * hash + (this.oldText != null ? this.oldText.hashCode() : 0);
        return hash;
    }

    
}
