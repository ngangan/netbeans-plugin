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
        this.newText = newText != null ? newText : "";
    }

    @Override
    public void perform(Transformer t) {
        insertText(pos, newText, t);
    }

    @Override
    public void revert(Transformer t) {
        removeText(pos, newText.length(), t);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final InsertTextTransformation other = (InsertTextTransformation) obj;
        if (this.pos != other.pos) {
            return false;
        }
        if ((this.newText == null) ? (other.newText != null) : !this.newText.equals(other.newText)) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 41 * hash + this.pos;
        hash = 41 * hash + (this.newText != null ? this.newText.hashCode() : 0);
        return hash;
    }
}
