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
    final private String newText;

    public InsertTextTransformation(int pos, String newText) {
        super(pos);
        this.newText = newText != null ? newText : ""; // NOI18N
    }

    @Override
    public void perform(Transformer t) {
        insertText(getPosition(), newText, t);
    }

    @Override
    protected int priority() {
        return 2;
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
        if (this.getPosition() != other.getPosition()) {
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
        hash = 41 * hash + this.getPosition();
        hash = 41 * hash + (this.newText != null ? this.newText.hashCode() : 0);
        return hash;
    }
}
