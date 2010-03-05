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
    final private String oldText, newText;

    public ReplaceTextTransformation(int pos, String oldText, String newText) {
        super(pos);
        this.oldText = oldText;
        this.newText = newText;
    }

    @Override
    public void perform(Transformer t) {
        replaceText(getPosition(), oldText, newText, t);
    }

    @Override
    protected int priority() {
        return 1;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final ReplaceTextTransformation other = (ReplaceTextTransformation) obj;
        if (this.getPosition() != other.getPosition()) {
            return false;
        }
        if ((this.oldText == null) ? (other.oldText != null) : !this.oldText.equals(other.oldText)) {
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
        hash = 47 * hash + this.getPosition();
        hash = 47 * hash + (this.oldText != null ? this.oldText.hashCode() : 0);
        hash = 47 * hash + (this.newText != null ? this.newText.hashCode() : 0);
        return hash;
    }

    
}
