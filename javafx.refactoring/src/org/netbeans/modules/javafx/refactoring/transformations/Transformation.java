/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.transformations;

import java.util.Comparator;

/**
 *
 * @author Jaroslav Bachorik <jaroslav.bachorik@sun.com>
 */
abstract public class Transformation {
    private int pos;

    public Transformation(int pos) {
        this.pos = pos;
    }

    final protected void insertText(int pos, String text, Transformer t) {
        t.insertText(pos, text);
    }

    final protected String removeText(int pos, int len, Transformer t) {
        return t.removeText(pos, len);
    }

    final protected void replaceText(int pos, String oldText, String newText, Transformer t) {
        t.replaceText(pos, oldText, newText);
    }

    final protected int getPosition() {
        return pos;
    }

    abstract public void perform(Transformer t);
    abstract protected int priority();

    final static Comparator<Transformation> COMPARATOR = new Comparator<Transformation>() {

        public int compare(Transformation o1, Transformation o2) {
            if (o1 == null && o2 == null) return 0;
            if (o1 != null && o2 == null) return 1;
            if (o1 == null && o2 != null) return -1;

            if (o1.priority() == o2.priority()) {
                if (o1.getPosition() == o2.getPosition()) return 0;
                if (o1.getPosition() > o2.getPosition()) return 1;
                return -1;
            }
            if (o1.priority() > o2.priority()) return 1;
            return -1;
        }
    };
}
