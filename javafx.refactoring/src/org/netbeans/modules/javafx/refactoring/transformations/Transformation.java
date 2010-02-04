/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.transformations;

/**
 *
 * @author Jaroslav Bachorik <jaroslav.bachorik@sun.com>
 */
abstract public class Transformation {
    final protected void insertText(int pos, String text, Transformer t) {
        t.insertText(pos, text);
    }

    final protected String removeText(int pos, int len, Transformer t) {
        return t.removeText(pos, len);
    }

    final protected void replaceText(int pos, String oldText, String newText, Transformer t) {
        t.replaceText(pos, oldText, newText);
    }

    abstract public void perform(Transformer t);
    abstract public void revert(Transformer t);
}
