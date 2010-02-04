/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.transformations;

/**
 *
 * @author Jaroslav Bachorik <jaroslav.bachorik@sun.com>
 */
public class StringTransformer extends Transformer {
    final String backup;
    public StringTransformer(String text) {
        super(text);
        backup = text;
    }

    @Override
    public Transformer newClone() {
        return new StringTransformer(backup);
    }

    @Override
    protected void saveTransformed(String transformed) {
        // just ignore this
    }
}
