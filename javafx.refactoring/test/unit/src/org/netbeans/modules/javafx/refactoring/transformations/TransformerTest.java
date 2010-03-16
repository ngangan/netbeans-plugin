/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.transformations;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class TransformerTest {
    final private String text = "import refactoringissues.i180546.M.*;\nimport refactoringissues.i180546.M;\n";

    private class MyTransformer extends Transformer {
        String transformed;
        String backup;

        public MyTransformer(String text) {
            super(text);
            this.backup = text;
        }

        @Override
        public Transformer newClone() {
            return new MyTransformer(backup);
        }

        @Override
        protected void saveTransformed(String transformed) {
            this.transformed = transformed;
        }
    }

    private MyTransformer transformer;

    public TransformerTest() {
    }

    @Before
    public void setUp() {
        transformer = new MyTransformer(text);
    }

    @After
    public void tearDown() {
        transformer = null;
    }

    @Test
    public void testReplaceDelete() throws Exception {
        transformer.addTransformation(new ReplaceTextTransformation(7, "refactoringissues.i180546", "refactoringissues.i180546.a"));
        transformer.addTransformation(new ReplaceTextTransformation(45, "refactoringissues.i180546", "refactoringissues.i180546.a"));
        transformer.addTransformation(new RemoveTextTransformation(0, 38));
        transformer.addTransformation(new RemoveTextTransformation(39, 36));

        transformer.transform();
        assertEquals("", transformer.transformed);
    }

}