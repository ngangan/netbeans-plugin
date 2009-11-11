/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.editor.hints;

import org.junit.Test;

/**
 *
 * @author karol
 */
public class HintsAllTest extends  HintsTestBase {

    public HintsAllTest() {
        super("testAll");
    }

    @Test
    public void testOnlyPackageFile() throws Exception {
        testHints("OnlyPackage", "^package abcd;", "topLevelKeywords.pass");
    }
}

