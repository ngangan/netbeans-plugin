/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 *
 * Contributor(s):
 *
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.test.javafx.editor.templates;

/**
 *
 * @author Lark
 */
import junit.framework.Test;
import org.netbeans.jellytools.Bundle;
import org.netbeans.jemmy.QueueTool;
import org.netbeans.jemmy.operators.JEditorPaneOperator;
import org.netbeans.junit.NbModuleSuite;
import org.netbeans.test.javafx.editor.lib.JavaFXTestCase;
import org.netbeans.test.javafx.editor.lib.Util;

public class TemplateInd extends JavaFXTestCase {

    static String[] TESTS = {"testCreate", "testTemplate", "testCloseProject"}; //, "testIDELogForErrors"};
    public static String _bundle = "org.netbeans.test.javafx.editor.lib.Bundle";
    public static String _main = Bundle.getStringTrimmed(_bundle, "mainFX");
    public static String _source = Bundle.getStringTrimmed(_bundle, "sourcePackages");
    public static String _placeCodeHere = Bundle.getStringTrimmed(_bundle, "placeCodeHere");
    public static String GOLDEN = "goldenfiles/org/netbeans/test/javafx/editor/templates/TemplateInd.pass";
    protected static final String PROJECT_NAME = "TemplateInd";

    public TemplateInd(String name) {
        super(name);
    }

    public static Test suite() {
        return NbModuleSuite.create(TemplateInd.class, ".*", ".*", TESTS);
    }

    public void testCreate() {
        if ((Util.createProject(PROJECT_NAME, this.getWorkDirAsString())).equals(false)) {
            fail("Project " + PROJECT_NAME + " was not found.");
        }
        Util.waitScanFinished();
    }

    public void testTemplate() {
        //put sample code in editor 'Place Code Here'
        JEditorPaneOperator editor = Util.typeTemplateAtPlaceCodeHere(PROJECT_NAME, SAMPLE_PATH, "ind");
        new QueueTool().waitEmpty();

        //Diff it
        String goldenState = Util.getSourceCodeData(GOLDEN, this.getDataDirAsString());
        String currentState = Util.trimComments(editor.getText());

        if (Util.diff(currentState, goldenState)) {
            assertTrue("Pass: Editor Source Matches Golden Source.", true);
        } else {
            log("WHAT WE WANT TO SEE: Golden State: length = " + goldenState.length());
            log(goldenState);
            log("------");
            log("WHAT WE GOT: Current State: length = " + currentState.length());
            log(currentState);
            fail("Source does not match Golden.");
            log("------");
        }

    }

    public void testCloseProject() {
        Util.closeProject(PROJECT_NAME);
    }

/*
    public static void testIDELogForErrors() {
        String err = Util.hasUnexpectedException();
        String str = "";
        if (!(err.equals(""))) {
            assertTrue("Unexpected  exceptions found in message.log: " + err, str.equals(""));
        }
    }
 */
}

