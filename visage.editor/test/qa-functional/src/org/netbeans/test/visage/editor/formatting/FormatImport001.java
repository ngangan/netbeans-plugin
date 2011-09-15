/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
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
package org.netbeans.test.javafx.editor.formatting;

/**
 *
 * @author Lark
 */
import junit.framework.Test;
import java.io.File;
import org.netbeans.jellytools.ProjectsTabOperator;
import org.netbeans.jellytools.TopComponentOperator;
import org.netbeans.jellytools.actions.SaveAction;
import org.netbeans.jemmy.QueueTool;
import org.netbeans.jemmy.operators.JEditorPaneOperator;
import org.netbeans.junit.NbModuleSuite;
import org.netbeans.test.javafx.editor.lib.JavaFXTestCase;
import org.netbeans.test.javafx.editor.lib.Util;
import org.openide.filesystems.FileObject;
import org.netbeans.jellytools.nodes.Node;
import org.netbeans.jemmy.operators.JPopupMenuOperator;
import org.netbeans.junit.diff.LineDiff;

public class FormatImport001 extends JavaFXTestCase {

    static String[] TESTS = {
        "testOpenProject",
        "FormatImport001",
        "BackgroundImage",
        "testCloseProject"}; //, "testIDELogForErrors"};
    public static String _bundle = "org.netbeans.test.javafx.editor.lib.Bundle";

    protected static final String PROJECT_NAME = "fx-prj-2";
    protected final String DATA_DIR = this.getDataDirAsString();
    protected final String WORK_DIR = this.getWorkDirAsString();
    protected File DIFF_FILE;
    protected File REF_FILE;
    public FileObject SOURCE;

    public FormatImport001(String name) {
        super(name);
    }

    public static Test suite() {
        return NbModuleSuite.create(FormatImport001.class, ".*", ".*", TESTS);
    }

    /** Open Project opens he fx-prj-2 project which contains the
     * Format related .fx files
     */
    public void testOpenProject() {
        try {
            this.openDataProjects(PROJECT_NAME);
        } catch (java.io.IOException ioe) {
            fail("Unable to open data project: " + PROJECT_NAME + " at " + DATA_DIR);
        }
        log("Opened: " + PROJECT_NAME + " at " + DATA_DIR);
        Util.waitScanFinished();
    }

    void setSource(String filePath) {
        try {
            SOURCE = Util.getTestFile(this.getDataDir(), PROJECT_NAME, filePath);
        } catch (java.io.IOException ioe) {
            log("Unable to open file: " + filePath);
            fail(ioe.getCause());
        } catch (java.lang.InterruptedException ie) {
            log("Open interrupted for " + filePath);
            fail(ie.getCause());
        }
        log("Opened " + filePath + "in project: " + PROJECT_NAME);
    }

    void getFile(File file, String fileName) {
        try {
            file = new File(getWorkDir(), fileName);
        } catch (java.io.IOException ioe) {
            log("Unable to open file: " + fileName);
            fail(ioe.getCause());
        }
        log("Opened " + fileName);
    }

    public void FormatImport001() {
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj2|FormatImport001.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        fxsource.callPopup();
        new JPopupMenuOperator().pushMenuNoBlock("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("FormatImport001.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        //Format, Save and Close
        editor.clickForPopup();
        Util.clickPopup("Format");
        new QueueTool().waitEmpty();
        log("Right click > Format done.");
        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();

        //Diff
        setSource("fxprj2/FormatImport001.fx");
        getFile(DIFF_FILE, "FormatImport001.diff");
        getFile(REF_FILE, "FormatImport001.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void BackgroundImage() {
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj2|BackgroundImage.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        fxsource.callPopup();
        new JPopupMenuOperator().pushMenuNoBlock("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("BackgroundImage.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        //Format, Save and Close
        editor.clickForPopup();
        Util.clickPopup("Format");
        new QueueTool().waitEmpty();
        log("Right click > Format done.");
        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();

        //Diff
        setSource("fxprj2/BackgroundImage.fx");
        getFile(DIFF_FILE, "BackgroundImage.diff");
        getFile(REF_FILE, "BackgroundImage.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
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

