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

public class TemplateTests extends JavaFXTestCase {

    static String[] TESTS = {
        "testOpenProject", //Must always do this
        "TemplateAb",
        "TemplateAs",
        "TemplateBi",
        "TemplateBo",
        "TemplateBr", //5
        "TemplateCa",
        "TemplateCl",
        "TemplateCn",
        "TemplateDe",
        "TemplateDef", //10
        "TemplateEl", 
        "TemplateEli",
        "TemplateEx",
        "TemplateFa",
        "TemplateFi", //15
        "TemplateFor", 
        "TemplateFu",
        "TemplateIf",
        "TemplateIfe",
        "TemplateIm", //20
        "TemplateIn", 
        "TemplateInd",
        "TemplateIns",
        "TemplateInsa",
        "TemplateInsb", //25
        "TemplateInv", 
        "TemplateIof",
        "TemplateNu",
        "TemplatePa",
        "TemplatePe", //30
        "TemplatePi",
        "TemplatePr",
        "TemplatePu", 
        "TemplateRe",
        "TemplateSerr", //35
        "TemplateSout",
        "TemplateSo",
        "TemplateSt", 
        "TemplateTh",
        "TemplateTof", //40
        "TemplateTr",
        "TemplateTrid",
        "TemplateTrii", 
        "TemplateTrin",
        "TemplateTrir", //45
        "TemplateTw",
        "TemplateVar",
        "TemplateWe", 
        "TemplateWh",
        "testCloseProject"}; //, "testIDELogForErrors"};

    protected static final String PROJECT_NAME = "fx-prj-3";
    protected final String DATA_DIR = this.getDataDirAsString();
    protected final String WORK_DIR = this.getWorkDirAsString();
    public File DIFF_FILE;
    public File REF_FILE;
    public FileObject SOURCE;

    public TemplateTests(String name) {
        super(name);
    }

    public static Test suite() {
        return NbModuleSuite.create(TemplateTests.class, ".*", ".*", TESTS);
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

    void typeTemplate(String text, JEditorPaneOperator editor) {
        for (int i = 0; i < text.length(); i++) {
            editor.typeKey(text.charAt(i));
        }
        editor.typeKey('\t'); //Tab
        new QueueTool().waitEmpty();
        log(text + " + tab done.");
    }

    public void TemplateAb() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateAb.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateAb.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "ab";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateAb.fx");
        getFile(DIFF_FILE, "TemplateAb.diff");
        getFile(REF_FILE, "TemplateAb.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
        

    }

    public void TemplateAs() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateAs.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        Util.sleep();
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateAs.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "as";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateAs.fx");
        getFile(DIFF_FILE, "TemplateAs.diff");
        getFile(REF_FILE, "TemplateAs.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateBi() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateBi.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        Util.sleep();
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateBi.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "bi";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateBi.fx");
        getFile(DIFF_FILE, "TemplateBi.diff");
        getFile(REF_FILE, "TemplateBi.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateBo() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateBo.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateBo.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "Bo";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateBo.fx");
        getFile(DIFF_FILE, "TemplateBo.diff");
        getFile(REF_FILE, "TemplateBo.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateBr() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateBr.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateBr.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "br";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateBr.fx");
        getFile(DIFF_FILE, "TemplateBr.diff");
        getFile(REF_FILE, "TemplateBr.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateCa() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateCa.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateCa.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "ca";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateCa.fx");
        getFile(DIFF_FILE, "TemplateCa.diff");
        getFile(REF_FILE, "TemplateCa.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateCl() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateCl.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateCl.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "cl";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateCl.fx");
        getFile(DIFF_FILE, "TemplateCl.diff");
        getFile(REF_FILE, "TemplateCl.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateCn() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateCn.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateCn.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "cn";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateCn.fx");
        getFile(DIFF_FILE, "TemplateCn.diff");
        getFile(REF_FILE, "TemplateCn.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateDe() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateDe.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateDe.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "de";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateDe.fx");
        getFile(DIFF_FILE, "TemplateDe.diff");
        getFile(REF_FILE, "TemplateDe.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateDef() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateDef.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateDef.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "def";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateDef.fx");
        getFile(DIFF_FILE, "TemplateDef.diff");
        getFile(REF_FILE, "TemplateDef.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateEl() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateEl.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateEl.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "el";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateEl.fx");
        getFile(DIFF_FILE, "TemplateEl.diff");
        getFile(REF_FILE, "TemplateEl.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateEli() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateEli.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateEli.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "eli";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateEli.fx");
        getFile(DIFF_FILE, "TemplateEli.diff");
        getFile(REF_FILE, "TemplateEli.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateEx() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateEx.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateEx.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "ex";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateEx.fx");
        getFile(DIFF_FILE, "TemplateEx.diff");
        getFile(REF_FILE, "TemplateEx.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateFa() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateFa.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateFa.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "fa";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateFa.fx");
        getFile(DIFF_FILE, "TemplateFa.diff");
        getFile(REF_FILE, "TemplateFa.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateFi() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateFi.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateFi.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "fi";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateFi.fx");
        getFile(DIFF_FILE, "TemplateFi.diff");
        getFile(REF_FILE, "TemplateFi.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateFor() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateFor.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateFor.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "for";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateFor.fx");
        getFile(DIFF_FILE, "TemplateFor.diff");
        getFile(REF_FILE, "TemplateFor.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateFu() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateFu.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateFu.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "fu";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateFu.fx");
        getFile(DIFF_FILE, "TemplateFu.diff");
        getFile(REF_FILE, "TemplateFu.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateIf() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateIf.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateIf.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "if";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateIf.fx");
        getFile(DIFF_FILE, "TemplateIf.diff");
        getFile(REF_FILE, "TemplateIf.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateIfe() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateIfe.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateIfe.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "ife";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateIfe.fx");
        getFile(DIFF_FILE, "TemplateIfe.diff");
        getFile(REF_FILE, "TemplateIfe.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateIm() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateIm.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateIm.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "im";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateIm.fx");
        getFile(DIFF_FILE, "TemplateIm.diff");
        getFile(REF_FILE, "TemplateIm.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateIn() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateIn.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateIn.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "In";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateIn.fx");
        getFile(DIFF_FILE, "TemplateIn.diff");
        getFile(REF_FILE, "TemplateIn.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateInd() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateInd.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateInd.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "ind";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateInd.fx");
        getFile(DIFF_FILE, "TemplateInd.diff");
        getFile(REF_FILE, "TemplateInd.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateIns() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateIns.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateIns.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "ins";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateIns.fx");
        getFile(DIFF_FILE, "TemplateIns.diff");
        getFile(REF_FILE, "TemplateIns.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateInsa() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateInsa.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateInsa.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "insa";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateInsa.fx");
        getFile(DIFF_FILE, "TemplateInsa.diff");
        getFile(REF_FILE, "TemplateInsa.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateInsb() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateInsb.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateInsb.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "insb";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateInsb.fx");
        getFile(DIFF_FILE, "TemplateInsb.diff");
        getFile(REF_FILE, "TemplateInsb.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateInv() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateInv.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateInv.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "inv";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateInv.fx");
        getFile(DIFF_FILE, "TemplateInv.diff");
        getFile(REF_FILE, "TemplateInv.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateIof() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateIof.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateIof.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "iof";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateIof.fx");
        getFile(DIFF_FILE, "TemplateIof.diff");
        getFile(REF_FILE, "TemplateIof.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateNu() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateNu.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateNu.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "Nu";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateNu.fx");
        getFile(DIFF_FILE, "TemplateNu.diff");
        getFile(REF_FILE, "TemplateNu.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplatePa() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplatePa.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplatePa.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "pa";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplatePa.fx");
        getFile(DIFF_FILE, "TemplatePa.diff");
        getFile(REF_FILE, "TemplatePa.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplatePe() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplatePe.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplatePe.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "pe";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplatePe.fx");
        getFile(DIFF_FILE, "TemplatePe.diff");
        getFile(REF_FILE, "TemplatePe.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplatePi() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplatePi.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplatePi.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "pi";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplatePi.fx");
        getFile(DIFF_FILE, "TemplatePi.diff");
        getFile(REF_FILE, "TemplatePi.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplatePr() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplatePr.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplatePr.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "pr";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplatePr.fx");
        getFile(DIFF_FILE, "TemplatePr.diff");
        getFile(REF_FILE, "TemplatePr.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplatePu() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplatePu.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplatePu.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "pu";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplatePu.fx");
        getFile(DIFF_FILE, "TemplatePu.diff");
        getFile(REF_FILE, "TemplatePu.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateRe() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateRe.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateRe.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "re";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateRe.fx");
        getFile(DIFF_FILE, "TemplateRe.diff");
        getFile(REF_FILE, "TemplateRe.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateSerr() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateSerr.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateSerr.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "serr";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateSerr.fx");
        getFile(DIFF_FILE, "TemplateSerr.diff");
        getFile(REF_FILE, "TemplateSerr.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateSo() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateSo.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateSo.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "so";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateSo.fx");
        getFile(DIFF_FILE, "TemplateSo.diff");
        getFile(REF_FILE, "TemplateSo.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateSout() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateSout.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateSout.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "sout";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateSout.fx");
        getFile(DIFF_FILE, "TemplateSout.diff");
        getFile(REF_FILE, "TemplateSout.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateSt() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateSt.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateSt.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "St";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateSt.fx");
        getFile(DIFF_FILE, "TemplateSt.diff");
        getFile(REF_FILE, "TemplateSt.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateTh() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateTh.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTh.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "th";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateTh.fx");
        getFile(DIFF_FILE, "TemplateTh.diff");
        getFile(REF_FILE, "TemplateTh.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateTof() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateTof.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTof.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "tof";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateTof.fx");
        getFile(DIFF_FILE, "TemplateTof.diff");
        getFile(REF_FILE, "TemplateTof.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateTr() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateTr.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTr.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "tr";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateTr.fx");
        getFile(DIFF_FILE, "TemplateTr.diff");
        getFile(REF_FILE, "TemplateTr.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateTrid() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateTrid.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTrid.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "trid";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateTrid.fx");
        getFile(DIFF_FILE, "TemplateTrid.diff");
        getFile(REF_FILE, "TemplateTrid.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateTrii() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateTrii.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTrii.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "trii";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateTrii.fx");
        getFile(DIFF_FILE, "TemplateTrii.diff");
        getFile(REF_FILE, "TemplateTrii.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateTrin() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateTrin.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTrin.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "trin";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateTrin.fx");
        getFile(DIFF_FILE, "TemplateTrin.diff");
        getFile(REF_FILE, "TemplateTrin.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateTrir() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateTrir.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        Util.sleep();
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTrir.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "trir";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateTrir.fx");
        getFile(DIFF_FILE, "TemplateTrir.diff");
        getFile(REF_FILE, "TemplateTrir.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateTw() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateTw.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        Util.sleep();
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTw.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "tw";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateTw.fx");
        getFile(DIFF_FILE, "TemplateTw.diff");
        getFile(REF_FILE, "TemplateTw.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateVar() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateVar.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateVar.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "var";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateVar.fx");
        getFile(DIFF_FILE, "TemplateVar.diff");
        getFile(REF_FILE, "TemplateVar.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateWe() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateWe.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateWe.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "we";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateWe.fx");
        getFile(DIFF_FILE, "TemplateWe.diff");
        getFile(REF_FILE, "TemplateWe.ref"); //Not used yet.

        //TODO
        //getSource as String
        //getPass as String
        //output only when fails.

        this.assertFile("Output does not match Golden.", new File(SOURCE.getPath()) , this.getGoldenFile(), DIFF_FILE,  new LineDiff(false));
    }

    public void TemplateWh() {
        new QueueTool().waitEmpty();
        ProjectsTabOperator pto = new ProjectsTabOperator();
        String path = PROJECT_NAME + "|Source Packages|fxprj3|TemplateWh.fx";
        Node fxsource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        fxsource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateWh.fx");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "wh";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("fxprj3/TemplateWh.fx");
        getFile(DIFF_FILE, "TemplateWh.diff");
        getFile(REF_FILE, "TemplateWh.ref"); //Not used yet.

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

