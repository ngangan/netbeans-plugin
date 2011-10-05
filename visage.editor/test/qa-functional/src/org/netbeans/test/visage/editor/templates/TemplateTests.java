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

package org.netbeans.test.visage.editor.templates;

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
import org.netbeans.test.visage.editor.lib.VisageTestCase;
import org.netbeans.test.visage.editor.lib.Util;
import org.openide.filesystems.FileObject;
import org.netbeans.jellytools.nodes.Node;
import org.netbeans.jemmy.operators.JPopupMenuOperator;
import org.netbeans.junit.diff.LineDiff;

public class TemplateTests extends VisageTestCase {

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

    protected static final String PROJECT_NAME = "visage-prj-3";
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

    /** Open Project opens he visage-prj-2 project which contains the
     * Format related .visage files
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateAb.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateAb.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "ab";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateAb.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateAs.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        Util.sleep();
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateAs.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "as";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateAs.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateBi.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        Util.sleep();
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateBi.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "bi";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateBi.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateBo.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateBo.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "Bo";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateBo.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateBr.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateBr.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "br";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateBr.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateCa.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateCa.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "ca";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateCa.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateCl.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateCl.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "cl";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateCl.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateCn.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateCn.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "cn";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateCn.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateDe.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateDe.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "de";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateDe.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateDef.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateDef.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "def";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateDef.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateEl.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateEl.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "el";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateEl.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateEli.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateEli.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "eli";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateEli.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateEx.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateEx.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "ex";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateEx.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateFa.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateFa.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "fa";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateFa.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateFi.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateFi.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "fi";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateFi.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateFor.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateFor.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "for";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateFor.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateFu.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateFu.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "fu";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateFu.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateIf.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateIf.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "if";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateIf.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateIfe.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateIfe.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "ife";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateIfe.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateIm.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateIm.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "im";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateIm.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateIn.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateIn.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "In";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateIn.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateInd.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateInd.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "ind";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateInd.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateIns.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateIns.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "ins";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateIns.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateInsa.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateInsa.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "insa";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateInsa.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateInsb.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateInsb.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "insb";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateInsb.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateInv.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateInv.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "inv";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateInv.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateIof.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateIof.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "iof";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateIof.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateNu.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateNu.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "Nu";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateNu.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplatePa.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplatePa.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "pa";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplatePa.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplatePe.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplatePe.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "pe";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplatePe.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplatePi.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplatePi.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "pi";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplatePi.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplatePr.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplatePr.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "pr";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplatePr.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplatePu.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplatePu.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "pu";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplatePu.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateRe.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateRe.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "re";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateRe.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateSerr.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateSerr.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "serr";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateSerr.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateSo.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateSo.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "so";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateSo.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateSout.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateSout.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "sout";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateSout.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateSt.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateSt.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "St";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateSt.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateTh.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTh.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "th";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateTh.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateTof.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTof.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "tof";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateTof.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateTr.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTr.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "tr";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateTr.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateTrid.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTrid.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "trid";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateTrid.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateTrii.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTrii.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "trii";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateTrii.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateTrin.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTrin.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "trin";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateTrin.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateTrir.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        Util.sleep();
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTrir.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "trir";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateTrir.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateTw.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        Util.sleep();
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateTw.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "tw";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateTw.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateVar.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateVar.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "var";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateVar.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateWe.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateWe.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "we";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateWe.visage");
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
        String path = PROJECT_NAME + "|Source Packages|visageprj3|TemplateWh.visage";
        Node visagesource = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        visagesource.callPopup();
        new QueueTool().waitEmpty();
        new JPopupMenuOperator().pushMenu("Open");
        new QueueTool().waitEmpty();
        log("File path: " + path + " opened in editor.");

        // get Editor
        TopComponentOperator main = new TopComponentOperator("TemplateWh.visage");
        JEditorPaneOperator editor = new JEditorPaneOperator(main);

        String text = "wh";
        typeTemplate(text, editor);

        new SaveAction().perform();
        log("Ctrl-S.");
        new QueueTool().waitEmpty();
        main.closeWindow();
        Util.sleep();

        //Diff
        setSource("visageprj3/TemplateWh.visage");
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

