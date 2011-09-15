/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
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
 * Contributor(s):
 *
 * Portions Copyrighted 2007 Sun Microsystems, Inc.
 */

package qa.visage.smoke;

import qa.visage.functional.library.Util;
import java.io.File;
import java.io.FileFilter;
import org.netbeans.jellytools.Bundle;
import org.netbeans.jellytools.MainWindowOperator;
import org.netbeans.jellytools.ProjectsTabOperator;
import org.netbeans.jellytools.TopComponentOperator;
import org.netbeans.jellytools.nodes.Node;
import org.netbeans.jemmy.TimeoutExpiredException;
import org.netbeans.jemmy.operators.JButtonOperator;
import org.netbeans.jemmy.operators.JCheckBoxOperator;
import org.netbeans.jemmy.operators.JDialogOperator;
import org.netbeans.jemmy.operators.JMenuBarOperator;
import org.netbeans.jemmy.operators.JTabbedPaneOperator;
import org.netbeans.jemmy.operators.JTableOperator;
import org.netbeans.jemmy.operators.JTextComponentOperator;
import org.netbeans.jemmy.operators.JTextFieldOperator;

import qa.visage.functional.library.VisageTestCase;
import qa.visage.functional.library.project.EditorOperator;
import qa.visage.functional.library.project.VisageProject;

import junit.framework.Test;
import junit.textui.TestRunner;
import org.netbeans.junit.NbModuleSuite;
import org.netbeans.junit.NbTestSuite;
import qa.visage.functional.library.Constant;
import qa.visage.functional.library.operator.FXPaletteOperator;
import qa.visage.functional.library.operator.FXPreviewOperator;

/**
 *
 * @author Alexandr Scherbatiy sunflower@netbeans.org
 */
public class VisageSmokeTest extends VisageTestCase {

    static String[] TESTS = {
        "testLoadModule",
        "testProjectCreation",
        "testMainFile",
        "testProjectBuilding",
        "testPalette",
        "testEditor",
        "testPreview",
        //"testProfiler",
    };



    public VisageSmokeTest(String name) {
        super(name);
    }

//    public static NbTestSuite suite() {
//        NbTestSuite suite = new NbTestSuite();
//        suite.addTest(new VisageSmokeTest("testLoadModule"));
//        suite.addTest(new VisageSmokeTest("testProjectCreation"));
//        suite.addTest(new VisageSmokeTest("testMainFile"));
//        suite.addTest(new VisageSmokeTest("testPalette"));
//        suite.addTest(new VisageSmokeTest("testProjectBuilding"));
//        suite.addTest(new VisageSmokeTest("testEditor"));
//        suite.addTest(new VisageSmokeTest("testPreview"));
//        //suite.addTest(new VisageSmokeTest("testProfiler"));
//        return suite;
//    }

    public static Test suite() {
        return NbModuleSuite.create(VisageSmokeTest.class, ".*", ".*", TESTS);

    }

    public static void main(String[] args) {
        TestRunner.run(new NbTestSuite(VisageSmokeTest.class));
    }


    public void testLoadModule() {

        new JMenuBarOperator(MainWindowOperator.getDefault()).pushMenuNoBlock("Tools|Plugins");
        String pluginTitle = Bundle.getString("org.netbeans.modules.autoupdate.ui.actions.Bundle", "PluginManager_Panel_Name");

        JDialogOperator plugin = new JDialogOperator(pluginTitle);
        Util.waitProgressBar(plugin);

        JTabbedPaneOperator tabbedPane = new JTabbedPaneOperator(plugin);

        tabbedPane.selectPage("Installed");
        new JTextFieldOperator(tabbedPane).typeText("Visage");
        Util.sleep(2000);

        JTableOperator table = new JTableOperator(tabbedPane);

        if (table.getRowCount() == 0) {
            System.out.println("load module] (-) module is not loaded");

            tabbedPane.selectPage("Downloaded");
            //Util.showComponents(plugin.getSource());
            loadPlugins(plugin);

            Util.sleep();
        } else {
            System.out.println("load module] (+) module is loaded");
            new JButtonOperator(plugin, "Close").pushNoBlock();
        }
    }

    public void loadPlugins(JDialogOperator pluginManager) {
        //System.setOut(getLog());
        //String nbmsPath = Util.getXtestNBMsPath();
        //System.out.println("[xtest path] \"" + nbmsPath + "\"");
        File nbmsPath = Util.getXtestNBMsPath();
        System.out.println("[nbms path] \"" + nbmsPath + "\"");

        assertTrue("Dir does not exist:\"" + nbmsPath.getAbsolutePath(), nbmsPath.isDirectory());


        File[] nbmList = nbmsPath.listFiles(new FileFilter() {

            public boolean accept(File pathname) {
                System.out.println("[filter] \"" + pathname.getName() + "\"");

                return pathname.getName().endsWith(".nbm");
            }
        });

        System.out.println("[add plugins]");

        String nbms = "";
        for (File file : nbmList) {
            nbms += "\"" + file.getAbsolutePath() + "\" ";
        }
        //for (File file : nbmList) {

        new JButtonOperator(pluginManager, "Add Plugins...").push();

        JDialogOperator addPlugins = new JDialogOperator("Add Plugins");
        JTextFieldOperator textField = new JTextFieldOperator(addPlugins);
        //textField.setText(file.getAbsolutePath());
        textField.setText(nbms);
        new JButtonOperator(addPlugins, "Open").push();
        //System.out.println("[load] \"" + file.getAbsolutePath() + "\"");
        System.out.println("[load] \"" + nbms + "\"");

        //Util.sleep();
        //}
        Util.sleep(2000);


        new JButtonOperator(pluginManager, "Install").pushNoBlock();
        //Util.sleep(900000);

        JDialogOperator ideInstaller = new JDialogOperator("NetBeans IDE Installer");
        new JButtonOperator(ideInstaller, "Next >").pushNoBlock();
        Util.sleep(1000);


        ideInstaller = new JDialogOperator("NetBeans IDE Installer");

        //System.out.println("[check box] select");
        //Util.showComponents(ideInstaller);
        new JCheckBoxOperator(ideInstaller).doClick();
        //System.out.println("[check box] install");

        new JButtonOperator(ideInstaller, "Install").pushNoBlock();

        new JButtonOperator(new JDialogOperator("Validation Warning"), "Continue").push();
        new JButtonOperator(ideInstaller, "Finish").pushNoBlock();

    //  IDE Restarts
    }

    public void testProjectCreation() {
        VisageProject project = VisageProject.createProject(PROJECT_NAME_HELLO_WORLD);
        project.openOutput();


    }

    public void testMainFile() {
        try {
            VisageProject project = new VisageProject(PROJECT_NAME_HELLO_WORLD);
            assertNotNull("Main fx file has not been found", project.getFileNode("helloworld|Main.fx"));
            
            
            TopComponentOperator mainFile = new TopComponentOperator("Main.fx");
        } catch (TimeoutExpiredException e) {
            fail("Known issue: Main FX file is not open after project creation");
        }

    }

    public void testProjectBuilding() {
        VisageProject project = new VisageProject(PROJECT_NAME_HELLO_WORLD);
        project.build();


        System.out.println("Project name: '" + project.getName()  +"'");
        assertTrue("Project is not built!", project.getOutput().isCompiled());
    }

    public void testPalette() {
        //System.out.println("Test Palette!");

        VisageProject project = new VisageProject(PROJECT_NAME_HELLO_WORLD);
        EditorOperator editor = project.getMainEditor();
        editor.setText("package helloworld;\n");


        TopComponentOperator main = new TopComponentOperator("Main.fx");
        JTextComponentOperator textComponent = new JTextComponentOperator(main);


        FXPaletteOperator palette = new FXPaletteOperator();

        assertNotNull("Palette is not open!!!", palette);

        palette.selectComponent("Stage");
        
        System.out.println("[palette] Drag and Drop");
        palette.dragNDrop(textComponent);

//        JListOperator list = palette.lstComponents();
//        Point point = list.getClickPoint(0);
//
//        int x1 = point.x;
//        int y1 = point.y;
//
//        int x2 = textComponent.getCenterXForClick();
//        int y2 = textComponent.getCenterYForClick();
//
//        //System.out.println("[palette] Drag and Drop");
//
//        MouseRobot.dragNDrop(list, x1, y1, textComponent, x2, y2);
        

        Util.sleep(8000);
        project.build();
        assertTrue("Project is not built!", project.getOutput().isCompiled());
        

    }

    
    public void testEditor() {

        VisageProject project = new VisageProject(PROJECT_NAME_HELLO_WORLD);
        EditorOperator editor = project.getMainEditor();
        editor.setText();
        
        String sample = "samples/helloworld/HelloWorld.fx";
        String text = Util.getSampleText(sample);
        assertNotNull("Sample \"" + sample + "\" was not found", text);
        editor.setText(text);
        
        
    }
    

    public void testPreview() {
        //System.setOut(getLog());
        Node umlNode = new Node(ProjectsTabOperator.invoke().tree(), PROJECT_NAME_HELLO_WORLD);
        TopComponentOperator main = new TopComponentOperator("Main.fx");
        JTextComponentOperator textComponent = new JTextComponentOperator(main);
        String sample = "samples/helloworld/HelloWorld.fx";
        String text = Util.getSampleText(sample);
        assertNotNull("Sample \"" + sample + "\" was not found", text);
        textComponent.setText(text);

        FXPreviewOperator preview = new FXPreviewOperator(main);
        preview.enable();
        Util.sleep(7000);

//        //new JButtonOperator(main, "Enable Preview").push();
//        ContainerOperator cont = new ContainerOperator(main, new ClassNameComponentChooser("PreviewButton"));
//        //Util.showComponents(cont);
//        JToggleButtonOperator preview = new JToggleButtonOperator((JToggleButton) cont.getSource());
//        preview.push();

        
    //Util.sleep(7000);
    //Util.
    //Util.showComponents(main);

    //JInternalFrameOperator mainInternalFrame = new JInternalFrameOperator(main, PREVIEW_FRAME_TITLE);

    //assertNotNull("Internal Frame: \"" + PREVIEW_FRAME_TITLE + "\" is not shown in the Preview!"  , mainInternalFrame);
    //*/
    //fail();
    //Util.showComponents(new TopComponentOperator("Main.fx"));
    //Util.showComponents(main);
    }
    
     public void testProfiler() {
        VisageProject profiler  = VisageProject.createProject("SmokeProfiler");
        EditorOperator main = profiler.openMainFile();
        String code = Util.getSampleText(Constant.SMOKE_PROFILER_FILE_PATH);

        main.setText(code);
        profiler.profile();
        //assertTrue("Prifiler project is not compiled!", profiler.getOutput().isCompiled());
        Util.sleep(4000);

        
    }
}