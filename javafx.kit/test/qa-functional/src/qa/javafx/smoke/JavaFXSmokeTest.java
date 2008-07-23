package qa.javafx.smoke;

import java.awt.Point;
import qa.javafx.functional.library.Util;
import java.io.File;
import java.io.FileFilter;
import javax.swing.JToggleButton;
import org.netbeans.jellytools.Bundle;
import org.netbeans.jellytools.MainWindowOperator;
import org.netbeans.jellytools.NewProjectWizardOperator;
import org.netbeans.jellytools.OutputTabOperator;
import org.netbeans.jellytools.PaletteOperator;
import org.netbeans.jellytools.ProjectsTabOperator;
import org.netbeans.jellytools.TopComponentOperator;
import org.netbeans.jellytools.nodes.Node;
import org.netbeans.jellytools.nodes.ProjectRootNode;
import org.netbeans.jemmy.TimeoutExpiredException;
import org.netbeans.jemmy.operators.ContainerOperator;
import org.netbeans.jemmy.operators.JButtonOperator;
import org.netbeans.jemmy.operators.JCheckBoxOperator;
import org.netbeans.jemmy.operators.JDialogOperator;
import org.netbeans.jemmy.operators.JListOperator;
import org.netbeans.jemmy.operators.JMenuBarOperator;
import org.netbeans.jemmy.operators.JTabbedPaneOperator;
import org.netbeans.jemmy.operators.JTableOperator;
import org.netbeans.jemmy.operators.JTextComponentOperator;
import org.netbeans.jemmy.operators.JTextFieldOperator;
import org.netbeans.jemmy.operators.JToggleButtonOperator;
import org.netbeans.junit.NbTestSuite;

import qa.javafx.functional.library.JavaFXTestCase;
import qa.javafx.functional.library.MouseRobot;

/**
 *
 * @author Alexandr Scherbatiy sunflower@netbeans.org
 */
public class JavaFXSmokeTest extends JavaFXTestCase {

    public JavaFXSmokeTest(String name) {
        super(name);
    }

    public static NbTestSuite suite() {
        NbTestSuite suite = new NbTestSuite();
        suite.addTest(new JavaFXSmokeTest("testLoadModule"));
        suite.addTest(new JavaFXSmokeTest("testProjectCreation"));
        suite.addTest(new JavaFXSmokeTest("testMainFile"));
        suite.addTest(new JavaFXSmokeTest("testPalette"));
        suite.addTest(new JavaFXSmokeTest("testProjectBuilding"));
        suite.addTest(new JavaFXSmokeTest("testEditor"));
        suite.addTest(new JavaFXSmokeTest("testPreview"));
        //suite.addTest(new JavaFXSmokeTest("testProfiler"));
        return suite;
    }

    public void testLoadModule() {

        new JMenuBarOperator(MainWindowOperator.getDefault()).pushMenuNoBlock("Tools|Plugins");
        String pluginTitle = Bundle.getString("org.netbeans.modules.autoupdate.ui.actions.Bundle", "PluginManager_Panel_Name");

        JDialogOperator plugin = new JDialogOperator(pluginTitle);
        Util.waitProgressBar(plugin);

        JTabbedPaneOperator tabbedPane = new JTabbedPaneOperator(plugin);

        tabbedPane.selectPage("Installed");
        new JTextFieldOperator(tabbedPane).typeText("JavaFX");
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
        NewProjectWizardOperator projectWizard = NewProjectWizardOperator.invoke();
        projectWizard.selectCategory("JavaFX");
        projectWizard.selectProject("JavaFX Script Application");
        projectWizard.next();
        new JTextFieldOperator(projectWizard, 0).setText(PROJECT_NAME_HELLO_WORLD);
        new JTextFieldOperator(projectWizard, 1).setText(System.getProperty("netbeans.user"));
        projectWizard.finish();

    }

    public void testMainFile() {
        try {
            Node projectNode = new Node(ProjectsTabOperator.invoke().tree(), PROJECT_NAME_HELLO_WORLD);
            Node mainFileNode = new Node(projectNode, "Source Packages|helloworld|Main.fx");
            assertNotNull("Main fx file has not been found", mainFileNode);
            
            TopComponentOperator mainFile = new TopComponentOperator("Main.fx");
        } catch (TimeoutExpiredException e) {
            fail("Known issue: Main FX file is not open after project creation");
        }

    }


    public void testPalette() {
        //System.out.println("Test Palette!");

        TopComponentOperator main = new TopComponentOperator("Main.fx");
        JTextComponentOperator textComponent = new JTextComponentOperator(main);


        PaletteOperator palette = new PaletteOperator();

        assertNotNull("Palette is not open!!!", palette);

        palette.selectComponent("Frame");
        
        System.out.println("[palette] Drag and Drop");
        
        JListOperator list = palette.lstComponents();
        Point point = list.getClickPoint(0);
        
        int x1 = point.x;
        int y1 = point.y;

        int x2 = textComponent.getCenterXForClick();
        int y2 = textComponent.getCenterYForClick();

        //System.out.println("[palette] Drag and Drop");
        
        MouseRobot.dragNDrop(list, x1, y1, textComponent, x2, y2);
        

        Util.sleep(1000);

    }

    public void testProjectBuilding() {
        JMenuBarOperator menuBar = new JMenuBarOperator(MainWindowOperator.getDefault());
        menuBar.pushMenuNoBlock("Window|Output|Output");


        ProjectRootNode projectNode = new ProjectRootNode(ProjectsTabOperator.invoke().tree(), PROJECT_NAME_HELLO_WORLD);
        projectNode.buildProject();
        Util.sleep(1000);

        OutputTabOperator output = new OutputTabOperator(PROJECT_NAME_HELLO_WORLD + " (jar) ");

        String outputText = output.getText();

        int timeout = 120;

        while (!(outputText.contains(BUILD_FAILED) || outputText.contains(BUILD_SUCCESSFUL) || timeout < 0)) {
            outputText = output.getText();
            timeout--;
            Util.sleep(1000);
        }


        //System.out.println("[output] " + outputText);
        assertTrue("Project is not built!", outputText.contains(BUILD_SUCCESSFUL));


    }
    
    public void testEditor() {

        TopComponentOperator main = new TopComponentOperator("Main.fx");
        JTextComponentOperator textComponent = new JTextComponentOperator(main);
        textComponent.setText("");
        Util.sleep(1000);
        
        String sample = "samples/helloworld/HelloWorld.fx";
        String text = Util.getSampleText(sample);
        assertNotNull("Sample \"" + sample + "\" was not found", text);
        textComponent.setText(text);
    }
    

    public void testPreview() {
        System.setOut(getLog());
        Node umlNode = new Node(ProjectsTabOperator.invoke().tree(), PROJECT_NAME_HELLO_WORLD);
        TopComponentOperator main = new TopComponentOperator("Main.fx");
        JTextComponentOperator textComponent = new JTextComponentOperator(main);
        String sample = "samples/helloworld/HelloWorld.fx";
        String text = Util.getSampleText(sample);
        assertNotNull("Sample \"" + sample + "\" was not found", text);
        textComponent.setText(text);
        //new JButtonOperator(main, "Enable Preview").push();
        ContainerOperator cont = new ContainerOperator(main, new ClassNameComponentChooser("PreviewButton"));
        //Util.showComponents(cont);
        JToggleButtonOperator preview = new JToggleButtonOperator((JToggleButton) cont.getSource());
        preview.push();
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
    
//     public void testProfiler() {
//        JavaFXProject profiler  = JavaFXProject.createProject("SmokeProfiler");
//        EditorOperator main = profiler.openMainFile();
//        String code = Util.getSampleText(Constant.SMOKE_PROFILER_FILE_PATH);
//        main.insert(code);
//        Util.sleep(4000);
//        
//    }
}