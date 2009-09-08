package qa.javafx.validation;

import qa.javafx.smoke.*;
import qa.javafx.functional.library.Util;
import java.io.File;
import java.io.FileFilter;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import org.netbeans.jellytools.Bundle;
import org.netbeans.jellytools.MainWindowOperator;
//import org.netbeans.jellytools.ProjectsTabOperator;
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
import org.netbeans.jemmy.operators.JListOperator;

import qa.javafx.functional.library.JavaFXTestCase;
import qa.javafx.functional.library.project.EditorOperator;
import qa.javafx.functional.library.project.JavaFXProject;

import junit.framework.Test;
import junit.textui.TestRunner;
import org.netbeans.jemmy.operators.JRadioButtonOperator;
import org.netbeans.junit.NbModuleSuite;
import org.netbeans.junit.NbTestSuite;
import qa.javafx.functional.library.Constant;
import qa.javafx.functional.library.operator.FXPaletteOperator;
import qa.javafx.functional.library.operator.FXPreviewOperator;


import java.io.File;
import java.io.FileOutputStream;
import java.net.URL;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 *
 * @author Alexandr Scherbatiy sunflower@netbeans.org
 */

public class JavaFXValidationTest extends JavaFXTestCase {


    final static String DIALOG_MANAGER_TITLE = "Java Platform Manager";
    final static String DIALOG_PLATFORMS_TITLE = "Add Java Platform";


    final static String JAVAFX_SDK_LABEL = "Test JavaFX SDK";

    final static String JAVAFX_SDK_DIR = "TBD";


    static String[] TESTS = {
        "testJavaFXPlatform",
    };



    public JavaFXValidationTest(String name) {
        super(name);
    }


    public static Test suite() {
        return NbModuleSuite.create(JavaFXValidationTest.class, ".*", ".*", TESTS);

    }

    public static void main(String[] args) {
        TestRunner.run(new NbTestSuite(JavaFXValidationTest.class));
    }

    public void testJavaFXPlatform() {
        //System.setOut(getLog());

        System.out.println("  Test JavaFX Plaform  ");


        new JMenuBarOperator(MainWindowOperator.getDefault()).pushMenuNoBlock("Tools|Java Platforms");
        JDialogOperator managerDialog = new JDialogOperator(DIALOG_MANAGER_TITLE);

        JButtonOperator addPlatformButton = new JButtonOperator(managerDialog, "Add Platform...");
        addPlatformButton.push();

        
        JDialogOperator platformsDialog = new JDialogOperator(DIALOG_PLATFORMS_TITLE);
        JRadioButtonOperator platformRadioButton = new JRadioButtonOperator(platformsDialog, "JavaFX Platform");
        platformRadioButton.push();

        JButtonOperator nextButton = new JButtonOperator(platformsDialog, "Next >");
        nextButton.push();

        JTextFieldOperator nameTextField = new JTextFieldOperator(platformsDialog, 0);
        nameTextField.setText(JAVAFX_SDK_LABEL);

        JTextFieldOperator folderTextField = new JTextFieldOperator(platformsDialog, 1);
        folderTextField.setText(JAVAFX_SDK_DIR);


        JButtonOperator finishButton = new JButtonOperator(platformsDialog, "Finish");
        finishButton.push();

         Util.sleep(2000);

        JButtonOperator closeButton = new JButtonOperator(managerDialog, "Close");
        closeButton.push();

        Util.sleep(2000);

        //JButtonOperator browseButton = new JButtonOperator(platformsDialog, "Browse...");

        // Create a JavaFX Project
         JavaFXProject project = JavaFXProject.createProject("TestJavaFX Project");

         project.getProjectNode().performPopupActionNoBlock("Properties");



        JDialogOperator propertiesDialog = new JDialogOperator("Project Properties");

        Util.sleep(2000);

        //JListOperator listOperator = new JListOperator(propertiesDialog);
        //listOperator.clickOnItem("Libraries");

        Util.sleep(7000);



        

    }


    public void saveJavaFXSDK(){
        String urlString = "TBD";

        try {
            String destinationname = "TBD";
            byte[] buf = new byte[1024];
            ZipInputStream zipinputstream = null;
            ZipEntry zipentry;
            URL url = new URL(urlString);


            zipinputstream = new ZipInputStream(url.openStream());

            zipentry = zipinputstream.getNextEntry();
            while (zipentry != null) {
                //for each entry to be extracted
                String entryName = zipentry.getName();
                System.out.println("entryname " + entryName);
                int n;
                FileOutputStream fileoutputstream;
                File newFile = new File(entryName);
                String directory = newFile.getParent();

                if (directory == null) {
                    if (newFile.isDirectory()) {
                        break;
                    }
                }

                if (zipentry.isDirectory()) {
                    new File(destinationname + "/" + entryName).mkdirs();
                } else {
                    //System.out.println("Save: '" + destinationname + entryName + "'");
                    fileoutputstream = new FileOutputStream(
                            destinationname + entryName);

                    while ((n = zipinputstream.read(buf, 0, 1024)) > -1) {
                        fileoutputstream.write(buf, 0, n);
                    }

                    fileoutputstream.close();
                }
                zipinputstream.closeEntry();
                zipentry = zipinputstream.getNextEntry();

            }

            zipinputstream.close();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    



  
  
}