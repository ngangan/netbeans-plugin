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

package qa.visage.validation;

import qa.visage.functional.library.Util;
import org.netbeans.jellytools.Bundle;
import org.netbeans.jellytools.MainWindowOperator;
//import org.netbeans.jellytools.ProjectsTabOperator;
import org.netbeans.jellytools.TopComponentOperator;
import org.netbeans.jellytools.nodes.Node;

import org.netbeans.jemmy.operators.*;

import qa.visage.functional.library.VisageTestCase;
import qa.visage.functional.library.project.VisageProject;

import junit.framework.Test;
import junit.textui.TestRunner;
import org.netbeans.jemmy.operators.JRadioButtonOperator;
import org.netbeans.junit.NbModuleSuite;
import org.netbeans.junit.NbTestSuite;

import java.io.File;
import java.io.FileOutputStream;
import java.net.URL;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import javax.swing.tree.TreePath;
import qa.visage.functional.library.project.OperationSystem;

/**
 *
 * @author Alexandr Scherbatiy sunflower@netbeans.org
 */

public class VisageValidationTest extends VisageTestCase {


    final static String DIALOG_MANAGER_TITLE = "Java Platform Manager";
    final static String DIALOG_PLATFORMS_TITLE = "Add Java Platform";


    final static String JAVAFX_SDK_LABEL = "Test Visage SDK";

    final static String JAVAFX_SDK_VER = "visage-sdk1.2";
    final static String JAVAFX_SDK_URL = "http://jre.sfbay.sun.com/java/re/visage/1.2.1/promoted/fcs/b06/bundles";

    String JAVAFX_SDK_DIR;



    static String[] TESTS = {
        "testVisagePlatform",
        "testVisageProject",
    };



    public VisageValidationTest(String name) {
        super(name);
    }


    public static Test suite() {
        return NbModuleSuite.create(VisageValidationTest.class, ".*", ".*", TESTS);

    }

    public static void main(String[] args) {
        TestRunner.run(new NbTestSuite(VisageValidationTest.class));
    }



    
    public void testVisagePlatform() {
        //System.setOut(getLog());

        System.out.println("[validation] setup");
        JAVAFX_SDK_DIR= getDataDir().getAbsolutePath() + File.separator;
        System.out.println("XTEST_WORK_DIR = " + JAVAFX_SDK_DIR);

        System.out.println("  Test Visage Plaform  ");

        String dst = Util.WORK_DIR;

        System.out.println("Destination dir: \"" + JAVAFX_SDK_DIR + "\"");
        
        saveVisageSDK();
        

        new JMenuBarOperator(MainWindowOperator.getDefault()).pushMenuNoBlock("Tools|Java Platforms");
        JDialogOperator managerDialog = new JDialogOperator(DIALOG_MANAGER_TITLE);

        JButtonOperator addPlatformButton = new JButtonOperator(managerDialog, "Add Platform...");
        addPlatformButton.push();

        
        JDialogOperator platformsDialog = new JDialogOperator(DIALOG_PLATFORMS_TITLE);
        JRadioButtonOperator platformRadioButton = new JRadioButtonOperator(platformsDialog, "Visage Platform");
        platformRadioButton.push();

        JButtonOperator nextButton = new JButtonOperator(platformsDialog, "Next >");
        nextButton.push();

        JTextFieldOperator nameTextField = new JTextFieldOperator(platformsDialog, 0);
        nameTextField.setText(JAVAFX_SDK_LABEL);

        JTextFieldOperator folderTextField = new JTextFieldOperator(platformsDialog, 1);
        folderTextField.setText(JAVAFX_SDK_DIR + "/" + JAVAFX_SDK_VER);


        JButtonOperator finishButton = new JButtonOperator(platformsDialog, "Finish");
        finishButton.push();

         Util.sleep(2000);

        JButtonOperator closeButton = new JButtonOperator(managerDialog, "Close");
        closeButton.push();

        Util.sleep(2000);


        //JButtonOperator browseButton = new JButtonOperator(platformsDialog, "Browse...");

    }

    public void testVisageProject() {

        // Create a Visage Project


         VisageProject project = VisageProject.createProject("TestVisageProject");

         project.getProjectNode().performPopupActionNoBlock("Properties");



        JDialogOperator propertiesDialog = new JDialogOperator("Project Properties");


        //JListOperator listOperator = new JListOperator(propertiesDialog);
        //listOperator.clickOnItem("Libraries");

        JTreeOperator treeOperator = new JTreeOperator(propertiesDialog);
        //System.out.println("*********  Show Tree *****************");
        //Util.showComponents(treeOperator);
        Object root = treeOperator.getRoot();
        //TreeModel model = treeOperator.getModel();
        
        TreePath treepPath = new TreePath(new Object[]{root, treeOperator.getModel().getChild(root, 1)});
        treeOperator.selectPath(treepPath);

        //System.out.println("*********  Show Properties Dialog *****************");
        //Util.showComponents(propertiesDialog);
        //Util.sleep(2000);

        JComboBoxOperator comboboxOperator = new JComboBoxOperator(propertiesDialog);

        //comboboxOperator.setSelectedItem(JAVAFX_SDK_LABEL);

        boolean checkSDK = false;
        for(int i=0; i< comboboxOperator.getItemCount(); i++){
            Object obj = comboboxOperator.getItemAt(i);
            if(obj != null && JAVAFX_SDK_LABEL.equals(obj.toString())){
                checkSDK = true;
                System.out.println("Item2 : \"" + obj + "\"");
                comboboxOperator.setSelectedIndex(i);
            }
        }

        new JButtonOperator(propertiesDialog, "OK").push();
        
        if(!checkSDK){
            fail("Custom Visage SDK is not found in the project properties");
        }

        Util.waitScanFinished();

        project.build();

        if(!project.getOutput().isCompiled()){
            fail("Project BUILD FAILS");
            System.out.println(project.getOutput().getText());
        }


        System.out.println("The end");


        Util.sleep(7000);



        

    }


    public void saveVisageSDK(){

        //windows-i586/visage_sdk-1_2_1-windows-i586.zip

        OperationSystem os = OperationSystem.getOS();
        String label = os.getLabel();

        String url = JAVAFX_SDK_URL + "/" + label + "/visage_sdk-1_2_1-" + label + ".zip";
        
        new File(JAVAFX_SDK_DIR).mkdirs();
        unzipFile(url, JAVAFX_SDK_DIR);
    }

    public void unzipFile(String src, String dst){
        System.out.println("*** UNzip File");
        System.out.println("   from: \"" + src + "\"");
        System.out.println("   to  : \"" + dst + "\"");

        String urlString = src;

        try {
            String destinationname = dst;
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