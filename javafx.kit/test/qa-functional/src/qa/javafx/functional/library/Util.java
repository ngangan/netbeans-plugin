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
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
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
 */


package qa.javafx.functional.library;

import java.awt.Component;
import java.awt.Container;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import javax.swing.AbstractButton;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.ListModel;
import javax.swing.table.TableModel;
import javax.swing.text.JTextComponent;
import javax.swing.tree.TreeModel;
//import org.netbeans.junit.ide.ProjectSupport;
import org.netbeans.jellytools.MainWindowOperator;

import org.netbeans.jemmy.TimeoutExpiredException;
import org.netbeans.jemmy.operators.ComponentOperator;
import org.netbeans.jemmy.operators.JProgressBarOperator;
import org.netbeans.jemmy.operators.JDialogOperator;
import org.netbeans.jemmy.operators.WindowOperator;

import java.io.FileOutputStream;
import java.net.URL;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;


/**
 *
 * @author Alexandr Scherbatiy sunflower@netbeans.org
 */


public class Util {

    
    
    
    protected static final String XTEST_DATA = "xtest.data";
    protected static final String XTEST_WORK_DIR = "xtest.workdir";
    public static String XTEST_DATA_PATH = System.getProperty(XTEST_DATA);


    private static final boolean FLAG_SHOW_DETAIL_INFORMATION = true;
    private static final boolean FLAG_SHOW_CLASS_HIERARCHY = false;

    
    private static final int N = 3;
    private static final int WAIT_TIME = 2000;

    public static String WORK_DIR = "tmp"; //System.getProperty(XTEST_WORK_DIR);
    
    public static final long MAX_WAIT_TIME = 300000;
    
    
    public static String getXtestDataPath() {
        //return System.getProperty(XTEST_DATA); // + "/data";
        return XTEST_DATA_PATH;
    }

    public static File getXtestNBMsPath() {
        return new File(new File(XTEST_DATA_PATH).getParentFile().getParentFile().getParentFile().getParentFile().getParentFile().getParentFile(), "nbbuild/nbms/javafx");
    }

    public static String getSampleText(String example) {

        System.out.println("[util] sample file: \"" + example + "\"");
        
        String sdkVersion = System.getProperty(Constant.JAVAFX_SDK_VERSION_PROPERTY);
        System.out.println("[util] sdk version: " + sdkVersion);
        
        String userDataPath = "";
        
        if(Constant.JAVAFX_SDK_REPRISE.equalsIgnoreCase(sdkVersion)){
            userDataPath = Constant.USER_DATA_REPRISE;
        } else if(Constant.JAVAFX_SDK_REPRISE.equalsIgnoreCase(sdkVersion)){
            userDataPath = Constant.USER_DATA_COMPILER;
        }else{
            userDataPath = Constant.USER_DATA_PATH_DEFAULT;
        }

        System.out.println("[util] user data path : " + userDataPath);
        
        //String examplePath = getXtestDataPath() + "/data/" + userDataPath + "/" + example;
        String examplePath = getXtestDataPath() + "/" + userDataPath + "/" + example;

        System.out.println("[util] example path : " + examplePath);

        //File file = new File(examplePath);
        try {
            BufferedReader input = new BufferedReader(new FileReader(examplePath));

            String text = "";

            String line = null;
            while ((line = input.readLine()) != null) {
                text += line + "\n";
            }
            return text;
        } catch (Exception e) {
            e.printStackTrace();
        }

        return null;
    }

    public static WindowOperator getMainWindowOperator() {
        return MainWindowOperator.getDefault();
    }

    public static void showIDE() {
        showComponents(MainWindowOperator.getDefault().getSource());
    }


    public static void showComponents(ComponentOperator comp) {
        showComponents(comp.getSource());
    }

    /**
     *
     * @param comp
     */
    public static void showComponents(Component comp) {
        showComponents("", comp);
    }

    /**
     *
     * @param blank
     * @param comp
     */
    public static void showComponents(String blank, Component comp) {
        System.out.println(blank + comp);

        showDetailedInformatin(blank, comp);
        showClassHierarchy(blank, comp);


        if (comp instanceof Container) {
            Container cont = (Container) comp;
            Component[] comps = cont.getComponents();

            for (Component c : comps) {
                showComponents(blank + " ", c);
            }
        }
    }


    /**
     *
     * @param obj
     */
    public static void showClassHierarchy(Object obj) {
        showClassHierarchy("", obj);
    }

    /**
     *
     * @param blank
     * @param obj
     */
    public static void showClassHierarchy(String blank, Object obj) {
        if (FLAG_SHOW_CLASS_HIERARCHY) {
            showClassHierarchy(blank + " ", obj.getClass());
        }
    }

    /**
     *
     * @param cls
     */
    protected static void showClassHierarchy(Class cls) {
        showClassHierarchy("", cls);
    }

    /**
     *
     * @param blank
     * @param cls
     */
    protected static void showClassHierarchy(String blank, Class cls) {


        Class superClass = cls.getSuperclass();
        if (superClass != null) {
            showClassHierarchy(blank + "  ", superClass);
        }

        Class[] interfaces = cls.getInterfaces();
        if (interfaces != null) {
            for (Class i : interfaces) {
                showClassHierarchy(blank + "  ", i);
            }
        }

        char c = (cls.isInterface()) ? ' ' : '+';

        System.out.println(blank + c + "\"" + cls.getName() + "\"");
    }

    public static void showDetailedInformatin(String blank, Component comp) {

        if (FLAG_SHOW_DETAIL_INFORMATION) {
            if (comp instanceof AbstractButton) {
                AbstractButton button = (AbstractButton) comp;
                System.out.println(blank + "[abstract button] { tooltip: " + button.getToolTipText() + " text:" + button.getText() + " action command: " + button.getActionCommand() +  "}");
            } else if (comp instanceof JTextComponent) {
                JTextComponent textComponent = (JTextComponent) comp;
                System.out.println(blank + "[text]");
                System.out.println(textComponent.getText());
            } else if (comp instanceof JList) {
                JList list = (JList) comp;
                
                ListModel model = list.getModel();
                for(int i =0; i < model.getSize(); i++ ){
                    System.out.println(blank + "[list] " + i + ", " + model.getElementAt(i) );
                }
            } else if (comp instanceof JComboBox) {
                JComboBox comboBox = (JComboBox) comp;

                for(int i =0; i < comboBox.getItemCount(); i++ ){
                    System.out.println(blank + "[combo box] " + i + ", \"" + comboBox.getItemAt(i) + "\"");
                }
                
            } else if (comp instanceof JTable) {
                JTable table = (JTable) comp;
                TableModel tableModel = table.getModel();
                System.out.println(blank + "[table] " + table.getRowCount() + ", " + table.getColumnCount());

                for (int i = 0; i < tableModel.getRowCount(); i++) {
                    for (int j = 0; j < tableModel.getColumnCount(); j++) {
                        System.out.println(blank + "  [" + i + "," + j + "] " + tableModel.getValueAt(i, j));
                    }
                }
            } else if (comp instanceof JTree) {
                JTree tree = (JTree) comp;

//                TableModel tableModel = table.getModel();
                System.out.println(blank + "[tree] " + tree);
                TreeModel model = tree.getModel();
                showTreeComponent(blank + " ", model, model.getRoot());

//                for (int i = 0; i < tableModel.getRowCount(); i++) {
//                    for (int j = 0; j < tableModel.getColumnCount(); j++) {
//                        System.out.println(blank + "  [" + i + "," + j + "] " + tableModel.getValueAt(i, j));
//                    }
//                }

            }
        }
    }


    public static void showTreeComponent(String blank, TreeModel model, Object node){
        System.out.println(blank + " - \"" +  node  + "\"");
        for(int i=0; i< model.getChildCount(node); i++){
            showTreeComponent(blank + " ", model, model.getChild(node, i));
        }

    }


    public static void unzipFile(String src, String dst){
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
                //System.out.println("entryname " + entryName);
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



    public static void waitProgressBar(ComponentOperator comp) {

        Container src = (Container) comp.getSource();
        int n = 0;

        for (n = 0; n < N; n++) {
            if (JProgressBarOperator.findJProgressBar(src) != null) {
                while (JProgressBarOperator.findJProgressBar(src) != null) {
                    sleep();
                }
                break;
            }
            sleep();
        }
    }


    public static void waitScanFinished(){

        System.out.println("*** [wait scan] start ***");
        try{Thread.sleep( 3000 ); }catch(Exception e) {}
        //ProjectSupport.waitScanFinished();
        try{Thread.sleep( 2000 ); }catch(Exception e) {}
        
        long waitTime = 2000;
        //long waitCount = MAX_WAIT_TIME / waitTime;
        System.out.println("*** [wait scan] ");
        
        for(long time=0; time < MAX_WAIT_TIME; time += waitTime){
            System.out.println("*** [wait scan] time " + time);
            try{Thread.sleep( waitTime ); }catch(Exception e) {}
            
            Object scanning = JProgressBarOperator.findJProgressBar((Container)MainWindowOperator.getDefault().getSource());
            if(scanning == null) { return; }
        }
        throw new TimeoutExpiredException("Scaning isn't finished in "+ MAX_WAIT_TIME+ " ms");
    }
    
    
// =================== Utility Operations  ===================

    public static void waitDialog(JDialogOperator  dialog) {
          sleep(2000);
          try{
            dialog.waitClosed();
          }catch(Throwable e){
             //e.printStackTrace();
            System.out.println("Dialog Wait Closed Exception: " + e.getMessage());
            sleep(10000);
          }
    }


    public static void sleep() {
        sleep(WAIT_TIME);
    }

    public static void sleep(long ms) {
        try {
            Thread.sleep(ms);
        } catch (InterruptedException ex) {
        }
    }
}
