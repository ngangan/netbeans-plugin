/*
 * Util.java
 *
 * Created on July 10, 2007, 12:27 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package qa.javafx.functional.library;

import java.awt.Component;
import java.awt.Container;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.ListModel;
import javax.swing.table.TableModel;
import javax.swing.text.JTextComponent;
import org.netbeans.jellytools.MainWindowOperator;
import org.netbeans.jemmy.TimeoutExpiredException;
import org.netbeans.jemmy.operators.ComponentOperator;
import org.netbeans.jemmy.operators.JProgressBarOperator;

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
                
            } else if (comp instanceof JTable) {
                JTable table = (JTable) comp;
                TableModel tableModel = table.getModel();
                System.out.println(blank + "[table] " + table.getRowCount() + ", " + table.getColumnCount());

                for (int i = 0; i < tableModel.getRowCount(); i++) {
                    for (int j = 0; j < tableModel.getColumnCount(); j++) {
                        System.out.println(blank + "  [" + i + "," + j + "] " + tableModel.getValueAt(i, j));
                    }
                }
            }
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
        
        try{Thread.sleep( 3000 ); }catch(Exception e) {}
        
        long waitTime = 50;
        long waitCount = MAX_WAIT_TIME / waitTime;
        
        for(long time=0; time < waitCount; time++){
            try{Thread.sleep( waitTime ); }catch(Exception e) {}
            
            Object scanning = JProgressBarOperator.findJProgressBar((Container)MainWindowOperator.getDefault().getSource());
            if(scanning == null) { return; }
        }
        throw new TimeoutExpiredException("Scaning isn't finished in "+ MAX_WAIT_TIME+ " ms");
    }
    
    
// =================== Utility Operations  ===================
    
    public static void sleep() {
        sleep(WAIT_TIME);
    }

    public static void sleep(int ms) {
        try {
            Thread.sleep(ms);
        } catch (InterruptedException ex) {
        }
    }
}
