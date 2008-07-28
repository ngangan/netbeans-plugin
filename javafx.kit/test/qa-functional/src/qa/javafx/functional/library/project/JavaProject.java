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
package qa.javafx.functional.library.project;

import javax.swing.JDialog;
import org.netbeans.jellytools.MainWindowOperator;
import org.netbeans.jellytools.NbDialogOperator;
import org.netbeans.jellytools.NewProjectWizardOperator;
import org.netbeans.jellytools.OutputTabOperator;
import org.netbeans.jellytools.ProjectsTabOperator;
import org.netbeans.jellytools.TopComponentOperator;
import org.netbeans.jellytools.actions.Action;
import org.netbeans.jellytools.actions.OpenAction;
import org.netbeans.jellytools.nodes.Node;
import org.netbeans.jellytools.nodes.ProjectRootNode;
import org.netbeans.jemmy.operators.JButtonOperator;
import org.netbeans.jemmy.operators.JCheckBoxOperator;
import org.netbeans.jemmy.operators.JDialogOperator;
import org.netbeans.jemmy.operators.JMenuBarOperator;
import org.netbeans.jemmy.operators.JTextFieldOperator;
import org.netbeans.junit.ide.ProjectSupport;
import qa.javafx.functional.library.Constant;
import qa.javafx.functional.library.Util;

/**
 *
 * @author Alexandr Scherbatiy
 */
public class JavaProject extends Project {

    /** Creates a new instance of JavaProject */
    private static long TIME_WAIT = 1000;
    public static final String BUILD_FAILED = "BUILD FAILED";
    public static final String BUILD_SUCCESSFUL = "BUILD SUCCESSFUL";
    String mainClass;
    ProjectRootNode rootNode;

    //JavaClassLoader classLoader;
    /**
     *
     * @param name
     * @param type
     */
    public JavaProject(String name) {
        this(name, ProjectType.JAVA_APPLICATION);
    }

    /**
     *
     * @param name
     * @param type
     */
    public JavaProject(String name, ProjectType type) {
        this(name, type, Util.WORK_DIR);
    }

    /**
     *
     * @param name
     * @param type
     * @param location
     */
    public JavaProject(String name, ProjectType type, String location) {
        this(name, type, location, null);
    }

    /**
     *
     * @param name
     * @param type
     * @param location
     * @param mainClass
     */
    public JavaProject(String name, ProjectType type, String location, String mainClass) {
        super(name, type, location);
        this.mainClass = mainClass;
        rootNode = new ProjectRootNode(ProjectsTabOperator.invoke().tree(), name);
    }

    /**
     *
     * @return
     */
    public String getMainClass() {
        return mainClass;
    }

    /**
     *
     * @return
     */
    public ProjectRootNode getProjectNode() {
        return rootNode;
    }

    /**
     *
     * @param name
     * @param type
     * @return
     */
    public static JavaProject createProject(String name, ProjectType type) {
        return createProject(name, type, Util.WORK_DIR);
    }

    /**
     *
     * @param name
     * @param type
     * @param location
     * @return
     */
    public static JavaProject createProject(String name, ProjectType type, String location) {
        return createProject(name, type, location, true);
    }

    public static JavaProject createProject(String name, ProjectType type, boolean setAsMain, boolean createMainClass) {
        return createProject(name, type, Util.WORK_DIR, setAsMain, createMainClass, null);
    }

    /**
     *
     * @param name
     * @param type
     * @param location
     * @param setAsMain
     * @return
     */
    public static JavaProject createProject(String name, ProjectType type, String location, boolean setAsMain) {
        return createProject(name, type, location, setAsMain, true, null);
    }

    /**
     *
     * @param name
     * @param type
     * @param location
     * @param setAsMain
     * @param createMainClass
     * @return
     */
    public static JavaProject createProject(String name, ProjectType type, String location, boolean setAsMain, boolean createMainClass) {
        return createProject(name, type, location, setAsMain, createMainClass, null);
    }

    /**
     *
     * @param name
     * @param type
     * @param location
     * @param setAsMain
     * @param createMainClass
     * @param mainClass
     * @return
     */
    public static JavaProject createProject(String name, ProjectType type, String location, boolean setAsMain, boolean createMainClass, String mainClass) {

        location = (location == null) ? Util.WORK_DIR : location;


        NewProjectWizardOperator newProject = NewProjectWizardOperator.invoke();
        //try{ Thread.sleep(TIME_WAIT); } catch(Exception e){}

        newProject.selectCategory(type.getCategoryName());
        newProject.selectProject(type.getProjectName());

        newProject.next();

        //try{ Thread.sleep(TIME_WAIT); } catch(Exception e){}

        //newProject.setName(name);

        new JTextFieldOperator(newProject, 0).setText(name);

        //try{ Thread.sleep(TIME_WAIT); } catch(Exception e){}

        JTextFieldOperator projectLocation = new JTextFieldOperator(newProject, 1);

        projectLocation.setText(location);


        new JCheckBoxOperator(newProject, 0).setSelected(setAsMain);
        new JCheckBoxOperator(newProject, 1).setSelected(createMainClass);

        JTextFieldOperator mainClassTextField = new JTextFieldOperator(newProject, 3);

        if (mainClass != null) {
            mainClassTextField.setText(mainClass);
        } else {
            mainClass = mainClassTextField.getText();
        }
        try {
            Thread.sleep(TIME_WAIT);
        } catch (Exception e) {
        }

        //this.location = location;
        //this.mainClass = mainClass;


        new JButtonOperator(newProject, "Finish").push();
        try {
            Thread.sleep(TIME_WAIT);
        } catch (Exception e) {
        }

        //Utils.waitScanningClassPath();

        return new JavaProject(name, type, location, mainClass);

    }

    public Node getSrcNode() {
        return new Node(getProjectNode(), "Source Packages");
    }

    public Node getFileNode(String file) {
        return new Node(getSrcNode(), file);
    }
    
    

    public EditorOperator getEditor(String fileName) {
//        String pack = getName().toLowerCase();
//        String mainFile = pack.replace('.', '|') + "|Main.fx";
//        Node mainFileNode = new Node(getSrcNode(), mainFile);
//        new OpenAction().performPopup(mainFileNode);
//        new TopComponentOperator("Main.fx");

        return new EditorOperator(fileName);


    }
    
    
    public void profile() {
        rootNode.performPopupActionNoBlock(Constant.POPUP_MENU_ITEM_PROFILE);
        JDialog dialog = JDialogOperator.waitJDialog(Constant.DIALOG_TITLE_ENABLE_PROFILING, false, true);

        System.out.println("[profiler] dialog: " + dialog);
        if (dialog != null) {
            new JButtonOperator(new JDialogOperator(dialog), Constant.BUTTON_OK).pushNoBlock();
        }

        NbDialogOperator profileOper = new NbDialogOperator("Profile " + getName());
        new JButtonOperator(profileOper, Constant.BUTTON_RUN).push();
        profileOper.waitClosed();
        //waitProgressDialog("Progress", 5000);
        new TopComponentOperator(Constant.TAB_PROFILER);
        new OutputTabOperator(getName()).waitText("Established local connection with the tool");
        Util.sleep(2000);
        new Action("Profile|Stop Profiling Session", null).perform();


    }
    
    
    public DebuggerOperator getDebugger(){
        return new DebuggerOperator(this);
    }
//    public void debug() {
//        rootNode.performPopupActionNoBlock(Constant.POPUP_MENU_ITEM_DEBUG);
//        JDialog dialog = JDialogOperator.waitJDialog(Constant.DIALOG_TITLE_DEBUG_PROJECT, false, true);
//
//        System.out.println("[debugger] dialog: " + dialog);
//        if (dialog != null) {
//            //JemmyProperties.setCurrentTimeout("DialogWaiter.WaitDialogTimeout", 60000);
//            //new JDialogOperator(dialog).waitClosed();
//            //Util.sleep(5000);
//            ProjectSupport.waitScanFinished();
//            new JDialogOperator(dialog).waitClosed();
//            
//            
//        }
//
//    }
//    
//    public void finishDebugger() {
//        //rootNode.performPopupActionNoBlock(Constant.POPUP_MENU_ITEM_PROFILE);
//        new Action("Run|Finish Debugger Session", null).perform();
//    }
    
    
    public void build() {
        rootNode.buildProject();
    }

    public void close() {
        rootNode.performPopupActionNoBlock("Close");
    }

    public Output getOutput() {
        
        new JMenuBarOperator(MainWindowOperator.getDefault()).pushMenuNoBlock("Window|Output|Output");
        return new Output(getName() + " (jar) ");
    }

    public class Output extends OutputTabOperator{

        //OutputTabOperator output = new OutputTabOperator(getName() + " (jar) ");

        public Output(String name){
            super(name);
        }
        
        public boolean isCompiled() {
            
        int timeout = 120;
        String outputText = getText();

        while (!(outputText.contains(BUILD_FAILED) || outputText.contains(BUILD_SUCCESSFUL) || timeout < 0)) {
            outputText = getText();
            timeout--;
            Util.sleep(1000);
        }
            
            return getText().contains(BUILD_SUCCESSFUL);
        }

    }
}
