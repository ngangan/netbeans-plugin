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

import org.netbeans.jellytools.actions.OpenAction;
import org.netbeans.jellytools.nodes.Node;
import org.netbeans.jemmy.operators.JButtonOperator;
import org.netbeans.jemmy.operators.JDialogOperator;
import org.netbeans.jemmy.operators.JRadioButtonOperator;
import org.netbeans.jemmy.operators.JTreeOperator;
import qa.javafx.functional.library.Constant;

/**
 *
 * @author Alexandr Scherbatiy
 */
public class JavaFXProject extends JavaProject {


    public DeploymentType deploymentType = DeploymentType.STANDARD;

    public JavaFXProject(String name) {
        super(name, ProjectType.JAVAFX_APPLICATION);
    }

    public JavaFXProject(String name, DeploymentType deploymentType) {
        super(name, ProjectType.JAVAFX_APPLICATION);
        this.deploymentType = deploymentType;
    }

    public JavaFXProject(JavaProject javaProject) {
        this(javaProject.getName());
    }

    public JavaFXProject(JavaProject javaProject, DeploymentType deploymentType) {
        this(javaProject.getName(), deploymentType);
    }

    public static JavaFXProject createProject(String name) {
        return new JavaFXProject(JavaProject.createProject(name, ProjectType.JAVAFX_APPLICATION));
    }
    public static JavaFXProject createProject(String name, DeploymentType deploymentType) {
        JavaProject project = JavaProject.createProject(name, ProjectType.JAVAFX_APPLICATION);
        return new JavaFXProject(project, deploymentType);
    }

    public String getMainFile() {
        return "Main.fx";
    }

    
    public EditorOperator openMainFile() {
        String pack = getName().toLowerCase();
        String mainFile = pack.replace('.', '|') + "|" + getMainFile();
        Node mainFileNode = new Node(getSrcNode(), mainFile);
        new OpenAction().performPopup(mainFileNode);

        return new EditorOperator("Main.fx");

    }
    
    public EditorOperator getMainEditor() {
        return new EditorOperator(getMainFile());
    }

    public void deploy() throws Exception{
        System.out.println("==== Deploy  ====");
        System.out.println("Deployment type = " + deploymentType);
        rootNode.performPopupActionNoBlock(Constant.POPUP_MENU_ITEM_PROPERTIES);
        //JDialog dialog = JDialogOperator.waitJDialog(Constant.DIALOG_TITLE_ENABLE_PROFILING, false, true);
        JDialogOperator propertyDialog = new JDialogOperator(Constant.DIALOG_TITLE_PROPERTIES);


        JTreeOperator categoryTree = new JTreeOperator(propertyDialog);

        categoryTree.selectPath(categoryTree.findPath("Run"));

        if(deploymentType == DeploymentType.MOBILE){

            JRadioButtonOperator button = new JRadioButtonOperator(propertyDialog, Constant.DEPLOYMENT_MOBILE);
            OperationSystem os = OperationSystem.getOS();
            if(os == OperationSystem.WINDOWS){
                button.push();
            } else {
                if (button.isEnabled()){
                    throw new Exception("Mobile deployment is enabled on " + os);
                }
            }
            //new JRadioButtonOperator(propertyDialog, Constant.DEPLOYMENT_MOBILE).push();
        }

        //Util.sleep(6000);
        
        new JButtonOperator(propertyDialog, Constant.BUTTON_OK).push();
        run();
       
    }
}
