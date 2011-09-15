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

package org.netbeans.test.visage.weather2.sample.lib;

import java.awt.Container;
import org.netbeans.jellytools.MainWindowOperator;
import org.netbeans.jellytools.NewProjectNameLocationStepOperator;
import org.netbeans.jellytools.NewProjectWizardOperator;
import org.netbeans.jellytools.OutputOperator;
import org.netbeans.jellytools.ProjectsTabOperator;
import org.netbeans.jellytools.actions.ActionNoBlock;
import org.netbeans.jellytools.actions.SaveAllAction;
import org.netbeans.jellytools.nodes.Node;
import org.netbeans.jemmy.QueueTool;
import org.netbeans.jemmy.TimeoutExpiredException;
import org.netbeans.jemmy.operators.JPopupMenuOperator;
import org.netbeans.jemmy.operators.JProgressBarOperator;
import org.netbeans.jemmy.util.Dumper;
import org.netbeans.jemmy.util.PNGEncoder;

/**
 *
 * @author Lark Fitzgerald
 */
public class Util {

    public static String WORKDIR = System.getProperty("xtest.workdir");
    public static String FILE_SEPARATOR = System.getProperty("file.separator");
    protected static final String _close = "Close";
    protected static final String _compile = "Compile";
    protected static final String _buildProject = "Build Project";
    private static final int WAIT_TIME = 2000;
    public static final long MAX_WAIT_TIME = 300000;

    /** Performs New Project > Samples > Visage > ...*/
    public static Boolean createSampleProject(String path, String projName, String location) {
        NewProjectWizardOperator projectWizard = NewProjectWizardOperator.invoke();
        projectWizard.selectCategory(path);
        projectWizard.next();

        NewProjectNameLocationStepOperator locationWizard = new NewProjectNameLocationStepOperator();
        locationWizard.txtProjectLocation().setText(location);

        projectWizard.finish();
        new QueueTool().waitEmpty();
        waitScanFinished();

        //Verifies that project exists
        try {
            ProjectsTabOperator pto = new ProjectsTabOperator();
            new Node(pto.invoke().tree(), projName);
            new QueueTool().waitEmpty();
        } catch (Exception e) {
            return false;
        }
        return true;
    }

    /** Compile project */
    public static Boolean compileProject(String projectName) {
        ProjectsTabOperator pto = new ProjectsTabOperator();
        Node projectNode = new Node(pto.invoke().tree(), projectName);
        new QueueTool().waitEmpty();
        sleep();
        JPopupMenuOperator item = projectNode.callPopup();
        item.pushMenuNoBlock(_buildProject);
        new QueueTool().waitEmpty();
        sleep();

        //Verify compilation
        try {
            OutputOperator oo = new OutputOperator();
            if (!oo.isShowing()) {
                new ActionNoBlock("Window|Output|Output", null).perform();
            }
            new QueueTool().waitEmpty();
            String output = oo.getText();
            CharSequence build = new String("BUILD");
            int done = 0;
            while (done < 3) {
                if (!output.contains(build)) { //Compilation hasn't finished yet or window isn't open.
                    sleep();
                    output = oo.getText();
                    done++;
                } else {
                    done = 3;
                }
            }
            CharSequence sucess = new String("BUILD SUCCESS");
            CharSequence warning = new String("warnings");
            if ((!output.contains(sucess)) || (output.contains(warning))) {
                return false;
            }
            return true;
        } catch (org.netbeans.jemmy.TimeoutExpiredException e) {
            //open it and try again
            new ActionNoBlock("Window|Output|Output", null).perform();
            new QueueTool().waitEmpty();
            try {
                OutputOperator oo = new OutputOperator();
                new QueueTool().waitEmpty();
                String output = oo.getText();
                CharSequence sucess = new String("BUILD SUCCESS");
                CharSequence warning = new String("warnings");
                if ((!output.contains(sucess)) || (output.contains(warning))) {
                    return false;
                }
                return true;
            } catch (org.netbeans.jemmy.TimeoutExpiredException e2) {
                return false; //output window not found
            }
        }
    }

    /** Compile single file using treePath */
    public static Boolean compileProjectFile(String path) {
        ProjectsTabOperator pto = new ProjectsTabOperator();
        Node projectNode = new Node(pto.invoke().tree(), path);
        new QueueTool().waitEmpty();
        sleep();
        JPopupMenuOperator item = projectNode.callPopup();
        item.pushMenuNoBlock(_compile);
        new QueueTool().waitEmpty();
        sleep();

        //Verify compilation
        try {
            OutputOperator oo = new OutputOperator();
            new QueueTool().waitEmpty();
            String output = oo.getText();
            CharSequence sucess = new String("BUILD SUCCESS");
            CharSequence warning = new String("warnings");
            if ((!output.contains(sucess)) || (output.contains(warning))) {
                return false;
            }
            return true;
        } catch (org.netbeans.jemmy.TimeoutExpiredException e) {
            //open it and try again
            new ActionNoBlock("Window|Output|Output", null).perform();
            new QueueTool().waitEmpty();
            try {
                OutputOperator oo = new OutputOperator();
                new QueueTool().waitEmpty();
                String output = oo.getText();
                CharSequence cs = new String("BUILD SUCCESS");
                if (!output.contains(cs)) {
                    return false;
                }
                return true;
            } catch (org.netbeans.jemmy.TimeoutExpiredException e2) {
                return false; //output window not found
            }
        }
    }

    /** Performs Save and Close of project */
    public static Boolean closeProject(String name) {
        new SaveAllAction().performAPI();
        new QueueTool().waitEmpty();
        Node projectNode = new Node(ProjectsTabOperator.invoke().tree(), name);
        JPopupMenuOperator item = projectNode.callPopup();
        item.pushMenuNoBlock(_close);
        new QueueTool().waitEmpty();
        sleep();

        //Verify Project is not listed and clear Output window.
        Boolean status = false;
        try {
            ProjectsTabOperator pto = new ProjectsTabOperator();
            new Node(pto.invoke().tree(), name);
        } catch (org.netbeans.jemmy.TimeoutExpiredException e) {
            status = true; //Should not find project
        }
        OutputOperator oo = new OutputOperator();
        new QueueTool().waitEmpty();
        oo.clear();
        return status;
    }

    /** Creates a screen capture of name in the workdir */
    public static void screenCapture(String name) { //screen.png
        String loc = WORKDIR + FILE_SEPARATOR + name;
        PNGEncoder.captureScreen(loc);
    }

    /** Creates a screen dump of name in the workdir */
    public static void screenDump(String name) { //screen.xml
        String loc = WORKDIR + FILE_SEPARATOR + name;
        try {
            Dumper.dumpAll(loc);
        } catch (Exception e) {
        }
    }

    public static void waitScanFinished() {
        try {
            Thread.sleep(3000);
        } catch (Exception e) {
        }

        long waitTime = 50;
        long waitCount = MAX_WAIT_TIME / waitTime;

        for (long time = 0; time < waitCount; time++) {
            try {
                Thread.sleep(waitTime);
            } catch (Exception e) {
            }

            Object scanning = JProgressBarOperator.findJProgressBar((Container) MainWindowOperator.getDefault().getSource());
            if (scanning == null) {
                return;
            }
        }
        throw new TimeoutExpiredException("Scaning isn't finished in " + MAX_WAIT_TIME + " ms");
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
