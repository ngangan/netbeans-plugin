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
 * Contributor(s): lfitzgerald
 * 
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */
package org.netbeans.test.javafx.bestpractices.samples;

import org.netbeans.jellytools.NewProjectWizardOperator;
import org.netbeans.jellytools.OutputOperator;
import org.netbeans.jellytools.ProjectsTabOperator;
import org.netbeans.jemmy.QueueTool;
import org.netbeans.test.javafx.bestpractices.lib.JavaFXTestCase;
import org.netbeans.jellytools.nodes.Node;
import org.netbeans.jemmy.operators.JPopupMenuOperator;

public class ColorWheel extends JavaFXTestCase {

    public String Color = "Color";
    public String color = "color";
    public String file = "ColorWheel.fx";
    public String color_wheel = "Color Wheel|Source Packages|color|ColorWheel.fx";
    
    public ColorWheel(String name) {
        super(name);
    }

    public void testColorWheelCreate() {
        NewProjectWizardOperator projectWizard = NewProjectWizardOperator.invoke();
        projectWizard.selectCategory(SAMPLE_PATH + Color);
        projectWizard.selectProject(Color);
        projectWizard.next();
        projectWizard.finish();
        new QueueTool().waitEmpty();
    }

    public void testColorWheelCompile() {
        ProjectsTabOperator pto = new ProjectsTabOperator();
        Node projectNode = new Node(pto.invoke().tree(), color_wheel);
        JPopupMenuOperator item = projectNode.callPopup();
        item.pushMenuNoBlock(COMPILE_FILE);
        new QueueTool().waitEmpty();
        
        OutputOperator oo = new OutputOperator();
        String output = oo.getText();
        CharSequence cs = new String("BUILD SUCCESS");
        if (!output.contains(cs)) {
            fail("Build Failed to compile: " + output);
        };

    }
    
    public void testCloseProject() {
        ProjectsTabOperator pto = new ProjectsTabOperator();
        new QueueTool().waitEmpty();
        Node projectNode = new Node(pto.invoke().tree(), color_wheel);
        JPopupMenuOperator item = projectNode.callPopup();
        item.pushMenuNoBlock(CLOSE);
        new QueueTool().waitEmpty();
    }

}

