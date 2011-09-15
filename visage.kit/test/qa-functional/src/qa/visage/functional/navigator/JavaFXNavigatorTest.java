/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Oracle and/or its affiliates. All rights reserved.
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
package qa.visage.functional.navigator;

import org.netbeans.jellytools.TopComponentOperator;
import org.netbeans.jemmy.operators.JTreeOperator;
import qa.visage.functional.library.VisageTestCase;
import qa.visage.functional.library.Util;
import qa.visage.functional.library.project.EditorOperator;
import qa.visage.functional.library.project.VisageProject;

import junit.framework.Test;
import junit.textui.TestRunner;
import org.netbeans.junit.NbModuleSuite;
import org.netbeans.junit.NbTestSuite;


/**
 *
 * @author Alexandr Scherbatiy sunflower@netbeans.org
 */
public class VisageNavigatorTest extends VisageTestCase {

    public static String PROJECT_NAME = "TestNavigator";

    static final String[] TESTS = {
        "testNavigator",
        "testVariable",
    };


    public VisageNavigatorTest(String name) {
        super(name);
    }

    public static Test suite() {
        return NbModuleSuite.create(VisageNavigatorTest.class, ".*", ".*", TESTS);

    }

    
    public void testNavigator(){
        VisageProject project  = VisageProject.createProject(PROJECT_NAME);
        TopComponentOperator navigator = new TopComponentOperator("Navigator");
        
        assertNotNull("Navigator has not been found.", navigator);
        
    }

    public void testVariable(){
        VisageProject project = new VisageProject(PROJECT_NAME);
        EditorOperator editor = project.openMainFile();
        editor.setText("var a = 10;");
        
        Util.sleep(3000);
        TopComponentOperator navigator = new TopComponentOperator("Navigator");
        
        JTreeOperator tree = new JTreeOperator(navigator);
        
        System.out.println("[show] navigator:");

        Util.showComponents(tree);
        
        
        
        
        
        
    }
    
    
}
