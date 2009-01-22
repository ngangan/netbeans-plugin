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
package qa.javafx.functional.editor;

import qa.javafx.functional.library.JavaFXTestCase;
import qa.javafx.functional.library.project.EditorOperator;
import qa.javafx.functional.library.project.JavaFXProject;

import junit.framework.Test;
import org.netbeans.junit.NbModuleSuite;
import qa.javafx.functional.library.Util;

/**
 *
 * @author Alexandr Scherbatiy sunflower@netbeans.org
 */
public class JavaCodeFormatingTest extends JavaFXTestCase {

    public static String PROJECT_NAME = "CFTests";
    final static String ccPath = "editor/codeformating";

    static final String[] TESTS = {
        "testCFFunctions",
    };

    public static String[] TEMPLATES = {
        "Function",
        "FunctionBody",
        "Class",
        "Variables",

    };
    
    public boolean pass = true;
    public String failComponents = "";

    public JavaCodeFormatingTest(String name) {
        super(name);
    }

    public static Test suite() {
        return NbModuleSuite.create(JavaCodeFormatingTest.class, ".*", ".*", TESTS);

    }

    public String getCFSampleCode(String fileName) {
        return Util.getSampleText(ccPath + "/" + fileName + ".fx");
    }

    public String getCFGoldenSampleCode(String fileName) {
        return Util.getSampleText(ccPath + "/" + fileName + "_golden.fx");
    }

    public void testCFFunctions() {
        System.out.println("============  Test Code Formating  =======");
        JavaFXProject project = JavaFXProject.createProject(PROJECT_NAME);

        for(String template: TEMPLATES){
            testCodeFormating(template);
        }

        System.out.println("Fail Code Formating tests: " + failComponents);
        assertTrue("Failed Code Formating tests: " + failComponents, pass);

    }

    public void testCodeFormating(String testName) {
        JavaFXProject project = new JavaFXProject(PROJECT_NAME);
        
        EditorOperator editor = project.getMainEditor();

        String code = getCFSampleCode(testName);
        String goldenCode = getCFGoldenSampleCode(testName);
        
        editor.setText(code);
        editor.format();
        Util.sleep(4000);

        compare(testName, editor.getText(), goldenCode);
        
    }


    void compare(String template, String code, String goldenCode){

        code = code.trim();
        goldenCode = goldenCode.trim();

        System.out.println("--------" + template + "----------");
        System.out.println("code before\n" + code);
        System.out.println("----------------------------");

        System.out.println("----------------------------");
        System.out.println("code golden\n" + goldenCode);
        System.out.println("----------------------------");


        if(!goldenCode.equals(code)){
            pass = false;
            failComponents+= ", " + template;
        }

    }
    
}
