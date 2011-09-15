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
package qa.visage.functional.editor;

import java.util.StringTokenizer;
import qa.visage.functional.library.VisageTestCase;
import qa.visage.functional.library.project.EditorOperator;
import qa.visage.functional.library.project.VisageProject;

import junit.framework.Test;
import org.netbeans.junit.NbModuleSuite;
import qa.visage.functional.library.Util;

/**
 *
 * @author Alexandr Scherbatiy sunflower@netbeans.org
 */
public class JavaCodeFormatingTest extends VisageTestCase {

    public static String PROJECT_NAME = "CFTests";
    final static String ccPath = "editor/codeformating";

    final static String variablePath = "variable/";


    static final String[] TESTS = {
        // === Init  ===
        "testCreateProject",

        // === Variables ===
        "testCFVariables",
        "testCFVariablesSpaces",
        "testCFVariablesNegativeValue",

        "testCFDataTypes",
        "testCFVariableTrigger",
        "testCFVariablesBinding",



        // === Function ===

        "testCFFunction",
        "testCFFunctionBody",

        // === Class ===
        "testCFClass",

        // === Object Literal ===
        "testCFObjectLiteral",


    };


     // === Variables ===

    public void testCFVariables(){
        testCodeFormating("Variables");
    }

    public void testCFVariablesSpaces(){
        testCodeFormating(variablePath + "Spaces");
    }


    public void testCFVariablesNegativeValue(){
        testCodeFormating(variablePath + "NegativeValue");
    }


    public void testCFDataTypes(){
        testCodeFormating("DataTypes");
    }

    public void testCFVariableTrigger(){
        testCodeFormating("Variable_Trigger");
    }

    public void testCFVariablesBinding(){
        testCodeFormating(variablePath + "Binding");
    }

     // === Function ===

    public void testCFFunction(){
        testCodeFormating("Function");
    }

    public void testCFFunctionBody(){
        testCodeFormating("FunctionBody");
    }

    public void testCFClass(){
        testCodeFormating("Class");
    }


    public void testCFObjectLiteral(){
        testCodeFormating("ObjectLiteral");
    }




//    public static String[] TEMPLATES = {
//        "Function",
//        "FunctionBody",
//        "Class",
//        "Variables",
//
//    };
    
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

    public void testCreateProject() {
        System.out.println("============  Test Code Formating  =======");
        VisageProject project = VisageProject.createProject(PROJECT_NAME);

//        for(String template: TEMPLATES){
//            testCodeFormating(template);
//        }
//
//        System.out.println("Fail Code Formating tests: " + failComponents);
//        assertTrue("Failed Code Formating tests: " + failComponents, pass);

    }




    public void testCodeFormating(String testName) {
        VisageProject project = new VisageProject(PROJECT_NAME);
        
        EditorOperator editor = project.getMainEditor();

        String code = getCFSampleCode(testName);
        String goldenCode = getCFGoldenSampleCode(testName);
        
        editor.setText(code);
        editor.format();
        Util.sleep(4000);

        compare(testName, editor.getText(), goldenCode);
        
    }


    void compare(String template, String code, String goldenCode){

        //System.out.println("------- normilize code  -------------");
        code = normilizeString(code);
        //System.out.println("------- normilize golden -------------");
        goldenCode = normilizeString(goldenCode);

        System.out.println("--------" + template + "----------");
        System.out.println(code);
        System.out.println("----------------------------");
        System.out.println("------------ Golden --------");
        System.out.println(goldenCode);
        System.out.println("----------------------------");





        if(!goldenCode.equals(code)){
            pass = false;
            failComponents+= ", " + template;
            System.out.println("Fail");
            int size1 = code.length();
            int size2 = goldenCode.length();

            System.out.println("size: " + size1 + ":" + size2);
            System.out.println("1) [" + code.charAt(0) + "|" + code.charAt(1) + "|" + code.charAt(size1 - 2) + "|" + code.charAt(size1 - 1) + "]");
            System.out.println("2) [" + goldenCode.charAt(0) + "|" + goldenCode.charAt(1) + "|" + goldenCode.charAt(size2 - 2) + "|" + goldenCode.charAt(size2 - 1) + "]");

            fail(template + ": Golden file does not match");
        }

    }

        public static String normilizeString(String str) {
            StringTokenizer tokenizer = new StringTokenizer(str, "\n");
            String res = "";

            boolean begin = true;

            while(tokenizer.hasMoreTokens()){

                String s = reduceSpaces(tokenizer.nextToken());
                if(begin && "".equals(s)){
                    //System.out.println("[begin] '" + s + "'");
                }else {
                    begin = false;
                    res +=  s + "\n";
                }
            }
            
            //return res;
            return reduceSpaces(res);

        }

        public static String reduceSpaces(String str) {

            int ind = str.length() - 1;


            while(0 <= ind && isSpace(str.charAt(ind))){
                ind--;
            }

            if(ind < 0 || (ind == 0 && isSpace(str.charAt(0)))){
                return "";
            }else{
                //System.out.println("[reduce] " + ind + ": '" + str.substring(0, ind + 1) + "'");
                return str.substring(0, ind + 1);
            }

        }

        public static boolean  isSpace(char c) {
            return Character.isSpaceChar(c) || c ==' ' || c =='\t' || c == '\n' || c == '\r';
        }

    

}
