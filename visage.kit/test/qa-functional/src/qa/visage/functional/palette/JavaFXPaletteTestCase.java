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
package qa.visage.functional.palette;

import javax.swing.ListModel;
import qa.visage.functional.library.VisageTestCase;
import qa.visage.functional.library.project.EditorOperator;
import qa.visage.functional.library.project.VisageProject;

import junit.framework.Test;
import org.netbeans.jemmy.operators.JListOperator;
import org.netbeans.junit.NbModuleSuite;
import qa.visage.functional.library.Util;
import qa.visage.functional.library.operator.FXPaletteOperator;

/**
 *
 * @author Alexandr Scherbatiy sunflower@netbeans.org
 */
public class VisagePaletteTestCase extends VisageTestCase {

    public static String PROJECT_NAME = "TestPalette";


    public String[] filter = {};

    public boolean pass = true;
    public String failComponents = "";

    public VisagePaletteTestCase(String name) {
        super(name);
    }



    String getProjectName(String name){
        String projectName = PROJECT_NAME + "_" + name;
        return projectName.replaceAll(" ", "");
    }

    public void testItems(String category) {
        testItems(category, "", "");
    }

    
//    public void testItems(String category, String template) {
//
//        testItems(category, template, 0, 0);
//    }


    public void testItems(String category, String begin, String end) {
    //public void testItems(String category, String template, int deltaX, int deltaY) {
        VisageProject project = VisageProject.createProject(getProjectName(category));
        project.openOutput();
        project.build();

        //VisageProject project = new VisageProject(PROJECT_NAME + "_" + category);
        
        EditorOperator editor = project.getMainEditor();

        FXPaletteOperator palette = new FXPaletteOperator();

        JListOperator list = palette.getList(category);

        ListModel model = list.getModel();
        for (int i = 0; i < model.getSize(); i++) {
            //editor.setText("package testpalette_" + category + ";\n");
            editor.setText("package " + project.getMainPackage() + ";\n" + begin);

            //int line = 4;
            //editor.setCaretPosition(line, 0);
            //Util.sleep(5000);
            Object item = model.getElementAt(i);
            System.out.println("=============   Test Item: " + item + "====================");
            palette.dragNDrop(list, i, editor);

            // Remove a comma from the end of the palette items (Paths, Gradients)
            String editorText = editor.getText().trim();
            System.out.println("---- Text to trim ---");
            System.out.println(editorText);
            System.out.println("---------------------");
            if(editorText.endsWith(",")){
                editor.setText(editorText.substring(0, editorText.length() - 1));
            }


            editor.insert("\n" + end, editor.getLineNumber(), 0);
            project.build();
            Util.sleep(2000);

            if(!project.getOutput().isCompiled()){
                System.out.println("---  Failed Component ---");
                System.out.println(editor.getText());
                System.out.println("-------------------------");
                boolean filtered = false;
                for(String f: filter){
                    if(f.equals(item.toString())){
                        filtered = true;
                        break;
                    }
                }
                if(! filtered){
                    pass = false;
                    failComponents += "[" + category + ":" + item + "]";
                }else{
                    System.out.println("   Filtered!!!");
                }
            }
            
        }

        System.out.println("Fail components: " + failComponents);
        assertTrue("Failed Palette items: " + failComponents, pass);
        
    }

}
