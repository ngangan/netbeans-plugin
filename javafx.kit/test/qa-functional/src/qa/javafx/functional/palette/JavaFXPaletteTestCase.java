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
package qa.javafx.functional.palette;

import javax.swing.ListModel;
import qa.javafx.functional.library.JavaFXTestCase;
import qa.javafx.functional.library.project.EditorOperator;
import qa.javafx.functional.library.project.JavaFXProject;

import junit.framework.Test;
import org.netbeans.jemmy.operators.JListOperator;
import org.netbeans.junit.NbModuleSuite;
import qa.javafx.functional.library.Util;
import qa.javafx.functional.library.operator.FXPaletteOperator;

/**
 *
 * @author Alexandr Scherbatiy sunflower@netbeans.org
 */
public class JavaFXPaletteTestCase extends JavaFXTestCase {

    public static String PROJECT_NAME = "TestPalette";


//    static final String CATEGORY_APPLICATIONS = "Applications";
//    static final String CATEGORY_BASIC_SHAPES = "Basic Shapes";
//    static final String CATEGORY_CONTROLS = "Controls";
//    static final String CATEGORY_TRANSFORMATIONS = "Transformations";
//    static final String CATEGORY_COLORS  = "Colors";
//    static final String CATEGORY_GRADIENTS = "Gradients";
//    static final String CATEGORY_EFFECTS = "Effects";
//    static final String CATEGORY_ANIMATION = "Animation";
//    static final String CATEGORY_MEDIA = "Media";
//    static final String CATEGORY_SWING_COMPONENTS = "Swing Components";


    public boolean pass = true;
    public String failComponents = "";

    public JavaFXPaletteTestCase(String name) {
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
        JavaFXProject project = JavaFXProject.createProject(getProjectName(category));
        project.openOutput();
        project.build();

        //JavaFXProject project = new JavaFXProject(PROJECT_NAME + "_" + category);
        
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
                pass = false;
                failComponents += "[" + category + ":" + item + "]";
                System.out.println("---  Failed Component ---");
                System.out.println(editor.getText());
                System.out.println("-------------------------");
            }
            
        }

        System.out.println("Fail components: " + failComponents);
        assertTrue("Failed Palette items: " + failComponents, pass);
        
    }

}
