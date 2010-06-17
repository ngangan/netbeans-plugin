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
package qa.javafx.functional.fxd;

import junit.framework.Test;
import org.netbeans.jellytools.NewFileWizardOperator;
import org.netbeans.jemmy.operators.JTextFieldOperator;
import org.netbeans.junit.NbModuleSuite;
import qa.javafx.functional.library.Util;
import qa.javafx.functional.library.project.JavaFXProject;
import org.netbeans.jellytools.nodes.Node;

/**
 *
 * @author Alexandr Scherbatiy sunflower@netbeans.org
 */
public class JavaFXFxdFileTestCase extends JavaFXFxdTestCase {


    static final String PROJECT_NAME = "FXDProject";
    static final String FILE_NAME = "FXDFile";



    public JavaFXFxdFileTestCase(String name) {
        super(name);
    }

    public static Test suite() {
        return NbModuleSuite.create(JavaFXFxdFileTestCase.class, ".*", ".*", TESTS);

    }
    static final String[] TESTS = {
        "testCreateFile",
    };


    public void testCreateFile() {

        String pack = PROJECT_NAME.toLowerCase();
        
        System.out.println("*** FXD Test File");
        JavaFXProject project = JavaFXProject.createProject(PROJECT_NAME);
        project.popup(pack, "New|Other...");


        NewFileWizardOperator wizard = new NewFileWizardOperator();
        wizard.selectCategory("JavaFX");
        wizard.selectFileType("Empty FXD File");
        wizard.btNext().push();
        new JTextFieldOperator(wizard).setText(FILE_NAME);
        Util.sleep(3000);
        //System.out.println("Press Finish Button");
        wizard.btFinish().push();
        //wizard.finish();
        //System.out.println("Select File");
        
        Node f = project.getFileNode(pack + "|" + FILE_NAME + ".fxd");
        //Node f = new Node(p, FILE_NAME + ".fxd");
        assertNotNull("FXD file '" + FILE_NAME + "' is not created!" , f);
        
    }



}
