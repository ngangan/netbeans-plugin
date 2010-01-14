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
package qa.javafx.functional.perfomance;

import java.io.File;
import java.net.URL;
import qa.javafx.functional.library.JavaFXTestCase;
import qa.javafx.functional.library.Util;

import junit.framework.Test;
import org.netbeans.junit.NbModuleSuite;
import qa.javafx.functional.library.project.JavaFXProject;
import qa.javafx.functional.library.project.Project;


/**
 *
 * @author andromeda
 */
public class JavaFXPerfomanceTest extends JavaFXTestCase {

    static final String[] TESTS = {
        "testBuildBigProject",
    };


    public JavaFXPerfomanceTest(String name) {
        super(name);
    }

    public static Test suite() {
        return NbModuleSuite.create(JavaFXPerfomanceTest.class, ".*", ".*", TESTS);

    }


    public void testBuildBigProject() throws Exception{

        System.out.println("*** Test Big Project ***");
        String root = Util.getXtestDataPath();

        System.out.println("src: '" + root  + "'");
        String src = root + "/projects/bigproject/BigJavaFXProject.zip";
        String dst = Util.WORK_DIR;

        src = src.replace('\\', '/');
        dst = dst.replace('\\', '/');

        System.out.println("Project path: '" + src + "'");
        System.out.println("Project exist: " + new File(src).exists());



        URL url = new File(src).toURI().toURL();
        
        Util.unzipFile(url.toString(), dst + File.separator);

        String projectPath = dst + "/BigJavaFXProject";
        Project.openProject(projectPath);

        JavaFXProject bigProject  = new JavaFXProject("BigJavaFXProject");
        bigProject.build();


        boolean isCompiled = bigProject.getOutput().isCompiled();
        System.out.println("Otput:");
        System.out.println(bigProject.getOutput().getText());
        assertTrue("Big project is not compiled", isCompiled);
        
//        JavaFXProject profilerProject  = JavaFXProject.createProject("SmokeProfiler");
//        EditorOperator main = profilerProject.openMainFile();
//        String code = Util.getSampleText(Constant.SMOKE_PROFILER_FILE_PATH);
//        main.setText(code);
//        profilerProject.profile();
//
//
//        Util.sleep(4000);
        
    }
}
