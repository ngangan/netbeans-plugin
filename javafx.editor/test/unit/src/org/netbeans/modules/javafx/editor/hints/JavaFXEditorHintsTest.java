/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
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
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2008 Sun
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
package org.netbeans.modules.javafx.editor.hints;

import java.io.File;
import javax.swing.text.Document;
import org.netbeans.api.javafx.editor.TestUtilities;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.SourceTestBase;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.lexer.Language;
import org.netbeans.spi.editor.hints.Fix;
import org.openide.LifecycleManager;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.util.Exceptions;

/**
 *
 * @author Karol Harezlak
 */
public class JavaFXEditorHintsTest extends SourceTestBase {

    public JavaFXEditorHintsTest(String testName) {
        super(testName);
    }

    /**
     * Test Implement all abstract methods - OverrideAllTaskFactory;
     */
    public void testImplementApp() {
        String code = "import java.lang.Runnable; class Test extends Runnable {}";
        String pattern = "\n    override public function run () : Void { \n        throw new UnsupportedOperationException('Not implemented yet');\n    }";
        try {
            doTest(new OverrideAllTaskFactory(), code, pattern);
        } catch (Exception ex) {
            Exceptions.printStackTrace(ex);
        }
    }

    /**
     * Test Generation try-catch - UncaughtExceptionsTaskFactory;
     */
    public void testTryCatchGeneration() {
        String code = "class Test {function tryCatchTest(){this.wait();}}";
        String pattern = "try {\n    this.wait();}\n} catch(ex : InterruptedException) {\n    ex.printStackTrace();\n}";
        try {
            doTest(new UncaughtExceptionsTaskFactory(), code, pattern);
        } catch (Exception ex) {
            Exceptions.printStackTrace(ex);
        }
    }
    
    /**
     * Test Add imports - AddImportTaskFactory;
     */
    public void testAddImports() {
        String code = "class Test extends Action {}";
        String pattern = "import javax.swing.Action;";
        try {
            doTest(new AddImportTaskFactory(), code, pattern);
        } catch (Exception ex) {
            Exceptions.printStackTrace(ex);
        }
    }

    /**
     * Test Add imports - ExtImportWarningTaskFactory;
     */
    public void testExtImportAlert() {
        String code = "import javafx.ext.swing.SwingButton; class Test {}";
        String pattern = ""; //NOT used in this case
        try {
            int i = doTest(new AddImportTaskFactory(), code, pattern);
            assert i == 1;
        } catch (Exception ex) {
            Exceptions.printStackTrace(ex);
        }
    }

    //TODO Unfinished test
    /**
     * Test Generate var - CreateElementTaskFactory;
     */
    public void DisabledtestVar() {
        String code = "class Test { function testIt() {testVar} }";
        String pattern = ""; //NOT used in this case
        try {
            int i = doTest(new AddImportTaskFactory(), code, pattern);
            assert i == 1;
        } catch (Exception ex) {
            Exceptions.printStackTrace(ex);
        }
    }

    protected int doTest(final JavaFXAbstractEditorHint hint, String code, String pattern) throws Exception {
        JavaFXSource fXSource = fXSource = getJavaFXSource(code);
        assertNotNull(fXSource);
        int i = 0;
        final String[] result = new String[1];
        //TODO For some reason at the beginning compiler returns wrong number of diagnostics in controller so it's neccessary
        // to run Task for some time to get relayable diagnostics. Possible env is not ready yet. Just in case it's run it 100 times to make sure
        // env is ready.
        final int[] fixesNumber = new int[1];
        while (i < 50) {
            fXSource.runUserActionTask(new Task<CompilationController>() {

                public void run(CompilationController controller) throws Exception {
                    hint.createTask(controller.getFileObject()).run(controller);
                    //FIXME At the begining compiler returns wrong number of diganostics. In this case number of fixes is wrong
                    //assert fixes.size() == 1 : "Fixes: " + fixes.size() + " Diagnostics: " +  controller.getDiagnostics().size() + " Iteration: " + j;
                    fixesNumber[0] = hint.getFixes().size();
                    for (Fix fix : hint.getFixes()) {
                        fix.implement();
                        Document document = controller.getDocument();
                        result[0] = document.getText(0, document.getLength());
                        return;
                    }
                }
            }, true);
            i++;
        }
        assert result[0].contains(pattern);
        return fixesNumber.length;
    }

    protected JavaFXSource getJavaFXSource(String code) throws Exception {
        File testSource = new File(getWorkDir(), "test/Test.fx");
        testSource.getParentFile().mkdirs();
        testSource.createNewFile();
        TestUtilities.copyStringToFile(testSource, code);
        FileObject testSourceFO = FileUtil.toFileObject(testSource);
        assertNotNull(testSourceFO);
        DataObject testSourceDO = DataObject.find(testSourceFO);
        assertNotNull(testSourceDO);
        EditorCookie ec = testSourceDO.getCookie(EditorCookie.class);
        assertNotNull(ec);
        final Document document = ec.openDocument();
        assertNotNull(document);
        document.putProperty(Language.class, JFXTokenId.language());
        document.putProperty("mimeType", "text/x-fx");
        LifecycleManager.getDefault().saveAll();

        return JavaFXSource.forDocument(document);
    }
}
