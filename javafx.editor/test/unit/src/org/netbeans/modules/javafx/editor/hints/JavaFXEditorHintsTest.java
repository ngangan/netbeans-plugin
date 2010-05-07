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
import java.io.IOException;
import java.util.Collection;
import org.netbeans.spi.editor.hints.Severity;
import javax.swing.text.Document;
import org.netbeans.api.javafx.editor.TestUtilities;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.SourceTestBase;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.lexer.Language;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.netbeans.spi.editor.hints.Fix;
import org.openide.LifecycleManager;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.text.Annotation;
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
    public void testImplementAllAbstract() {
        String code = "import java.lang.Runnable; class Test extends Runnable{}";
        String pattern = "\n    override public function run () : Void { \n        throw new UnsupportedOperationException('Not implemented yet');\n    }";
        defaultTestCall(new OverrideAllTaskFactory(), code, pattern);
    }

    /**
     * Test Generation try-catch - UncaughtExceptionsTaskFactory;
     */
    public void testTryCatchGeneration() {
        String code = "class Test {function tryCatchTest(){this.wait();}}";
        String pattern = "try {\n    this.wait();}\n} catch(ex : InterruptedException) {\n    ex.printStackTrace();\n}";
        defaultTestCall(new UncaughtExceptionsTaskFactory(), code, pattern);
    }

    //FIXME This test does not work because broken indexing is broken in testing env.
    /**
     * Test Add imports - AddImportTaskFactory;
     */
    public void DISABLEDtestAddImports() {
        String code = "class Test extends List{}";
        String pattern = "import java.util.List;";
        defaultTestCall(new AddImportTaskFactory(), code, pattern);
    }

    /**
     * Test warnings - ExtImportWarningTaskFactory;
     */
    public void testExtImportAlert() {
        String code = "import javafx.ext.swing.SwingButton; class Test{}";
        //Starting env and creating functional hint.
        ExtImportWarningTaskFactory hint = new ExtImportWarningTaskFactory();
        try {
            doTest(hint, code, null);
        } catch (Exception ex) {
            Exceptions.printStackTrace(ex);
        }
        assert hint.getErrorDescriptions().size() == 1;
        Severity severity = hint.getErrorDescriptions().iterator().next().getSeverity();
        assert severity == Severity.WARNING;
    }

    //TODO SourcePosition problems, test doesn't work
    /**
     * Test Generate var - CreateElementTaskFactory;
     */
    public void DISABLEDtestVarGeneration() {
        String code = "class Test {function testFunction() {testVar}}";
        String pattern = "\n    var testVar;";
        defaultTestCall(new CreateElementTaskFactory(), code, pattern);
    }

    /**
     * Test Generate var Local - CreateElementTaskFactory;
     */
    public void testLocalVarGeneration() {
        String code = "class Test {function testFunction() {testVar}}";
        String pattern = "\nvar testVar;";
        defaultTestCall(new CreateElementTaskFactory(), code, pattern);
    }

    /**
     * Test Generate function - CreateElementTaskFactory;
     */
    public void testFunctionGeneration() {
        String code = "class Test {function testFunction() {testMethod()}}";
        String pattern = "\nfunction testMethod() {\n    throw new UnsupportedOperationException('Not implemented yet');\n}";
        defaultTestCall(new CreateElementTaskFactory(), code, pattern, CreateElementTaskFactory.Kind.FUNCTION);
    }

    /**
     * Test Generate class - CreateElementTaskFactory;
     */
    public void testLocalClassGeneration() {
        String code = "class Test {function testFunction() {TestClass{}}}";
        String pattern = "\nclass TestClass {\n    //TODO Not implemented yet.\n}";
        defaultTestCall(new CreateElementTaskFactory(), code, pattern, CreateElementTaskFactory.Kind.LOCAL_CLASS);
    }

    //FIXME JavaFX templates not avialiable in test env.
    /**
     * Test Generate class - CreateElementTaskFactory;
     */
    public void DISABLEDtestClassGeneration() {
        String code = "class Test {function testFunction() {TestClass{}}}";
        JavaFXAbstractEditorHint hint = new CreateElementTaskFactory();
        try {
            doTest(hint, code, CreateElementTaskFactory.Kind.CLASS);
        } catch (Exception ex) {
            Exceptions.printStackTrace(ex);
        }
        try {
            File file = getWorkDir();
        } catch (IOException ex) {
            Exceptions.printStackTrace(ex);
        }
    }

    /**
     * Test Mark Override annotations - MarkOverriddenTaskFactory;
     */
    public void testMarkOverride() {
        String code = "import java.lang.Runnable;import java.lang.UnsupportedOperationException;class Test extends Runnable {override public function run () : Void { throw new UnsupportedOperationException('Not implemented yet');} function method() {}}";
        MarkOverriddenTaskFactory hint = new MarkOverriddenTaskFactory();
        try {
            doTest(hint, code, null);
        } catch (Exception ex) {
            Exceptions.printStackTrace(ex);
        }
        Collection<? extends Annotation> annotations = hint.getAnnotations();
        assert annotations.size() == 1;
        for (Annotation annotation : annotations) {
            assert annotation.getAnnotationType().equals(MarkOverriddenTaskFactory.ANNOTATION_TYPE);
        }
    }

    public void defaultTestCall(JavaFXAbstractEditorHint hint, String code, String pattern) {
        defaultTestCall(hint, code, pattern, null);
    }

    public void defaultTestCall(JavaFXAbstractEditorHint hint, String code, String pattern, CreateElementTaskFactory.Kind kind) {
        try {
            String result = doTest(hint, code, kind);
            assertNotNull(result);
            assert result.contains(pattern);
        } catch (Exception ex) {
            Exceptions.printStackTrace(ex);
        }
    }

    protected String doTest(final JavaFXAbstractEditorHint hint, String code, final CreateElementTaskFactory.Kind kind) throws Exception {
        JavaFXSource fXSource = getJavaFXSource(code);
        assertNotNull(fXSource);

        final String[] result = new String[1];
        final int[] i = {0};
        Task task = new Task<CompilationController>() {

            public void run(final CompilationController controller) {
                try {
                    hint.createTask(controller.getFileObject()).run(controller);
                    for (final ErrorDescription errorDescription : hint.getErrorDescriptions()) {
                        for (final Fix fix : errorDescription.getFixes().getFixes()) {
                            Runnable runnable = new Runnable() {

                                public void run() {
                                    try {
                                        fix.implement();
                                    } catch (Exception e) {
                                        Exceptions.printStackTrace(e);
                                        assert false;
                                    }
                                }
                            };
                            if (kind == null || kind != null && fix.toString().contains(" " + kind.name() + " ")) {
                                HintsUtils.runInAWTandWait(runnable);
                            } else {
                                continue;
                            }
                            Document document = controller.getDocument();
                            result[0] = document.getText(0, document.getLength());
                            i[0] = 50;
                            return;
                        }
                    }
                } catch (Exception e) {
                    Exceptions.printStackTrace(e);
                    assert false;
                }

            }
        };
        //TODO For some reason at the beginning compiler returns wrong number of diagnostics in controller so it's neccessary
        // to run Task for some time to get relayable diagnostics. Possible env is not ready yet.
        while (i[0] < 50) {
            fXSource.runUserActionTask(task, true);
            i[0]++;
        }
        return result[0];
    }

    protected JavaFXSource getJavaFXSource(String code) throws Exception {
        File testSource = new File(getWorkDir(), "test/Test.fx");
        testSource.getParentFile().mkdirs();
        //testSource.createNewFile();
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
