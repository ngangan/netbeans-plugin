/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
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
package org.netbeans.modules.javafx.editor.format;

import java.io.File;
import java.util.prefs.Preferences;
import javax.swing.text.Document;
import org.netbeans.api.editor.mimelookup.MimeLookup;
import org.netbeans.api.java.lexer.JavaTokenId;
import org.netbeans.api.javafx.editor.TestUtilities;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.lexer.Language;
import org.netbeans.junit.NbTestCase;
import org.netbeans.modules.editor.indent.api.Reformat;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;

/**
 *
 * @author Anton Chechel
 */
public class FormattingTest extends NbTestCase {

    private File testFile;

    public FormattingTest(String testName) {
        super(testName);
    }

    public void testClass() {
        // TODO
    }

    public void testImport() {
        // TODO
    }

    // XXX: disabled now, cf. issue #175439
    public void DISABLED_testVariable() throws Exception {
        // TODO move initialization part to separate method
        testFile = new File(getWorkDir(), "Test.fx");
        TestUtilities.copyStringToFile(testFile,
                "package org.netbeans.javafx.test;\n\n" +
                "public class Test {\n" +
                "   public def var1 : String = \"abc\";\n" +
                "}\n");

        FileObject testFileFO = FileUtil.toFileObject(testFile);
        assertNotNull(testFileFO);
        DataObject testSourceDO = DataObject.find(testFileFO);
        assertNotNull(testSourceDO);
        EditorCookie ec = (EditorCookie) testSourceDO.getCookie(EditorCookie.class);
        assertNotNull(ec);
        final Document doc = ec.openDocument();
        assertNotNull(doc);
        doc.putProperty(Language.class, JFXTokenId.language());
        doc.putProperty("mimeType", "text/x-fx");

        JavaFXSource src = JavaFXSource.forDocument(doc);
        src.runUserActionTask(new Task<CompilationController>() {

            public void run(CompilationController controller) throws Exception {
                assertFalse(controller.toPhase(JavaFXSource.Phase.PARSED).lessThan(JavaFXSource.Phase.PARSED));
            }
        }, true);

        // TODO use javafx preferences when will be available
        Preferences preferences = MimeLookup.getLookup(JavaTokenId.language().mimeType()).lookup(Preferences.class);
//        Preferences preferences = MimeLookup.getLookup(JFXTokenId.language().mimeType()).lookup(Preferences.class);
        assertNotNull(preferences);
        preferences.putBoolean("placeNewLineAfterModifiers", false);
        preferences.putBoolean("spaceAroundAssignOps", true);
        preferences.putBoolean("wrapAssignOps", false);
        preferences.putBoolean("alignMultilineAssignment", false);

        ec.saveDocument();
        String res = TestUtilities.copyFileToString(testFile);
        System.err.println(res);

        String golden =
                "package org.netbeans.javafx.test;\n\n" +
                "public class Test {\n" +
                "   public def var1 : String = \"abc\";\n" +
                "}\n";
        reformat(doc, res, golden);
    }

    public void testMethod() {
        // TODO
    }

    private void reformat(Document doc, String content, String golden) throws Exception {
        reformat(doc, content, golden, 0, content.length());
    }

    private void reformat(Document doc, String content, String golden, int startOffset, int endOffset) throws Exception {
        doc.remove(0, doc.getLength());
        doc.insertString(0, content, null);

        Reformat reformat = Reformat.get(doc);
        assertNotNull(reformat);
        reformat.lock();
        try {
            reformat.reformat(startOffset, endOffset);
        } finally {
            reformat.unlock();
        }
        String res = doc.getText(0, doc.getLength());
        System.err.println(res);
        assertEquals(golden, res);
    }
}
