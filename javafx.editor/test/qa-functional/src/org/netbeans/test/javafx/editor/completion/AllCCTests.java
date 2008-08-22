/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
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

package org.netbeans.test.javafx.editor.completion;

import org.netbeans.spi.editor.completion.CompletionProvider;

/**
 *
 * @author David Strupl
 */
public class AllCCTests extends CompletionTestPerformer {
    
    /** Creates a new instance of AllCCTests */
    public AllCCTests(String name) {
        super(name);
    }


    /*
     * Baseline Smoke test
     */
    public void testFX1() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "fxprj1/FXTest1.fx",
                15, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

    /*
     * Import Level Tests
     */
    public void testFXImport() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import ", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImport.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
 /*   Fails
    public void testFXImportJava() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import java.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJava.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
*/
    
 /*   Fails
    public void testFXImportJavaFX() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFX.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
*/
    
    public void testFXImportJavaFXAnimation() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.animation.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXAnimation.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJavaFXApplication() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.application.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXApplication.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

// /*   Fails
    public void testFXImportJavaFXAsync() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.async.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXAsync.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJavaFXExt() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.ext.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXExt.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

// /*   Fails
    public void testFXImportJavaFXExtSwing() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.ext.swing.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXExtSwing.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJavaFXFXUnit() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.fxunit.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXFXUnit.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

    public void testFXImportJavaFXInput() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.input.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXInput.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

    public void testFXImportJavaFXLang() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.lang.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXLang.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

    public void testFXImportJavaFXReflect() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.reflect.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXReflect.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

// /*   Fails
    public void testFXImportJavaFXScene() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.scene.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXScene.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJavaFXSceneEffect() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.scene.effect.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXSceneEffect.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJavaFXSceneEffectLight() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.scene.effect.light.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXSceneEffectLight.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJavaFXSceneGeometry() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.scene.geometry.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXSceneGeometry.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJavaFXSceneImage() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.scene.image.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXSceneImage.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJavaFXSceneLayout() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.scene.layout.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXSceneLayout.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJavaFXSceneMedia() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.scene.media.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXSceneMedia.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJavaFXScenePaint() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.scene.paint.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXScenePaint.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
// /*   Fails
    public void testFXImportJavaFXSceneText() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.scene.text.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXSceneText.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJavaFXSceneTransform() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.scene.transform.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXSceneTransform.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJavaFXUtil() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "import javafx.util.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJavaFXUtil.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    /*
     * Package level Tests
     */
    public void testFXPackageEmpty() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "packages/FXTestEmpty.fx",
                1, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXPackage() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "packages/FXTestPackage.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    /*
     * Declaration level tests
     */
    public void testFXTopLvlVar1() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "var ", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "declarations/TopLvlVar1.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXTopLvlVar2() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "declarations/TopLvlVar2.fx",
                8, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    /*
     * Class level completion
     */
    public void testFXClass001() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "classes/FXTestClass001.fx",
                4, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXClass002a() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "classes/FXTestClass002.fx",
                9, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXClass002b() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "classes/FXTestClass002.fx",
                11, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXClass002c() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "classes/FXTestClass002.fx",
                21, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXClassPublic() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "public ", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "classes/FXTestClass001.fx",
                4, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
 /*   Fails
    public void testFXClassVar1() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "var a : ", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "classes/FXTestClassVar1.fx",
                4, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
  */
    
    /*
     * Frame Level Tests
     */
    public void testFXFrame001() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "frame/Frame001.fx",
                11, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    /*
     * Stage Level Tests
     */
    public void testFXStage() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "frame/stage/Stage001.fx",
                19, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXStageContent() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "frame/stage/Stage001.fx",
                21, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXStageContentArc1() throws Exception {
        new CompletionTest().test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "frame/stage/content/ContentArc1.fx",
                25, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
}
