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

package org.netbeans.test.visage.editor.completion;

import junit.framework.Test;
import org.netbeans.junit.NbModuleSuite;
import org.netbeans.spi.editor.completion.CompletionProvider;

/**
 *
 * @author Lark Fitzgerald
 */
public class ImportCCTests extends CompletionTestPerformer {
    
    /** Creates a new instance of AllCCTests */
    public ImportCCTests(String name) {
        super(name);
    }

    /*
     * 
     * Import Level Tests
     * 
     */
    public void testFXImport() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import ", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImport.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
// /*   Fails
    public void testFXImportJava() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import java.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJava.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportJava2() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter,
                "jav", // what should be typed in the editor
                false,
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportJava2.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);
    }

// /*   Fails
    public void testFXImportVisage() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisage.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportVisageAnimation() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.animation.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageAnimation.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

/*    No longer used, need to migrate to Stage.
    public void testFXImportVisageApplication() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.application.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageApplication.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
*/
// /*   Fails
    public void testFXImportVisageAsync() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.async.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageAsync.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportVisageExt() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.ext.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageExt.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

// /*   Fails
    public void testFXImportVisageExtSwing() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.ext.swing.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageExtSwing.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportVisageFXUnit() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.fxunit.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageFXUnit.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

/* package no longer exists
    public void testFXImportVisageInput() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.input.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageInput.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
*/

    public void testFXImportVisageLang() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.lang.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageLang.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

    public void testFXImportVisageReflect() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.reflect.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageReflect.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

// /*   Fails
    public void testFXImportVisageScene() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.scene.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageScene.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportVisageSceneEffect() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.scene.effect.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageSceneEffect.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportVisageSceneEffectLight() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.scene.effect.light.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageSceneEffectLight.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

/* No longer present, renamed to Shape.
    public void testFXImportVisageSceneGeometry() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.scene.geometry.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageSceneGeometry.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
 */
    
    public void testFXImportVisageSceneImage() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.scene.image.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageSceneImage.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportVisageSceneLayout() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.scene.layout.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageSceneLayout.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportVisageSceneMedia() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.scene.media.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageSceneMedia.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportVisageScenePaint() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.scene.paint.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageScenePaint.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
// /*   Fails
    public void testFXImportVisageSceneText() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.scene.text.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageSceneText.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportVisageSceneTransform() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.scene.transform.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageSceneTransform.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXImportVisageUtil() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "import visage.util.", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageUtil.fx",
                3, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

    public void testFXImportVisageS() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter,
                "import visage.s", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "imports/FXTestImportVisageS.fx",
                5, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);
    }

    public static Test suite() {
        return NbModuleSuite.create(
                NbModuleSuite.createConfiguration(ImportCCTests.class).enableModules(".*").clusters(".*"));
    }
    
}