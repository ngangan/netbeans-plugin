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

import junit.framework.Test;
import org.netbeans.junit.NbModuleSuite;
import org.netbeans.spi.editor.completion.CompletionProvider;

/**
 *
 * @author David Strupl
 */
public class DeclarationCCTests extends CompletionTestPerformer {
    
    /** Creates a new instance of AllCCTests */
    public DeclarationCCTests(String name) {
        super(name);
    }

    /*
     * 
     * Declaration level tests
     * 
     */
    public void testFXTopLvlVar1() throws Exception {
        new CompletionTestCase(this).test(
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
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "declarations/TopLvlVar2.fx",
                8, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXAnimation001a() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "declarations/Animation001.fx",
                7, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXAnimation001b() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "declarations/Animation001.fx",
                10, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }
    
    public void testFXAnimation001c() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "fx-prj-1",
                "declarations/Animation001.fx",
                12, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

    public void testFXTopLvlVar3() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter,
                "var C6 = Color.rgb( ", // what should be typed in the editor
                false,
                getDataDir(),
                "fx-prj-1",
                "declarations/TopLvlVar3.fx",
                5, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);
    }

    public static Test suite() {
        return NbModuleSuite.create(
                NbModuleSuite.createConfiguration(DeclarationCCTests.class).enableModules(".*").clusters(".*"));
    }

}
