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
 * @author David Strupl
 */
public class ApplicationCCTests extends CompletionTestPerformer {

    /** Creates a new instance of AllCCTests */
    public ApplicationCCTests(String name) {
        super(name);
    }

    /*
     * 
     * Application level testing
     * 
     */
    public void testFXApplication1a() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "visage-prj-1",
                "applications/FXTestApplication1.visage",
                5, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

    public void testFXApplication1b() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "visage-prj-1",
                "applications/FXTestApplication1.visage",
                7, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

    public void testFXApplication1c() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "visage-prj-1",
                "applications/FXTestApplication1.visage",
                9, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

    public void testFXApplication1d() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter, 
                "", // what should be typed in the editor
                false, 
                getDataDir(),
                "visage-prj-1",
                "applications/FXTestApplication1.visage",
                14, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);        
    }

    public void testFXApplication02() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter,
                "for (num in ", // what should be typed in the editor
                false,
                getDataDir(),
                "visage-prj-1",
                "applications/FXTestApplication2.visage",
                5, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);
    }

    public void testFXApplication03() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter,
                "Timeline{}.", // what should be typed in the editor
                false,
                getDataDir(),
                "visage-prj-1",
                "applications/FXTestApplication3.visage",
                5, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);
    }

    public void testFXApplication04() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter,
                "", // what should be typed in the editor
                false,
                getDataDir(),
                "visage-prj-1",
                "applications/FXTestApplication4.visage",
                5, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);
    }

    public void testFXApplication05() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter,
                "", // what should be typed in the editor
                false,
                getDataDir(),
                "visage-prj-1",
                "applications/FXTestApplication5.visage",
                7, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);
    }

    public void testFXApplication06() throws Exception {
        new CompletionTestCase(this).test(
                outputWriter, logWriter,
                "style: ", // what should be typed in the editor
                false,
                getDataDir(),
                "visage-prj-1",
                "applications/FXTestApplication6.visage",
                19, // line number where the cursor should be
                CompletionProvider.COMPLETION_QUERY_TYPE);
    }

    /** For individual running of this class */
    public static Test suite() {
        return NbModuleSuite.create(
                NbModuleSuite.createConfiguration(ApplicationCCTests.class).enableModules(".*").clusters(".*"));
    }
    
}
