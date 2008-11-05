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

package org.netbeans.test.javafx.bestpractices.samples;

/**
 *
 * @author Lark
 */
import junit.framework.Test;
import org.netbeans.jellytools.OutputOperator;
import org.netbeans.junit.NbModuleSuite;
import org.netbeans.test.javafx.bestpractices.lib.JavaFXTestCase;
import org.netbeans.test.javafx.bestpractices.lib.Util;

public class ShapePrimitives extends JavaFXTestCase {

    public String _type = "Shapes"; //Type of Sample
    public String _project = "Shape Primitives";
    public String _projectPath = "Shape Primitives|Source Packages|shapes|ShapePrimitives.fx";
    static String[] TESTS = {"testShapePrimitivesCreate", "testShapePrimitivesCompile", "testShapePrimitivesCloseProject"};
    
    public ShapePrimitives(String name) {
        super(name);
    }

    /** Create Sample Project and Verify that Project exists */
    public void testShapePrimitivesCreate() {
        if ((Util.createSampleProject(SAMPLE_PATH, _type, _project, this.getWorkDirAsString())).equals(false)) {
            fail("Project " + _project + " was not found.");
        }
    }

    /** Compile Single File and Verify Success */
    public void testShapePrimitivesCompile() {
        if (Util.compileProject(_project).equals(false)) {
            fail("Build Failed to compile: " + new OutputOperator().getText());
        }
    }

    /** Close Sample Project and Output window*/
    public void testShapePrimitivesCloseProject() {
        if (Util.closeProject(_project).equals(false)) {
            fail("Project " + _project + " did not close properly.");
        }
    }

    public static Test suite() {
        return NbModuleSuite.create(ShapePrimitives.class, ".*", ".*", TESTS);
    }
}

