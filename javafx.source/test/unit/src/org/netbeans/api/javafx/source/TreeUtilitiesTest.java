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

package org.netbeans.api.javafx.source;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree;
import java.io.File;
import java.util.HashMap;
import java.util.Map;
import org.netbeans.api.javafx.source.JavaFXSource.Phase;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import static org.junit.Assert.*;

/**
 *
 * @author nenik
 */
public class TreeUtilitiesTest extends SourceTestBase {

    public TreeUtilitiesTest(String testName) {
        super(testName);
    }

    public void testIsSynthetic() throws Exception {
        System.out.println("isSynthetic");
        testInsideSourceTask(
                "import javafx.scene.shape.Circle;\n" +
                "Circle {\n" +
                "}",
          new Task<CompilationController>() {
            public void run(CompilationController controller) throws Exception {
                if (controller.toPhase(Phase.ANALYZED).lessThan(Phase.ANALYZED)) {
                    throw new Exception("Unable to resolve");
                }

                JavaFXTreePath currentPath = controller.getTreeUtilities().pathFor(42);
                assertFalse( "Tree " + currentPath.getLeaf() + " should not be synthetic.",
                        controller.getTreeUtilities().isSynthetic(currentPath));

                Tree tree = currentPath.getLeaf();
                System.err.println("Tree is: "+ tree);

                boolean result = controller.getTreeUtilities().isSynthetic(currentPath);
                assertFalse(result);
            }
        });
    }

    public void testPathFor() throws Exception {
        final Map<Integer, Tree.JavaFXKind> kindMap = new HashMap<Integer, Tree.JavaFXKind>();
        kindMap.put(0, Tree.JavaFXKind.CLASS_DECLARATION);
        kindMap.put(176, Tree.JavaFXKind.MEMBER_SELECT);
        kindMap.put(251, Tree.JavaFXKind.CLASS_DECLARATION);
        kindMap.put(261, Tree.JavaFXKind.CLASS_DECLARATION);
        kindMap.put(275, Tree.JavaFXKind.IDENTIFIER);
        kindMap.put(294, Tree.JavaFXKind.FUNCTION_DEFINITION);
        kindMap.put(320, Tree.JavaFXKind.FUNCTION_DEFINITION);
        kindMap.put(349, Tree.JavaFXKind.IDENTIFIER);
        kindMap.put(365, Tree.JavaFXKind.IDENTIFIER);
        kindMap.put(376, Tree.JavaFXKind.OBJECT_LITERAL_PART);
        kindMap.put(187, Tree.JavaFXKind.VARIABLE);
        kindMap.put(225, Tree.JavaFXKind.VARIABLE);
        kindMap.put(235, Tree.JavaFXKind.IDENTIFIER);
        kindMap.put(408, Tree.JavaFXKind.OBJECT_LITERAL_PART);
        kindMap.put(485, Tree.JavaFXKind.IDENTIFIER);
        kindMap.put(421, Tree.JavaFXKind.IDENTIFIER);
        
        System.out.println("pathFor(pos)");
        FileObject fo = FileUtil.toFileObject(getDataDir());
        fo = fo.getFileObject("org/netbeans/api/javafx/source/TreeUtilitiesTest.fx");
        testInsideSourceTask(fo,
          new Task<CompilationController>() {
            public void run(CompilationController controller) throws Exception {
                TreeUtilities tu = controller.getTreeUtilities();
                for(Map.Entry<Integer, Tree.JavaFXKind> entry : kindMap.entrySet()) {
                    JavaFXTreePath tp = tu.pathFor(entry.getKey());
                    assertNotNull(tp);
                    assertEquals("Wrong path @" + entry.getKey() + " " + tp.getLeaf(), entry.getValue(), tp.getLeaf().getJavaFXKind());
                }
            }
        });
    }
}