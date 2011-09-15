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

package org.netbeans.api.visage.source;
import com.sun.visage.api.tree.VisageTreePath;
import com.sun.visage.api.tree.Tree;
import org.netbeans.api.visage.source.VisageSource.Phase;

/**
 *
 * @author alex
 */
public class PathFinderTest extends SourceTestBase {
    public PathFinderTest(String testName) {
        super(testName);
    }
    
    public void testPathFor() throws Exception {
        testInsideSourceTask(
                "/* Top comment */\n" +
                "\n" +
                "import visage.ui.*;\n" +
                "/** @author nemo */\n" +
                "\n" +
                "Frame {\n" +
                "  title: \"Hello World F3\"\n" +
                "  content: Label{\n"+
                "      text:\"Hello World\"\n}"+
                "}",
          new Task<CompilationController>() {
            public void run(CompilationController controller) throws Exception {
                if (controller.toPhase(Phase.ANALYZED).compareTo(Phase.ANALYZED) < 0) {//TODO: ELEMENTS_RESOLVED may be sufficient
                    throw new Exception(
                                "Unable to resolve "+controller.getCompilationUnit().getSourceFile()+" to phase "+Phase.ANALYZED+", current phase = "+controller.getPhase()+
                                "\nDiagnostics = "/*+ci.getDiagnostics()*/+
                                "\nFree memory = "+Runtime.getRuntime().freeMemory());
                }
                int currentOffset = 73;
                VisageTreePath currentPath = controller.getTreeUtilities().pathFor(currentOffset);
                Tree tree = currentPath.getLeaf();
                System.err.println("Tree is: "+ tree);
                assertNotNull(tree);
                // offset 73 falls in the middle of the "title:"
                assertEquals(Tree.VisageKind.OBJECT_LITERAL_PART, tree.getVisageKind());
            }
        });
    }
}
