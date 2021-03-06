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

import java.util.HashMap;
import java.util.Map;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import static org.junit.Assert.*;

/**
 *
 * @author nenik
 */
public class ElementUtilitiesTest extends SourceTestBase {

    public ElementUtilitiesTest(String testName) {
        super(testName);
    }

    public void testPathFor() throws Exception {
        final Map<Integer, ElementKind> kindMap = new HashMap<Integer, ElementKind>();
        kindMap.put(0, ElementKind.CLASS);
        kindMap.put(176, ElementKind.CLASS);
        kindMap.put(251, ElementKind.CLASS);
        kindMap.put(261, ElementKind.CLASS);
        kindMap.put(275, ElementKind.CLASS);
        kindMap.put(294, ElementKind.METHOD);
        kindMap.put(320, ElementKind.METHOD);
        kindMap.put(349, ElementKind.CLASS);
        kindMap.put(365, ElementKind.CLASS);
        kindMap.put(376, ElementKind.FIELD);
        kindMap.put(187, ElementKind.FIELD);
        kindMap.put(225, ElementKind.PARAMETER);
        kindMap.put(235, ElementKind.PARAMETER);
        kindMap.put(408, ElementKind.FIELD);
        kindMap.put(485, ElementKind.CLASS);
        kindMap.put(421, ElementKind.FIELD);
        
        System.out.println("pathFor(pos)");
        FileObject fo = FileUtil.toFileObject(getDataDir());
        fo = fo.getFileObject("org/netbeans/api/visage/source/TreeUtilitiesTest.fx");
        testInsideSourceTask(fo,
          new Task<CompilationController>() {
            public void run(CompilationController controller) throws Exception {
                ElementUtilities eu = controller.getElementUtilities();
                for(Map.Entry<Integer, ElementKind> entry : kindMap.entrySet()) {
                    Element e = eu.elementFor(entry.getKey());
                    assertEquals(entry.getValue(), e.getKind());

//                    assertNotNull(e);
                }
            }
        });
    }
}