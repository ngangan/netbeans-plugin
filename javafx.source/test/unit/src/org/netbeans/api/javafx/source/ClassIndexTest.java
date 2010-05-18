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
 * Portions Copyrighted 2009 Sun Microsystems, Inc.
 */
package org.netbeans.api.javafx.source;

import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.javafx.source.ClassIndex.NameKind;
import org.netbeans.api.javafx.source.ClassIndex.SearchScope;

public class ClassIndexTest extends SourceTestBase {

    public ClassIndexTest(String testName) {
        super(testName);
    }

    @org.junit.Test
    public void testGetDeclaredTypes() throws Exception {
        indexPlatform();

        final Set<String> expectedTypes = new HashSet<String>();
        expectedTypes.addAll(Arrays.asList(new String[]{
            "com.sun.javafx.scene.control.caspian.ColorTransition",
            "com.sun.javafx.scene.paint.MultipleGradientPaint.ColorSpaceType",
            "com.sun.scenario.effect.ColorAdjust",
            "com.sun.javafx.tools.fxd.reflector.javafx.scene.effect.ColorAdjustReflector",
            "com.sun.javafx.tools.fxd.reflector.javafx.scene.paint.ColorReflector",
            "com.sun.stylesheet.types.ColorConverter",
            "javafx.scene.effect.ColorAdjust",
            "com.sun.scenario.effect.Color4f",
            "javafx.scene.paint.Color.ColorMap",
            "javafx.scene.paint.Color"
        }));
        final Set<ElementHandle<TypeElement>> types = new HashSet<ElementHandle<TypeElement>>();
        testInsideSourceTask("",
                new Task<CompilationController>() {
                    public void run(CompilationController controller) throws Exception {
                        JavaFXSource.Phase p = controller.toPhase(JavaFXSource.Phase.ANALYZED);
                        assertFalse(p.lessThan(JavaFXSource.Phase.ANALYZED));
                        types.addAll(controller.getClasspathInfo().getClassIndex().getDeclaredTypes(
                                "Color",
                                NameKind.PREFIX,
                                EnumSet.allOf(SearchScope.class)));
                    }
                });
        for (ElementHandle<TypeElement> type : types) {
            assertTrue("type provided", expectedTypes.remove(type.getQualifiedName()));
        }
        assertTrue("all types provided", expectedTypes.isEmpty());
    }
}
