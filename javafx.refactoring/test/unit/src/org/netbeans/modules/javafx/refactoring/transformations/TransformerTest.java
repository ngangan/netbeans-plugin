/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Sun Microsystems, Inc. All rights reserved.
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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
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

package org.netbeans.modules.javafx.refactoring.transformations;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class TransformerTest {
    final private String text = "import refactoringissues.i180546.M.*;\nimport refactoringissues.i180546.M;\n";

    private class MyTransformer extends Transformer {
        String transformed;
        String backup;

        public MyTransformer(String text) {
            super(text);
            this.backup = text;
        }

        @Override
        public Transformer newClone() {
            return new MyTransformer(backup);
        }

        @Override
        protected void saveTransformed(String transformed) {
            this.transformed = transformed;
        }
    }

    private MyTransformer transformer;

    public TransformerTest() {
    }

    @Before
    public void setUp() {
        transformer = new MyTransformer(text);
    }

    @After
    public void tearDown() {
        transformer = null;
    }

    @Test
    public void testReplaceDelete() throws Exception {
        transformer.addTransformation(new ReplaceTextTransformation(7, "refactoringissues.i180546", "refactoringissues.i180546.a"));
        transformer.addTransformation(new ReplaceTextTransformation(45, "refactoringissues.i180546", "refactoringissues.i180546.a"));
        transformer.addTransformation(new RemoveTextTransformation(0, 38));
        transformer.addTransformation(new RemoveTextTransformation(39, 36));

        transformer.transform();
        assertEquals("", transformer.transformed);
    }

}