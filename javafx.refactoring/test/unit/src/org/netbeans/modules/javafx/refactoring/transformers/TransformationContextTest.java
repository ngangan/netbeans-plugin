/*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 *  Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 * 
 *  The contents of this file are subject to the terms of either the GNU
 *  General Public License Version 2 only ("GPL") or the Common
 *  Development and Distribution License("CDDL") (collectively, the
 *  "License"). You may not use this file except in compliance with the
 *  License. You can obtain a copy of the License at
 *  http://www.netbeans.org/cddl-gplv2.html
 *  or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 *  specific language governing permissions and limitations under the
 *  License.  When distributing the software, include this License Header
 *  Notice in each file and include the License file at
 *  nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Sun in the GPL Version 2 section of the License file that
 *  accompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 * 
 *  Contributor(s):
 * 
 *  Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.refactoring.transformers;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.netbeans.modules.javafx.refactoring.impl.TransformationContext;
import static org.junit.Assert.*;

/**
 *
 * @author Jaroslav Bachorik
 */
public class TransformationContextTest {
    final private static String REF_TEXT = "one two three five";
    private StringBuilder content;

    private TransformationContext instance;

    public TransformationContextTest() {
    }

    @Before
    public void setUp() {
        instance = new TransformationContext();
        content = new StringBuilder(REF_TEXT);
    }

    @After
    public void tearDown() {
        instance = null;
        content = null;
    }

    /**
     * Test of replaceText method, of class TransformationContext.
     */
    @Test
    public void testReplaceText() {
        System.out.println("replaceText");
        int offset = 0;

        content.replace(instance.getRealOffset(offset), instance.getRealOffset(offset) + 3, "ones");
        instance.replaceText(offset, 3, 4);
        assertEquals("ones two three five", content.toString());

        content.replace(instance.getRealOffset(offset + 4), instance.getRealOffset(offset + 4) + 3, "twos");
        instance.replaceText(offset + 4, 3, 4);
        assertEquals("ones twos three five", content.toString());

        content.replace(instance.getRealOffset(offset + 14), instance.getRealOffset(offset + 14) + 4, "fives");
        instance.replaceText(offset + 14, 4, 5);
        assertEquals("ones twos three fives", content.toString());

        content.replace(instance.getRealOffset(offset + 14), instance.getRealOffset(offset + 14), "four ");
        instance.replaceText(offset + 14, 0, 5);
        assertEquals("ones twos three four fives", content.toString());

        content.replace(instance.getRealOffset(offset), instance.getRealOffset(offset), "zero ");
        instance.replaceText(offset, 0, 5);
        assertEquals("zero ones twos three four fives", content.toString());

        content.replace(instance.getRealOffset(offset + 8), instance.getRealOffset(offset + 8) + 6, "");
        instance.replaceText(offset + 8, 6, 0);
        assertEquals("zero ones twos four fives", content.toString());
    }
}