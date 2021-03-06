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

package org.netbeans.lib.visage.lexer;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.netbeans.api.visage.lexer.VisageTokenId;

/**
 * Set of the tests around the processing the Visage Double Quote String Literals
 * via the VisageLexer. 
 * 
 * <p><b>Note:</b>JUnit 4 framework is required.</p>
 * 
 * @author Victor G. Vasilyev
 */
public class DoubleQuoteStringLiteralTest extends LexerTestBase {
    
    public DoubleQuoteStringLiteralTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    @Override
    public void setUp() {
        super.setUp();
    }

    @After 
    @Override
    public void tearDown() {
        super.tearDown();
    }
    
    // "He{"l{"l"}o"} world"
    @Test
    public void testHelloWorld() throws Exception {
        setSource("\"He{\"l{\"l\"}o\"} world\"");
        assertNextTokenIs(VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, "\"He{", 0);
        assertNextTokenIs(VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, "\"l{", 4);
        assertNextTokenIs(VisageTokenId.STRING_LITERAL, "\"l\"", 7);
        assertNextTokenIs(VisageTokenId.RBRACE_QUOTE_STRING_LITERAL, "}o\"", 10);
        assertNextTokenIs(VisageTokenId.RBRACE_QUOTE_STRING_LITERAL, "} world\"", 13);
    }

    @Test
    public void testRBRACE_QUOTE_STRING_LITERAL() throws Exception {
        setSource("\"{a}{b}\"");
        assertNextTokenIs(VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, "\"{", 0);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "a", 2);
        assertNextTokenIs(VisageTokenId.RBRACE_LBRACE_STRING_LITERAL, "}{", 3);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "b", 5);
        assertNextTokenIs(VisageTokenId.RBRACE_QUOTE_STRING_LITERAL, "}\"", 6);
    }

    @Test
    public void testRBRACE_QUOTE_STRING_LITERAL$ForeignQuote$1() throws Exception {
        setSource("\"'{a}{b}\"");
        assertNextTokenIs(VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, "\"'{", 0);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "a", 3);
        assertNextTokenIs(VisageTokenId.RBRACE_LBRACE_STRING_LITERAL, "}{", 4);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "b", 6);
        assertNextTokenIs(VisageTokenId.RBRACE_QUOTE_STRING_LITERAL, "}\"", 7);
    }

    @Test
    public void testRBRACE_QUOTE_STRING_LITERAL$ForeignQuote$2() throws Exception {
        setSource("\"''{a}{b}\"");
        assertNextTokenIs(VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, "\"''{", 0);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "a", 4);
        assertNextTokenIs(VisageTokenId.RBRACE_LBRACE_STRING_LITERAL, "}{", 5);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "b", 7);
        assertNextTokenIs(VisageTokenId.RBRACE_QUOTE_STRING_LITERAL, "}\"", 8);
    }

    @Test
    public void testRBRACE_QUOTE_STRING_LITERAL$ForeignQuote$3() throws Exception {
        setSource("\"{a}'$'{b}\"");
        assertNextTokenIs(VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, "\"{", 0);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "a", 2);
        assertNextTokenIs(VisageTokenId.RBRACE_LBRACE_STRING_LITERAL, "}'$'{", 3);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "b", 8);
        assertNextTokenIs(VisageTokenId.RBRACE_QUOTE_STRING_LITERAL, "}\"", 9);
    }

    @Test
    public void testRBRACE_QUOTE_STRING_LITERAL$ForeignQuote$4() throws Exception {
        setSource("\"{a}{b}'$'\"");
        assertNextTokenIs(VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, "\"{", 0);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "a", 2);
        assertNextTokenIs(VisageTokenId.RBRACE_LBRACE_STRING_LITERAL, "}{", 3);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "b", 5);
        assertNextTokenIs(VisageTokenId.RBRACE_QUOTE_STRING_LITERAL, "}'$'\"", 6);
    }

    @Test
    public void testRBRACE_QUOTE_STRING_LITERAL$Escape$LBRACE() throws Exception {
        setSource("\"{a}{b}\\{\"");
        assertNextTokenIs(VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, "\"{", 0);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "a", 2);
        assertNextTokenIs(VisageTokenId.RBRACE_LBRACE_STRING_LITERAL, "}{", 3);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "b", 5);
        assertNextTokenIs(VisageTokenId.RBRACE_QUOTE_STRING_LITERAL, "}\\{\"", 6);
    }

    @Test
    public void testDoubleQUOTE_LBRACE_STRING_LITERAL() throws Exception {
        setSource("\"{");
        assertNextTokenIs(VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, "\"{", 0);
    }

    @Test
    public void testDoubleQUOTE_LBRACE_STRING_LITERAL1() throws Exception {
        setSource("\" {");
        assertNextTokenIs(VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, "\" {", 0);
    }

    @Test
    public void testCodeBlock() throws Exception {
        setSource("\"{a}\"");
        assertNextTokenIs(VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, "\"{", 0);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "a", 2);
        assertNextTokenIs(VisageTokenId.RBRACE_QUOTE_STRING_LITERAL, "}\"", 3);
    }

    @Test
    public void testNewLineAndBraceAfterComplexString() throws Exception {
        setSource("\"{a}{b}\"\n}");
        assertNextTokenIs(VisageTokenId.QUOTE_LBRACE_STRING_LITERAL, "\"{", 0);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "a", 2);
        assertNextTokenIs(VisageTokenId.RBRACE_LBRACE_STRING_LITERAL, "}{", 3);
        assertNextTokenIs(VisageTokenId.IDENTIFIER, "b", 5);
        assertNextTokenIs(VisageTokenId.RBRACE_QUOTE_STRING_LITERAL, "}\"", 6);
        assertNextTokenIs(VisageTokenId.WS, "\n", 8);
        assertNextTokenIs(VisageTokenId.RBRACE, "}", 9);
    }
}