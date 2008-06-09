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

package org.netbeans.modules.javafx.lexer;

import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.lexer.PartType;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.lexer.TokenId;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.lib.lexer.test.LexerTestUtilities;
import org.netbeans.junit.NbTestCase;

/**
 * Test several simple lexer impls.
 *
 * @author mmetelka
 */
public class JavaLexerBatchTest extends NbTestCase {

    public JavaLexerBatchTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws java.lang.Exception {
        // Set-up testing environment
        LexerTestUtilities.setTesting(true);
    }

    protected void tearDown() throws java.lang.Exception {
    }

    public void testComments() {
        String text = "/*ml-comment*//**//***//**\n*javadoc-comment*//* a";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, JFXTokenId.language());
        TokenSequence<? extends TokenId> ts = hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.COMMENT, "/*ml-comment*/");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.COMMENT, "/**/");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DOC_COMMENT, "/***/");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DOC_COMMENT, "/**\n*javadoc-comment*/");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.COMMENT, "/* a");
        assertEquals(PartType.START, ts.token().partType());
    }
    
    public void testIdentifiers() {
        String text = "a ab aB2 2a x\nyZ\r\nz";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, JFXTokenId.language());
        TokenSequence<? extends TokenId> ts = hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "a");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "ab");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "aB2");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "2");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "a");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "x");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, "\n");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "yZ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, "\r\n");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "z");
    }
    
    public void testStringLiterals() {
        String text = "\"\" \"a\"\"\" \"\\\"\" \"\\\\\" \"\\\\\\\"\" \"\\n\" \"a";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, JFXTokenId.language());
        TokenSequence<? extends TokenId> ts = hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STRING_LITERAL, "\"\"");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STRING_LITERAL, "\"a\"");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STRING_LITERAL, "\"\"");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STRING_LITERAL, "\"\\\"\"");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STRING_LITERAL, "\"\\\\\"");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STRING_LITERAL, "\"\\\\\\\"\"");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STRING_LITERAL, "\"\\n\"");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STRING_LITERAL, "\"a");
        assertEquals(PartType.START, ts.token().partType());
    }
    
    public void testNumberLiterals() {
        String text = "0 00 09 1 12 0L 1l 12L 0x1 0xf 0XdE 0Xbcy" + 
                " 09.5 1.5f 2.5d 6d 7e3 6.1E-7f 0xa.5dp+12d .3";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, JFXTokenId.language());
        TokenSequence<? extends TokenId> ts = hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "0");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "00");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "09");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "1");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "12");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "0L");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "1l");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "12L");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "0x1");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "0xf");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "0XdE");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DECIMAL_LITERAL, "0Xbc");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "y");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FLOATING_POINT_LITERAL, "09.5");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FLOATING_POINT_LITERAL, "1.5f");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FLOATING_POINT_LITERAL, "2.5d");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FLOATING_POINT_LITERAL, "6d");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FLOATING_POINT_LITERAL, "7e3");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FLOATING_POINT_LITERAL, "6.1E-7f");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FLOATING_POINT_LITERAL, "0xa.5dp+12d");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FLOATING_POINT_LITERAL, ".3");
    }
    
    public void testOperators() {
        String text = "% %= * *= / /= = == and or not ";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, JFXTokenId.language());
        TokenSequence<? extends TokenId> ts = hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.PERCENT, "%");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.PERCENTEQ, "%=");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STAR, "*");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STAREQ, "*=");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.SLASH, "/");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.SLASHEQ, "/=");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.EQ, "=");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.EQEQ, "==");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.AND, "and");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.OR, "or");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.NOT, "not");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
    }

    public void testKeywords() {
        /*String text = "abstract assert boolean break byte case catch char class const continue " +
            "default do dur double else enum extends final finally float for goto if " +
            "implements import instanceof int interface long native new package " +
            "private protected public return short static strictfp super switch " +
            "synchronized this throw throws transient try void volatile while " +
            "null true false " + 
            "after as attribute before bind delete first foreach from function " +
            "indexof insert in into inverse last later lazy nodebug on operation " +
            "reverse select sizeof trigger typeof var";*/
        String text = "assert break catch class continue " +
            "easeboth easein easeout else extends finally for fps if " +
            "import instanceof new package " +
            "private protected public return super " +
            "this then try while " +
            "null true false " + 
            "after as attribute before bind delete first foreach from function " +
            "indexof insert in into inverse last later lazy linear nodebug on operation " +
            "order reverse select sizeof trigger typeof var";
        
        TokenHierarchy<?> hi = TokenHierarchy.create(text, JFXTokenId.language());
        TokenSequence<? extends TokenId> ts = hi.tokenSequence();
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.ABSTRACT, "abstract");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.ASSERT, "assert");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.BOOLEAN, "boolean");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.BREAK, "break");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.BYTE, "byte");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.CASE, "case");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.BY, "by");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.CATCH, "catch");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.CHAR, "char");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.CLASS, "class");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.CONST, "const");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.CONTINUE, "continue");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DEFAULT, "default");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DISTINCT, "distinct");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DO, "do");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DURATION, "dur");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DOUBLE, "double");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.EASEBOTH, "easeboth");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.EASEIN, "easein");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.EASEOUT, "easeout");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.ELSE, "else");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.ENUM, "enum");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.EXTENDS, "extends");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FINAL, "final");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FINALLY, "finally");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FLOAT, "float");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FOR, "for");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FPS, "fps");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.GOTO, "goto");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IF, "if");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IMPLEMENTS, "implements");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IMPORT, "import");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.INSTANCEOF, "instanceof");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.INT, "int");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.INTERFACE, "interface");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.LONG, "long");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.NATIVE, "native");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.NEW, "new");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.PACKAGE, "package");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.PRIVATE, "private");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.PROTECTED, "protected");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.PUBLIC, "public");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.RETURN, "return");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.SHORT, "short");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STATIC, "static");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STRICTFP, "strictfp");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.SUPER, "super");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.SWITCH, "switch");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.SYNCHRONIZED, "synchronized");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.THIS, "this");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.THEN, "then");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.THROW, "throw");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.THROWS, "throws");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.TRANSIENT, "transient");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.TRY, "try");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.VOID, "void");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.VOLATILE, "volatile");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WHILE, "while");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.NULL, "null");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.TRUE, "true");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FALSE, "false");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.AFTER, "after"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.AS, "as");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.ATTRIBUTE, "attribute"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.BEFORE, "before"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.BIND, "bind"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DELETE, "delete");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FIRST, "first"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FOREACH, "foreach"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FROM, "from"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.FUNCTION, "function"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.INDEXOF, "indexof"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.INSERT, "insert"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IN, "in"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.INTO, "into"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.INVERSE, "inverse"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.LAST, "last"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.LATER, "later"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.LAZY, "lazy"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.LINEAR, "linear");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.NODEBUG, "nodebug"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.ON, "on");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.OPERATION, "operation"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.ORDER, "order");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.REVERSE, "reverse");     
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.SELECT, "select"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.SIZEOF, "sizeof"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.TRIGGER, "trigger"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.TYPEOF, "typeof"); 
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.VAR, "var"); 
    }

    public void testNonKeywords() {
        String text = "abstracta assertx b br car dou doubl finall im i ifa inti throwsx";

        TokenHierarchy<?> hi = TokenHierarchy.create(text, JFXTokenId.language());
        TokenSequence<? extends TokenId> ts = hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "abstracta");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "assertx");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "b");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "br");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "car");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "dou");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "doubl");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "finall");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "im");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "i");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "ifa");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "inti");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "throwsx");
    }
    
    public void testEmbedding() {
        String text = "ddx \"d\\t\\br\" /** @see X */";
        
        TokenHierarchy<?> hi = TokenHierarchy.create(text, JFXTokenId.language());
        TokenSequence<? extends TokenId> ts = hi.tokenSequence();
        
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.IDENTIFIER, "ddx");
        assertEquals(0, ts.offset());
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        assertEquals(3, ts.offset());
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.STRING_LITERAL, "\"d\\t\\br\"");
        assertEquals(4, ts.offset());
        
        TokenSequence<? extends TokenId> es = ts.embedded();
        
//        LexerTestUtilities.assertNextTokenEquals(es, JavaStringTokenId.TEXT, "d");
//        assertEquals(5, es.offset());
//        LexerTestUtilities.assertNextTokenEquals(es, JavaStringTokenId.TAB, "\\t");
//        assertEquals(6, es.offset());
//        LexerTestUtilities.assertNextTokenEquals(es, JavaStringTokenId.BACKSPACE, "\\b");
//        assertEquals(8, es.offset());
//        LexerTestUtilities.assertNextTokenEquals(es, JavaStringTokenId.TEXT, "r");
//        assertEquals(10, es.offset());
        
        assertFalse(es.moveNext());
        
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.WS, " ");
        assertEquals(12, ts.offset());
        LexerTestUtilities.assertNextTokenEquals(ts, JFXTokenId.DOC_COMMENT, "/** @see X */");
        assertEquals(13, ts.offset());
        
        TokenSequence<? extends TokenId> ds = ts.embedded();
        
//        LexerTestUtilities.assertNextTokenEquals(ds, JavadocTokenId.OTHER_TEXT, " ");
//        assertEquals(16, ds.offset());
//        LexerTestUtilities.assertNextTokenEquals(ds, JavadocTokenId.TAG, "@see");
//        assertEquals(17, ds.offset());
//        LexerTestUtilities.assertNextTokenEquals(ds, JavadocTokenId.OTHER_TEXT, " ");
//        assertEquals(21, ds.offset());
//        LexerTestUtilities.assertNextTokenEquals(ds, JavadocTokenId.IDENT, "X");
//        assertEquals(22, ds.offset());
//        LexerTestUtilities.assertNextTokenEquals(ds, JavadocTokenId.OTHER_TEXT, " ");
//        assertEquals(23, ds.offset());
        
        assertFalse(ds.moveNext());
        
        assertFalse(ts.moveNext());
    }
}
