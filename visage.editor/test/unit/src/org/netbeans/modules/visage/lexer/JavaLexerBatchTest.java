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

package org.netbeans.modules.visage.lexer;

import org.netbeans.api.visage.lexer.VisageStringTokenId;
import org.netbeans.api.visage.lexer.VisageTokenId;
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

    // XXX: disabled now, fails because of VisageC-3602
    public void DISABLED_testComments() {
        String text = "/*ml-comment*//**//***//**\n*javadoc-comment*//* a";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VisageTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.COMMENT, "/*ml-comment*/");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.COMMENT, "/**/");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DOC_COMMENT, "/***/");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DOC_COMMENT, "/**\n*javadoc-comment*/");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.COMMENT, "/* a");
        assertEquals(PartType.START, ts.token().partType());
    }
    
    // XXX: disabled now, fails because of VisageC-3601
    public void DISABLED_testIdentifiers() {
        String text = "a ab aB2 2a x\nyZ\r\nz";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VisageTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "a");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "ab");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "aB2");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DECIMAL_LITERAL, "2");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "a");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "x");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, "\n");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "yZ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, "\r\n");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "z");
    }
    
    // XXX: disabled now, cf. issue #175442
    public void DISABLED_testStringLiterals() {
        // text = '"" "a""" "\"" "\\" "\\\"" "\n" "a'
        String text = "\"\" \"a\"\"\" \"\\\"\" \"\\\\\" \"\\\\\\\"\" \"\\n\" \"a";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VisageTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.STRING_LITERAL, "\"\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.STRING_LITERAL, "\"a\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.STRING_LITERAL, "\"\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.STRING_LITERAL, "\"\\\"\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.STRING_LITERAL, "\"\\\\\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.STRING_LITERAL, "\"\\\\\\\"\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.STRING_LITERAL, "\"\\n\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.UNKNOWN, "\"a");
        assertEquals(PartType.START, ts.token().partType());
    }

    // XXX: disabled now, cf. issue VisageC-3601
    public void DISABLED_testLongWhiteSpace() {
        String text = "    ";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VisageTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, "    ");
    }
    
    // XXX: disabled now, cf. issue #175442
    public void testNumberLiterals() {
        String text = "0 00 09 1 12 0x1 0xf 0XdE 0Xbcy" + 
                " 09.5 1.5 2.5 7e3 6.1E-7 0xa.5dp .3";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VisageTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DECIMAL_LITERAL, "0");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.OCTAL_LITERAL, "00");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.OCTAL_LITERAL, "09"); // invalid octal
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DECIMAL_LITERAL, "1");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DECIMAL_LITERAL, "12");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.HEX_LITERAL, "0x1");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.HEX_LITERAL, "0xf");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.HEX_LITERAL, "0XdE");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.HEX_LITERAL, "0Xbcy"); // out of range hex literal
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "y");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.OCTAL_LITERAL, "09.5"); // invalid octal
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FLOATING_POINT_LITERAL, "1.5");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FLOATING_POINT_LITERAL, "2.5");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FLOATING_POINT_LITERAL, "7e3");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FLOATING_POINT_LITERAL, "6.1E-7");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.HEX_LITERAL, "0xa.5dp");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FLOATING_POINT_LITERAL, ".3");
    }
    
    public void testOperators() {
        String text = "% %= * *= / /= = == and or not ";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VisageTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.PERCENT, "%");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.PERCENTEQ, "%=");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.STAR, "*");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.STAREQ, "*=");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.SLASH, "/");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.SLASHEQ, "/=");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.EQ, "=");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.EQEQ, "==");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.AND, "and");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.OR, "or");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.NOT, "not");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
    }

    // XXX: disabled now, cf. issue #175442
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
            "else extends finally for if " +
            "import instanceof new package " +
            "private protected public return super " +
            "this then try while " +
            "null true false " + 
            "after as attribute before bind delete first from function " +
            "indexof insert in into inverse last lazy on " +
            "reverse sizeof trigger typeof var";
        
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VisageTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.ABSTRACT, "abstract");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.ASSERT, "assert");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.BOOLEAN, "boolean");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.BREAK, "break");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.BYTE, "byte");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.CASE, "case");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.BY, "by");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.CATCH, "catch");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.CHAR, "char");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.CLASS, "class");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.CONST, "const");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.CONTINUE, "continue");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DEFAULT, "default");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DISTINCT, "distinct");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DO, "do");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DURATION, "dur");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DOUBLE, "double");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.EASEBOTH, "easeboth");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.EASEIN, "easein");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.EASEOUT, "easeout");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.ELSE, "else");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.ENUM, "enum");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.EXTENDS, "extends");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FINAL, "final");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FINALLY, "finally");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FLOAT, "float");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FOR, "for");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FPS, "fps");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.GOTO, "goto");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IF, "if");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IMPLEMENTS, "implements");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IMPORT, "import");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.INSTANCEOF, "instanceof");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.INT, "int");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.INTERFACE, "interface");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.LONG, "long");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.NATIVE, "native");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.NEW, "new");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.PACKAGE, "package");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.PRIVATE, "private");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.PROTECTED, "protected");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.PUBLIC, "public");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.RETURN, "return");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.SHORT, "short");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.STATIC, "static");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.STRICTFP, "strictfp");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.SUPER, "super");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.SWITCH, "switch");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.SYNCHRONIZED, "synchronized");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.THIS, "this");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.THEN, "then");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.THROW, "throw");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.THROWS, "throws");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.TRANSIENT, "transient");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.TRY, "try");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.VOID, "void");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.VOLATILE, "volatile");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WHILE, "while");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.NULL, "null");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.TRUE, "true");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FALSE, "false");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.AFTER, "after"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.AS, "as");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.ATTRIBUTE, "attribute"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.BEFORE, "before"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.BIND, "bind"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DELETE, "delete");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FIRST, "first"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FOREACH, "foreach"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FROM, "from"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.FUNCTION, "function"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.INDEXOF, "indexof"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.INSERT, "insert"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IN, "in"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.INTO, "into"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.INVERSE, "inverse"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.LAST, "last"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.LATER, "later"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.LAZY, "lazy"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.LINEAR, "linear");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.NODEBUG, "nodebug"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.ON, "on");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.OPERATION, "operation"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.ORDER, "order");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.REVERSE, "reverse");     
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.SELECT, "select"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.SIZEOF, "sizeof"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.TRIGGER, "trigger"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.TYPEOF, "typeof"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.VAR, "var"); 
    }

    public void testNonKeywords() {
        String text = "abstracta assertx b br car dou doubl finall im i ifa inti throwsx";

        TokenHierarchy<?> hi = TokenHierarchy.create(text, VisageTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "abstracta");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "assertx");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "b");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "br");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "car");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "dou");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "doubl");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "finall");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "im");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "i");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "ifa");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "inti");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "throwsx");
    }
    
    // XXX: disabled now, cf. issue #175442
    public void testEmbedding() {
        String text = "ddx \"d\\t\\br\" /** @see X */";
        
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VisageTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.IDENTIFIER, "ddx");
        assertEquals(0, ts.offset());
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        assertEquals(3, ts.offset());
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.STRING_LITERAL, "\"d\\t\\br\"");
        assertEquals(4, ts.offset());
        
        TokenSequence<? extends TokenId> es = (TokenSequence<? extends TokenId>)ts.embedded();
        assertNotNull(es);
        
        LexerTestUtilities.assertNextTokenEquals(es, VisageStringTokenId.TEXT, "d");
        assertEquals(5, es.offset());
        LexerTestUtilities.assertNextTokenEquals(es, VisageStringTokenId.TAB, "\\t");
        assertEquals(6, es.offset());
        LexerTestUtilities.assertNextTokenEquals(es, VisageStringTokenId.BACKSPACE, "\\b");
        assertEquals(8, es.offset());
        LexerTestUtilities.assertNextTokenEquals(es, VisageStringTokenId.TEXT, "r");
        assertEquals(10, es.offset());
                
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.WS, " ");
        assertEquals(12, ts.offset());
        LexerTestUtilities.assertNextTokenEquals(ts, VisageTokenId.DOC_COMMENT, "/** @see X */");
        assertEquals(13, ts.offset());
        
        TokenSequence<? extends TokenId> ds = (TokenSequence<? extends TokenId>)ts.embedded();
//        assertNotNull(ds);
        
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
        
        assertFalse(ts.moveNext());
    }
}
