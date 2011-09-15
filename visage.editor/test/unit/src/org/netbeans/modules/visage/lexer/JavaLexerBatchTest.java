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

import org.netbeans.api.visage.lexer.VSGStringTokenId;
import org.netbeans.api.visage.lexer.VSGTokenId;
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

    // XXX: disabled now, fails because of VSGC-3602
    public void DISABLED_testComments() {
        String text = "/*ml-comment*//**//***//**\n*javadoc-comment*//* a";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VSGTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.COMMENT, "/*ml-comment*/");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.COMMENT, "/**/");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DOC_COMMENT, "/***/");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DOC_COMMENT, "/**\n*javadoc-comment*/");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.COMMENT, "/* a");
        assertEquals(PartType.START, ts.token().partType());
    }
    
    // XXX: disabled now, fails because of VSGC-3601
    public void DISABLED_testIdentifiers() {
        String text = "a ab aB2 2a x\nyZ\r\nz";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VSGTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "a");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "ab");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "aB2");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DECIMAL_LITERAL, "2");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "a");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "x");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, "\n");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "yZ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, "\r\n");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "z");
    }
    
    // XXX: disabled now, cf. issue #175442
    public void DISABLED_testStringLiterals() {
        // text = '"" "a""" "\"" "\\" "\\\"" "\n" "a'
        String text = "\"\" \"a\"\"\" \"\\\"\" \"\\\\\" \"\\\\\\\"\" \"\\n\" \"a";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VSGTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.STRING_LITERAL, "\"\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.STRING_LITERAL, "\"a\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.STRING_LITERAL, "\"\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.STRING_LITERAL, "\"\\\"\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.STRING_LITERAL, "\"\\\\\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.STRING_LITERAL, "\"\\\\\\\"\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.STRING_LITERAL, "\"\\n\"");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.UNKNOWN, "\"a");
        assertEquals(PartType.START, ts.token().partType());
    }

    // XXX: disabled now, cf. issue VSGC-3601
    public void DISABLED_testLongWhiteSpace() {
        String text = "    ";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VSGTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, "    ");
    }
    
    // XXX: disabled now, cf. issue #175442
    public void testNumberLiterals() {
        String text = "0 00 09 1 12 0x1 0xf 0XdE 0Xbcy" + 
                " 09.5 1.5 2.5 7e3 6.1E-7 0xa.5dp .3";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VSGTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DECIMAL_LITERAL, "0");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.OCTAL_LITERAL, "00");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.OCTAL_LITERAL, "09"); // invalid octal
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DECIMAL_LITERAL, "1");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DECIMAL_LITERAL, "12");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.HEX_LITERAL, "0x1");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.HEX_LITERAL, "0xf");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.HEX_LITERAL, "0XdE");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.HEX_LITERAL, "0Xbcy"); // out of range hex literal
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "y");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.OCTAL_LITERAL, "09.5"); // invalid octal
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FLOATING_POINT_LITERAL, "1.5");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FLOATING_POINT_LITERAL, "2.5");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FLOATING_POINT_LITERAL, "7e3");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FLOATING_POINT_LITERAL, "6.1E-7");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.HEX_LITERAL, "0xa.5dp");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FLOATING_POINT_LITERAL, ".3");
    }
    
    public void testOperators() {
        String text = "% %= * *= / /= = == and or not ";
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VSGTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.PERCENT, "%");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.PERCENTEQ, "%=");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.STAR, "*");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.STAREQ, "*=");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.SLASH, "/");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.SLASHEQ, "/=");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.EQ, "=");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.EQEQ, "==");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.AND, "and");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.OR, "or");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.NOT, "not");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
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
        
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VSGTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.ABSTRACT, "abstract");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.ASSERT, "assert");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.BOOLEAN, "boolean");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.BREAK, "break");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.BYTE, "byte");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.CASE, "case");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.BY, "by");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.CATCH, "catch");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.CHAR, "char");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.CLASS, "class");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.CONST, "const");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.CONTINUE, "continue");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DEFAULT, "default");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DISTINCT, "distinct");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DO, "do");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DURATION, "dur");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DOUBLE, "double");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.EASEBOTH, "easeboth");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.EASEIN, "easein");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.EASEOUT, "easeout");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.ELSE, "else");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.ENUM, "enum");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.EXTENDS, "extends");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FINAL, "final");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FINALLY, "finally");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FLOAT, "float");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FOR, "for");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FPS, "fps");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.GOTO, "goto");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IF, "if");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IMPLEMENTS, "implements");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IMPORT, "import");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.INSTANCEOF, "instanceof");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.INT, "int");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.INTERFACE, "interface");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.LONG, "long");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.NATIVE, "native");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.NEW, "new");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.PACKAGE, "package");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.PRIVATE, "private");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.PROTECTED, "protected");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.PUBLIC, "public");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.RETURN, "return");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.SHORT, "short");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.STATIC, "static");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.STRICTFP, "strictfp");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.SUPER, "super");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.SWITCH, "switch");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.SYNCHRONIZED, "synchronized");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.THIS, "this");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.THEN, "then");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.THROW, "throw");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.THROWS, "throws");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.TRANSIENT, "transient");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.TRY, "try");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.VOID, "void");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.VOLATILE, "volatile");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WHILE, "while");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.NULL, "null");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.TRUE, "true");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FALSE, "false");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.AFTER, "after"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.AS, "as");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.ATTRIBUTE, "attribute"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.BEFORE, "before"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.BIND, "bind"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DELETE, "delete");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FIRST, "first"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FOREACH, "foreach"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FROM, "from"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.FUNCTION, "function"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.INDEXOF, "indexof"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.INSERT, "insert"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IN, "in"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.INTO, "into"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.INVERSE, "inverse"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.LAST, "last"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.LATER, "later"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.LAZY, "lazy"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.LINEAR, "linear");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.NODEBUG, "nodebug"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.ON, "on");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.OPERATION, "operation"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.ORDER, "order");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.REVERSE, "reverse");     
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.SELECT, "select"); 
//        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.SIZEOF, "sizeof"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.TRIGGER, "trigger"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.TYPEOF, "typeof"); 
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.VAR, "var"); 
    }

    public void testNonKeywords() {
        String text = "abstracta assertx b br car dou doubl finall im i ifa inti throwsx";

        TokenHierarchy<?> hi = TokenHierarchy.create(text, VSGTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "abstracta");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "assertx");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "b");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "br");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "car");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "dou");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "doubl");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "finall");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "im");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "i");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "ifa");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "inti");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "throwsx");
    }
    
    // XXX: disabled now, cf. issue #175442
    public void testEmbedding() {
        String text = "ddx \"d\\t\\br\" /** @see X */";
        
        TokenHierarchy<?> hi = TokenHierarchy.create(text, VSGTokenId.language());
        TokenSequence<? extends TokenId> ts = (TokenSequence<? extends TokenId>)hi.tokenSequence();
        
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.IDENTIFIER, "ddx");
        assertEquals(0, ts.offset());
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        assertEquals(3, ts.offset());
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.STRING_LITERAL, "\"d\\t\\br\"");
        assertEquals(4, ts.offset());
        
        TokenSequence<? extends TokenId> es = (TokenSequence<? extends TokenId>)ts.embedded();
        assertNotNull(es);
        
        LexerTestUtilities.assertNextTokenEquals(es, VSGStringTokenId.TEXT, "d");
        assertEquals(5, es.offset());
        LexerTestUtilities.assertNextTokenEquals(es, VSGStringTokenId.TAB, "\\t");
        assertEquals(6, es.offset());
        LexerTestUtilities.assertNextTokenEquals(es, VSGStringTokenId.BACKSPACE, "\\b");
        assertEquals(8, es.offset());
        LexerTestUtilities.assertNextTokenEquals(es, VSGStringTokenId.TEXT, "r");
        assertEquals(10, es.offset());
                
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.WS, " ");
        assertEquals(12, ts.offset());
        LexerTestUtilities.assertNextTokenEquals(ts, VSGTokenId.DOC_COMMENT, "/** @see X */");
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
