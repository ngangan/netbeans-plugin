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

package org.netbeans.api.javafx.lexer;

import org.netbeans.api.java.lexer.JavadocTokenId;
import org.netbeans.api.lexer.*;
import org.netbeans.lib.javafx.lexer.JFXLexer;
import org.netbeans.spi.lexer.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Rastislav Komara (<a href="mailto:rastislav.komara@sun.com">RKo</a>)
 * @todo documentation
 */
public enum JFXTokenId implements TokenId {
/* v4Lexer tokens. */
	LAZY("keyword", 39), //NOI18N
	COMMA("separator", 85),//NOI18N
	DEF("keyword", 18),//NOI18N
	AS("keyword", 7),//NOI18N
	NOTEQ("operator", 104),//NOI18N
	INTO("keyword", 36),//NOI18N
	TranslationKeyBody("operator", 119),//NOI18N
	FALSE("keyword-literal", 23),//NOI18N
	ABSTRACT("keyword", 4),//NOI18N
	THEN("keyword", 64),//NOI18N
	STEP("keyword", 62),//NOI18N
	PLUSPLUS("operator", 80),//NOI18N
	IMPORT("keyword", 30),//NOI18N
	PACKAGE("keyword", 48),//NOI18N
	SIZEOF("keyword", 60),//NOI18N
	PIPE("operator", 79),//NOI18N
	CONTINUE("keyword-directive", 17),//NOI18N
	ON("keyword", 45),//NOI18N
	DOT("separator", 86),//NOI18N
	SingleQuoteBody("string", 110),//NOI18N
	PRIVATE("keyword", 50),//NOI18N
	Letter("identifier", 128),//NOI18N
	AND("keyword", 6),//NOI18N
	FUNCTION("keyword", 28),//NOI18N
	TRIGGER("keyword", 67),//NOI18N
	STRING_LITERAL("string", 111),//NOI18N
	RBRACKET("separator", 83),//NOI18N
	RPAREN("separator", 82),//NOI18N
	RBRACE_LBRACE_STRING_LITERAL("string", 116),//NOI18N
	ASSERT("keyword-directive", 8),//NOI18N
	PLUS("operator", 94),//NOI18N
	FINALLY("keyword-directive", 24),//NOI18N
	EXTENDS("keyword", 22),//NOI18N
	AT("keyword", 9),//NOI18N
	PUBLIC_READABLE("keyword", 54),//NOI18N
	TIME_LITERAL("time", 121),//NOI18N
	SUPER("keyword", 63),//NOI18N
	DECIMAL_LITERAL("number", 122),//NOI18N
	WS("whitespace", 131),//NOI18N
	SUBSUB("operator", 108),//NOI18N
	NEW("keyword", 41),//NOI18N
	PUBLIC_READ("keyword", 55),//NOI18N
	EQ("operator", 88),//NOI18N
	EXCLUSIVE("keyword", 21),//NOI18N
	LT("operator", 90),//NOI18N
	BOUND("keyword", 13),//NOI18N
	LINE_COMMENT("comment", 134), // NOI18N
	EQEQ("operator", 87), // NOI18N
	QUOTE_LBRACE_STRING_LITERAL("string", 113), // NOI18N
	FLOATING_POINT_LITERAL("number", 127), // NOI18N
	CATCH("keyword-directive", 15), // NOI18N
	STATIC("keyword", 61), // NOI18N
	SEMI("separator", 84), // NOI18N
	ELSE("keyword-directive", 20), // NOI18N
	INDEXOF("keyword", 31), // NOI18N
	FORMAT_STRING_LITERAL("format", 118), // NOI18N
	LTEQ("operator", 92), // NOI18N
	FIRST("keyword", 25), // NOI18N
	BREAK("keyword-directive", 14), // NOI18N
	NULL("keyword-literal", 44), // NOI18N
	QUES("operator", 106), // NOI18N
	COLON("operator", 105), // NOI18N
	DOTDOT("operator", 81), // NOI18N
	IDENTIFIER("identifier", 130), // NOI18N
	NextIsPercent("string", 112), // NOI18N
	INSERT("keyword", 34), // NOI18N
	TRUE("keyword-literal", 68), // NOI18N
	DOC_COMMENT("comment", 133), // NOI18N
	POUND("operator", 78), // NOI18N
	POSTINIT("keyword", 49), // NOI18N
	THROW("keyword-directive", 66), // NOI18N
	WHERE("keyword", 73), // NOI18N
	PUBLIC("keyword", 53), // NOI18N
	LTGT("operator", 91), // NOI18N
	PERCENT("operator", 98), // NOI18N
	TYPEOF("keyword", 71), // NOI18N
	LAST("keyword", 38), // NOI18N
	LBRACKET("separator", 76), // NOI18N
	MOD("keyword", 40), // NOI18N
	INIT("keyword", 33), // NOI18N
	OCTAL_LITERAL("number", 123), // NOI18N
	HEX_LITERAL("number", 124), // NOI18N
	OR("keyword", 46), // NOI18N
	LBRACE("separator", 114), // NOI18N
	AFTER("keyword", 5), // NOI18N
	RBRACE("separator", 117), // NOI18N
	PROTECTED("keyword", 51), // NOI18N
	INVERSE("keyword", 37), // NOI18N
	SUBEQ("operator", 100), // NOI18N
	INSTANCEOF("keyword", 35), // NOI18N
	TRANSLATION_KEY("i18n-artifact", 120), // NOI18N
	LPAREN("separator", 77), // NOI18N
	DoubleQuoteBody("string", 109), // NOI18N
	SLASHEQ("operator", 102), // NOI18N
	FROM("keyword", 27), // NOI18N
	PERCENTEQ("operator", 103), // NOI18N
	DELETE("keyword", 19), // NOI18N
	Exponent("number", 126), // NOI18N
	SLASH("operator", 97), // NOI18N
	WHILE("keyword-directive", 74), // NOI18N
	STAREQ("operator", 101), // NOI18N
	READABLE("keyword", 56), // NOI18N
	PLUSEQ("operator", 99), // NOI18N
	PUBLIC_INIT("keyword", 52), // NOI18N
	REPLACE("keyword", 57), // NOI18N
	GT("operator", 89), // NOI18N
	COMMENT("comment", 132), // NOI18N
	OVERRIDE("keyword", 47), // NOI18N
	GTEQ("operator", 93), // NOI18N
	THIS("keyword", 65), // NOI18N
	WITH("keyword", 75), // NOI18N
	IN("keyword", 32), // NOI18N
	REVERSE("keyword", 59), // NOI18N
	INVALIDC("keyword", 136), // NOI18N
	JavaIDDigit("identifier", 129), // NOI18N
	VAR("keyword", 72), // NOI18N
	CLASS("keyword", 16), // NOI18N
	TWEEN("keyword", 70), // NOI18N
	RETURN("keyword-directive", 58), // NOI18N
	IF("keyword-directive", 29), // NOI18N
	SUCHTHAT("operator", 107), // NOI18N
	FOR("keyword-directive", 26), // NOI18N
	LAST_TOKEN("future-literal", 135), // NOI18N
	NON_WRITABLE("keyword", 42), // NOI18N
	BEFORE("keyword", 11), // NOI18N
	STAR("operator", 96), // NOI18N
	ATTRIBUTE("keyword", 10), // NOI18N
	SUB("operator", 95), // NOI18N
	BIND("keyword", 12), // NOI18N
	Digits("number", 125), // NOI18N
	NOT("keyword", 43), // NOI18N
	TRY("keyword-directive", 69), // NOI18N
	RBRACE_QUOTE_STRING_LITERAL("string", 115), // NOI18N
    UNKNOWN("error", 200); // NOI18N

    /*v3.g lexer
    EXPR_LIST("error", 120),
	LAZY("keyword", 69),
	COMMA("separator", 83),
	SEQ_INDEX("error", 125),
	AS("keyword", 55),
	HexDigit("error", 158),
	SEQ_SLICE_EXCLUSIVE("error", 127),
	NOTEQ("operator", 102),
	INTO("keyword", 66),
	TranslationKeyBody("operator", 151),
	FALSE("keyword-literal", 15),
	ABSTRACT("keyword", 5),
	THEN("keyword", 75),
	STEP("keyword", 74),
	PLUSPLUS("operator", 49),
	IMPORT("keyword", 19),
	PACKAGE("keyword", 28),
	SIZEOF("keyword", 37),
	PIPE("operator", 51),
	CONTINUE("keyword-directive", 13),
	ON("keyword", 71),
	DOT("separator", 84),
	SingleQuoteBody("string", 142),
	PRIVATE("keyword", 30),
	Letter("identifier", 162),
	TYPED_ARG_LIST("error", 138),
	EXPRESSION("error", 114),
	AND("keyword", 54),
	FUNCTION("keyword", 17),
	TRIGGER("keyword", 76),
	STRING_LITERAL("string", 143),
	EMPTY_MODULE_ITEM("error", 108),
	MODULE("error", 107),
	RBRACKET("separator", 81),
	RPAREN("separator", 80),
	SEMI_INSERT_START("error", 4),
	RBRACE_LBRACE_STRING_LITERAL("string", 148),
	ASSERT("keyword-directive", 6),
	PLUS("operator", 92),
	OBJECT_LIT("error", 128),
	ON_REPLACE("error", 119),
	FINALLY("keyword-directive", 61),
	EXTENDS("keyword", 60),
	AT("keyword", 7),
	TIME_LITERAL("time", 156),
	SUPER("keyword", 36),
	DECIMAL_LITERAL("number", 153),
	SLICE_CLAUSE("error", 117),
	WS("whitespace", 165),
	NEW("keyword", 24),
	SUBSUB("operator", 50),
	EQ("operator", 86),
	FUNC_EXPR("error", 112),
	EXCLUSIVE("keyword", 59),
	LT("operator", 88),
	BOUND("keyword", 10),
	LINE_COMMENT("comment", 167),
	RangeDots("error", 160),
	NEGATIVE("error", 122),
	EQEQ("operator", 85),
	QUOTE_LBRACE_STRING_LITERAL("string", 145),
	FLOATING_POINT_LITERAL("number", 161),
	TYPE_ANY("error", 135),
	STATIC("keyword", 38),
	CATCH("keyword-directive", 57),
	SEMI("separator", 82),
	ELSE("keyword-directive", 58),
	INDEXOF("keyword", 20),
	FORMAT_STRING_LITERAL("format", 150),
	LTEQ("operator", 90),
	BREAK("keyword-directive", 11),
	FIRST("keyword", 62),
	NULL("keyword-literal", 26),
	QUES("operator", 104),
	COLON("operator", 103),
	DOTDOT("operator", 79),
	IDENTIFIER("identifier", 164),
	NextIsPercent("string", 144),
	TYPE_UNKNOWN("error", 136),
	INSERT("keyword", 22),
	TRUE("keyword-literal", 42),
	DOC_COMMENT("comment", 139),
	POUND("operator", 46),
	THROW("keyword-directive", 40),
	POSTINIT("keyword", 29),
	WHERE("keyword", 78),
	POSTINCR("error", 123),
	OBJECT_LIT_PART("error", 129),
	PUBLIC("keyword", 32),
	LTGT("operator", 89),
	STATEMENT("error", 113),
	PERCENT("operator", 96),
	TYPEOF("keyword", 43),
	LAST("keyword", 68),
	SEQ_EMPTY("error", 130),
	READONLY("error", 33),
	LBRACKET("separator", 48),
	INIT("keyword", 21),
	MOD("keyword", 70),
	OCTAL_LITERAL("number", 157),
	SEQ_SLICE("error", 126),
	FUNC_APPLY("error", 121),
	HEX_LITERAL("number", 159),
	OR("keyword", 72),
	LBRACE("separator", 146),
	AFTER("keyword", 53),
	RBRACE("separator", 149),
	BLOCK("error", 115),
	EMPTY_FORMAT_STRING("error", 132),
	PROTECTED("keyword", 31),
	INVERSE("keyword", 67),
	TYPE_NAMED("error", 133),
	SUBEQ("operator", 98),
	POSTDECR("error", 124),
	INSTANCEOF("keyword", 65),
	TRANSLATION_KEY("i18n-artifact", 152),
	PARAM("error", 111),
	ON_REPLACE_SLICE("error", 118),
	LPAREN("separator", 47),
	DoubleQuoteBody("string", 141),
	SLASHEQ("operator", 100),
	FROM("keyword", 63),
	PERCENTEQ("operator", 101),
	DELETE("keyword", 14),
	Exponent("number", 155),
	SLASH("operator", 95),
	WHILE("keyword-directive", 45),
	STAREQ("operator", 99),
	CLASS_MEMBERS("error", 110),
	PLUSEQ("operator", 97),
	REPLACE("keyword", 73),
	GT("operator", 87),
	COMMENT("comment", 166),
	OVERRIDE("keyword", 27),
	GTEQ("operator", 91),
	SEQ_EXPLICIT("error", 131),
	THIS("keyword", 39),
	WITH("keyword", 77),
	REVERSE("keyword", 35),
	IN("keyword", 64),
	JavaIDDigit("identifier", 163),
	VAR("keyword", 44),
	CLASS("keyword", 12),
	TWEEN("keyword", 105),
	RETURN("keyword-directive", 34),
	LET("error", 23),
	IF("keyword-directive", 18),
	SUCHTHAT("operator", 106),
	SEMI_INSERT_END("error", 52),
	TYPE_FUNCTION("error", 134),
	FOR("keyword-directive", 16),
	LAST_TOKEN("future-literal", 168),
	BEFORE("keyword", 56),
	ATTR_INTERPOLATE("error", 140),
	MISSING_NAME("error", 116),
	STAR("operator", 94),
	ATTRIBUTE("keyword", 8),
	MODIFIER("error", 109),
	SUB("operator", 93),
	BIND("keyword", 9),
	Digits("number", 154),
	TYPE_ARG("error", 137),
	TRY("keyword-directive", 41),
	NOT("keyword", 25),
	RBRACE_QUOTE_STRING_LITERAL("string", 147),
    UNKNOWN("error", 200);                      */

    public static final String UNIVERSAL_CATEGORY = "future-literal"; // NOI18N
    private final String primaryCategory;
    private final int tokenType;
    private static JFXTokenId[] typeToId;

    static {
        try {
            final JFXTokenId[] tokenIds = JFXTokenId.values();
            Arrays.sort(tokenIds, new Comparator<JFXTokenId>() {
                public int compare(JFXTokenId o1, JFXTokenId o2) {
                    return o1.getTokenType() - o2.getTokenType();
                }
            });
            final JFXTokenId tid = tokenIds[tokenIds.length - 1];
            final int type = tid.getTokenType();
            typeToId = new JFXTokenId[type + 2];
            for (JFXTokenId jfxTokenId : tokenIds) {
                if (jfxTokenId.getTokenType() > 0) {
                    typeToId[jfxTokenId.getTokenType()] = jfxTokenId;
                }
            }
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }


    public static JFXTokenId getId(int tokenType) {
        if (tokenType >= 0 && tokenType < typeToId.length) {
            return typeToId[tokenType];
        } else {
            return UNKNOWN;
        }
    }

    JFXTokenId(String primaryCategory, int tokenType) {
        this.primaryCategory = primaryCategory;
        this.tokenType = tokenType;
    }

    @Override
    public String toString() {
        return super.toString();
    }

    public int getTokenType() {
        return tokenType;
    }

    public String primaryCategory() {
        return primaryCategory != null ? primaryCategory : UNIVERSAL_CATEGORY;
    }


    private static final Language<JFXTokenId> language = new LanguageHierarchy<JFXTokenId>() {
        private Logger log = Logger.getLogger(JFXTokenId.class.getName());
//        JFXLexer lexer;

        protected Collection<JFXTokenId> createTokenIds() {
            return Arrays.asList(JFXTokenId.values());
        }

        protected Lexer<JFXTokenId> createLexer(LexerRestartInfo<JFXTokenId> info) {
            /*if (lexer == null) {
                lexer = new JFXLexer();
            }
            try {
                lexer.restart(info);
            } catch (IOException e) {
                e.printStackTrace();
            }*/
            try {
                return new JFXLexer(info);
            } catch (IOException e) {
                if (log.isLoggable(Level.SEVERE)) log.severe("Cannot create lexer.\n" + e); // NOI18N
                throw new IllegalStateException(e);
            }
        }

        @Override
        protected String mimeType() {
            return "text/x-fx";
        }

        @Override
        protected LanguageEmbedding<?> embedding(Token<JFXTokenId> token, LanguagePath languagePath, InputAttributes inputAttributes) {
            switch (token.id()) {
                case DOC_COMMENT:
                    return LanguageEmbedding.create(JavadocTokenId.language(), 3,
                                (token.partType() == PartType.COMPLETE) ? 2 : 0);
                case QUOTE_LBRACE_STRING_LITERAL:
                    return LanguageEmbedding.create(JFXStringTokenId.language(), 1, 0);
                case RBRACE_QUOTE_STRING_LITERAL:
                    return LanguageEmbedding.create(JFXStringTokenId.language(), 0, 1);
                case RBRACE_LBRACE_STRING_LITERAL:
                    return LanguageEmbedding.create(JFXStringTokenId.language(), 0, 0);
                case DoubleQuoteBody:
                case SingleQuoteBody:
                case STRING_LITERAL:
                    return LanguageEmbedding.create(JFXStringTokenId.language(true), 1,
                            (token.partType() == PartType.COMPLETE) ? 1 : 0);
            }
            return null; // No embedding
        }

        @Override
        protected EmbeddingPresence embeddingPresence(JFXTokenId id) {
            switch (id) {
                case COMMENT:
                case QUOTE_LBRACE_STRING_LITERAL:
                case RBRACE_QUOTE_STRING_LITERAL:
                case RBRACE_LBRACE_STRING_LITERAL:
                case DoubleQuoteBody:
                case SingleQuoteBody:
                case STRING_LITERAL:
                    return EmbeddingPresence.ALWAYS_QUERY;
                default:
                    return EmbeddingPresence.NONE;
            }
        }

    }.language();

    public static Language<JFXTokenId> language() {
        return language;
    }

    /**
     * Check if provided token is comment token.
     * @param id to check.
     * @return true if <code>id</code> is comment
     */
    public static boolean isComment(JFXTokenId id) {
        if (id == null) return false;
        switch (id) {
            case COMMENT:
            case LINE_COMMENT:
            case DOC_COMMENT:
                return true;
            default:
                return false;
        }
    }
}
