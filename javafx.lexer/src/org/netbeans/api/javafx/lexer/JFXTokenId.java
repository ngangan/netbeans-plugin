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
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * @author Rastislav Komara (<a href="mailto:rastislav.komara@sun.com">RKo</a>)
 * @todo documentation
 */
public enum JFXTokenId implements TokenId {
    PACKAGE("keyword", 28),
    FUNCTION("keyword", 17),
    LT("operator", 88),
    STAR("operator", 94),
    WHILE("keyword-directive", 45),
    TranslationKeyBody("operator", 151),
    NEW("keyword", 24),
    INDEXOF("keyword", 20),
    SEQ_EXPLICIT(null, 131),
    PARAM(null, 111),
    TIME_LITERAL("time", 156),
    TYPE_UNKNOWN("error", 136),
    NOT("keyword", 25),
    FUNC_EXPR(null, 112),
    RBRACE_QUOTE_STRING_LITERAL("string", 147),
    BREAK("keyword-directive", 11),
    STATEMENT(null, 113),
    MODIFIER(null, 109),
    LBRACKET("separator", 48),
    RPAREN("separator", 80),
    IMPORT("keyword", 19),
    STRING_LITERAL("string", 143),
    FLOATING_POINT_LITERAL("number", 161),
    INSERT("keyword", 22),
    SUBSUB("operator", 50),
    Digits("number", 154),
    BIND("keyword", 9),
    STAREQ("operator", 99),
    OBJECT_LIT_PART("string", 129),
    THIS("keyword", 39),
    RETURN("keyword-directive", 34),
    DoubleQuoteBody("string", 141),
    TRANSLATION_KEY("i18n-artifact", 152),
    VAR("keyword", 44),
    SUPER("keyword", 36),
    LAST("keyword", 68),
    EQ("operator", 86),
    COMMENT("comment", 166),
    INTO("keyword", 66),
    QUES("operator", 104),
    BOUND("keyword", 10),
    EQEQ("operator", 85),
    MISSING_NAME(null, 116),
    RBRACE("separator", 149),
    POUND("operator", 46),
    LINE_COMMENT("comment", 167),
    STATIC("keyword", 38),
    PRIVATE("keyword", 30),
    SEQ_INDEX(null, 125),
    NULL("keyword-literal", 26),
    ELSE("keyword-directive", 58),
    ON("keyword", 71),
    DELETE("keyword", 14),
    SLASHEQ("operator", 100),
    TYPE_FUNCTION(null, 134),
    ASSERT("keyword-directive", 6),
    TRY("keyword-directive", 41),
    TYPED_ARG_LIST(null, 138),
    TYPE_ANY(null, 135),
    SLICE_CLAUSE(null, 117),
    INVERSE("keyword", 67),
    WS("whitespace", 165),
    RangeDots("separator", 160),
    TYPEOF("keyword", 43),
    OR("keyword", 72),
    JavaIDDigit("identifier", 163),
    SIZEOF("keyword", 37),
    GT("operator", 87),
    CATCH("keyword-directive", 57),
    FROM("keyword", 63),
    REVERSE("keyword", 35),
    FALSE("keyword-literal", 15),
    INIT("keyword", 21),
    Letter("identifier", 162),
    DECIMAL_LITERAL("number", 153),
    THROW("keyword-directive", 40),
    LAST_TOKEN(null, 168),
    PROTECTED("keyword", 31),
    WHERE("keyword", 78),
    CLASS("keyword", 12),
    SEQ_SLICE_EXCLUSIVE(null, 127),
    ON_REPLACE_SLICE(null, 118),
    PLUSPLUS("operator", 49),
    LBRACE("separator", 146),
    TYPE_NAMED(null, 133),
    ATTRIBUTE("keyword", 8),
    LTEQ("operator", 90),
    SUBEQ("operator", 98),
    OBJECT_LIT(null, 128),
    Exponent("number", 155),
    FOR("keyword-directive", 16),
    STEP("keyword", 74),
    SUB("operator", 93),
    DOTDOT("operator", 79),
    ABSTRACT("keyword", 5),
    EXCLUSIVE("keyword", 59),
    NextIsPercent("string", 144),
    AND("keyword", 54),
    TYPE_ARG(null, 137),
    HexDigit("number", 158),
    PLUSEQ("operator", 97),
    LPAREN("separator", 47),
    IF("keyword-directive", 18),
    EXPR_LIST(null, 120),
    AS("keyword", 55),
    SLASH("operator", 95),
    IN("keyword", 64),
    THEN("keyword", 75),
    CONTINUE("keyword-directive", 13),
    COMMA("separator", 83),
    IDENTIFIER("identifier", 164),
    REPLACE("keyword", 73),
    TWEEN("keyword", 105),
    QUOTE_LBRACE_STRING_LITERAL("string", 145),
    DOC_COMMENT("comment", 139),
    POSTINCR(null, 123),
    SEMI_INSERT_START("string", 4),
    PIPE("operator", 51),
    PLUS("operator", 92),
    HEX_LITERAL("number", 159),
    EMPTY_FORMAT_STRING(null, 132),
    RBRACKET("separator", 81),
    DOT("separator", 84),
    RBRACE_LBRACE_STRING_LITERAL("string", 148),
    EXPRESSION(null, 114),
    WITH("keyword", 77),
    PERCENT("operator", 96),
    LAZY("keyword", 69),
    LTGT("operator", 89),
    NEGATIVE(null, 122),
    ON_REPLACE(null, 119),
    OCTAL_LITERAL("number", 157),
    BEFORE("keyword", 56),
    INSTANCEOF("keyword", 65),
    FUNC_APPLY(null, 121),
    AFTER("keyword", 53),
    GTEQ("operator", 91),
    CLASS_MEMBERS(null, 110),
    MODULE(null, 107),
    READONLY("keyword", 33),
    TRUE("keyword-literal", 42),
    SEMI("separator", 82),
    COLON("operator", 103),
    POSTINIT("keyword", 29),
    SEMI_INSERT_END(null, 52),
    PERCENTEQ("operator", 101),
    FINALLY("keyword-directive", 61),
    OVERRIDE("keyword", 27),
    FORMAT_STRING_LITERAL("format", 150),
    BLOCK(null, 115),
    SEQ_EMPTY(null, 130),
    SEQ_SLICE(null, 126),
    POSTDECR(null, 124),
    SUCHTHAT("operator", 106),
    PUBLIC("keyword", 32),
    EXTENDS("keyword", 60),
    SingleQuoteBody("string", 142),
    LET("keyword", 23),
    FIRST("keyword", 62),
    NOTEQ("operator", 102),
    TRIGGER("keyword", 76),
    EMPTY_MODULE_ITEM(null, 108),
    AT("keyword", 7),
    MOD("keyword",70),
    ATTR_INTERPOLATE("keyword", 140),
    UNKNOWN("error", 200);

    public static final String UNIVERSAL_CATEGORY = "future-literal";
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
                if (log.isLoggable(Level.SEVERE)) log.severe("Cannot create lexer.\n" + e);
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
                case COMMENT:
                    final StringBuilder tt = token.text() != null ? new StringBuilder(token.text()) : null;
                    if (tt != null && tt.toString().trim().startsWith("/**")) {
                        return LanguageEmbedding.create(JavadocTokenId.language(), 3,
                                (token.partType() == PartType.COMPLETE) ? 2 : 0);
                    } else {
                        return null;
                    }
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


}
