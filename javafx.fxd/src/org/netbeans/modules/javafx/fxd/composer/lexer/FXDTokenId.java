/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
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
package org.netbeans.modules.javafx.fxd.composer.lexer;

import java.util.Arrays;
import java.util.Collection;
import org.netbeans.api.lexer.Language;
import org.netbeans.api.lexer.TokenId;
import org.netbeans.modules.javafx.fxd.dataloader.fxd.FXDDataLoader;
import org.netbeans.spi.lexer.LanguageHierarchy;
import org.netbeans.spi.lexer.Lexer;
import org.netbeans.spi.lexer.LexerRestartInfo;

/**
 *
 * @author Pavel Benes
 */
public enum FXDTokenId implements TokenId {

    COLON("operator"), // NOI18N
    COMMA("separator"), // NOI18N
    COMMENT("comment"), // NOI18N
    NUMERIC_LITERAL("number"), // NOI18N
    DOT("separator"), // NOI18N
    DOTDOT("operator"), // NOI18N
    EQ("operator"), // NOI18N
    FALSE("keyword-literal"), // NOI18N
    FLOATING_POINT_LITERAL("number"), // NOI18N
    IDENTIFIER("identifier"), // NOI18N
    IDENTIFIER_ATTR("identifier_attr"), // NOI18N
    LBRACE("separator"), // NOI18N
    LBRACKET("separator"), // NOI18N
    LINE_COMMENT("comment"), // NOI18N
    LPAREN("separator"), // NOI18N
    NULL("keyword-literal"), // NOI18N
    RBRACE("separator"), // NOI18N
    RBRACKET("separator"), // NOI18N
    RPAREN("separator"), // NOI18N
    SEMI("separator"), // NOI18N
    STRING_LITERAL("string"), // NOI18N
    TRUE("keyword-literal"), // NOI18N
    WS("whitespace"), // NOI18N

    UNKNOWN("error"), // NOI18N
    EOF("default"),
    ;
    
    public static final String UNIVERSAL_CATEGORY = "future-literal"; // NOI18N
    
    private final String primaryCategory;
    private final String tokenName;

    FXDTokenId(String primaryCategory, String tokenName) {
        this.primaryCategory = primaryCategory;
        this.tokenName = tokenName;
    }

    FXDTokenId(String primaryCategory) {
        this.primaryCategory = primaryCategory;
        this.tokenName = null;
    }

    @Override
    public String toString() {
        return super.toString();
    }

    public String getTokenName() {
        return tokenName;
    }

    public String primaryCategory() {
        return primaryCategory != null ? primaryCategory : UNIVERSAL_CATEGORY;
    }
    private static final Language<FXDTokenId> language = new LanguageHierarchy<FXDTokenId>() {

        protected Collection<FXDTokenId> createTokenIds() {
            return Arrays.asList(FXDTokenId.values());
        }

        protected Lexer<FXDTokenId> createLexer(LexerRestartInfo<FXDTokenId> info) {
            return new FXDLexer(info);
        }

        @Override
        protected String mimeType() {
            return FXDDataLoader.REQUIRED_MIME;
        }
    }.language();

    public static Language<FXDTokenId> language() {
        return language;
    }
}
