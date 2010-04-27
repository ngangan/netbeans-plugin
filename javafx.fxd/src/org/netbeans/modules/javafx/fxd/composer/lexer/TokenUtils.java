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

import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;

/**
 *
 * @author avk
 */
public class TokenUtils {

    private TokenUtils() {
    }

    public static boolean isInsideMlComment(TokenSequence<FXDTokenId> ts, int startOffset){
        ts.move(startOffset);
        if (!(ts.moveNext() || ts.movePrevious())) {
            return false;
        }
        if (ts.token().id() == FXDTokenId.COMMENT && startOffset >= ts.offset() + 2) { // dot after '/*'
            return true;
        }
        return false;
    }

    /**
     * finds next token in forward direction skipping whitespaces
     * and comments (single- and multi-line)
     * @param ts TokenSequence
     * @return next non white token in forward direction
     */
    public static Token<FXDTokenId> getNextNonWhite(TokenSequence<FXDTokenId> ts){
        while (ts.moveNext()){
            if (!TokenUtils.isWhiteToken(ts.token()) ){
                return ts.token();
            }
        }
        return null;
    }

    /**
     * finds next token in forward direction skipping whitespaces
     * and comments (single- and multi-line)
     * @param ts TokenSequence
     * @param startOffset start offset
     * @return next non white token in forward direction
     */
    public static Token<FXDTokenId> getNextNonWhite(TokenSequence<FXDTokenId> ts,
            int startOffset){
        ts.move(startOffset);
        return getNextNonWhite(ts);
    }

    /**
     * finds next token in backward direction skipping whitespaces
     * and comments (single- and multi-line)
     * @param ts TokenSequence
     * @return next non white token in backward direction
     */
    public static Token<FXDTokenId> getPrevNonWhite(TokenSequence<FXDTokenId> ts){
        while (ts.movePrevious()){
            if (!TokenUtils.isWhiteToken(ts.token()) ){
                return ts.token();
            }
        }
        return null;
    }

    /**
     * finds next token in backward direction skipping whitespaces
     * and comments (single- and multi-line)
     * @param ts TokenSequence
     * @param startOffset start offset
     * @return next non white token in backward direction
     */
    public static Token<FXDTokenId> getPrevNonWhite(TokenSequence<FXDTokenId> ts,
            int startOffset){
        ts.move(startOffset);
        return getPrevNonWhite(ts);
    }

    public static boolean isWhiteToken(Token<FXDTokenId> t){
        return (   t.id() == FXDTokenId.WS
                  || isCommentToken(t)
//                || t.id() == FXDTokenId.EOF
                );
    }

    public static boolean isCommentToken(Token<FXDTokenId> t){
        return (   t.id() == FXDTokenId.LINE_COMMENT
                || t.id() == FXDTokenId.COMMENT );
    }

    public static boolean isLBracketToken(Token<FXDTokenId> t){
        return (   t.id() == FXDTokenId.LBRACKET
                || t.id() == FXDTokenId.LBRACE
                );
    }

    public static boolean isRBracketToken(Token<FXDTokenId> t){
        return (   t.id() == FXDTokenId.RBRACKET
                || t.id() == FXDTokenId.RBRACE
                );
    }

    public static boolean isAttribsSeparatorToken(Token<FXDTokenId> t){
        return (   t.id() == FXDTokenId.COMMA
                || t.id() == FXDTokenId.SEMI
                );
    }

}
