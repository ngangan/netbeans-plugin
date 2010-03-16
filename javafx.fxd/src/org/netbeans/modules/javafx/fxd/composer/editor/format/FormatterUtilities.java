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
package org.netbeans.modules.javafx.fxd.composer.editor.format;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.api.lexer.TokenUtilities;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.editor.indent.api.IndentUtils;
import org.netbeans.modules.javafx.fxd.composer.editor.BracketCompletion;
import org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId;
import org.netbeans.modules.javafx.fxd.composer.lexer.TokenUtils;

/**
 *
 * @author Andrey Korostelev
 */
public class FormatterUtilities {

    static final int MULTILINE_STRING_INDENT_STEPS = 2;
    static final int MULTILINE_COMMENT_INDENT_CHARS = 1;

    /**
     * tests if given char is lbracket supported by formatter : '[' or '{'
     * @param c char to test
     * @return true if char equals to [ or {
     */
    public static boolean isLBracket(char c){
        return ( c == '[' || c == '{' );
    }

    static boolean stringStartsOnPrevLine(Document document, TokenSequence<FXDTokenId> ts,
            int startOffset) throws BadLocationException{
        return isOnTokensLine(document, ts, startOffset, FXDTokenId.STRING_LITERAL, 1);
    }

    /**
     * tests if startOffset is on specified line of the token.
     * @param document
     * @param ts
     * @param startOffset
     * @param tid
     * @param whichTokensLine on which token's line startOffset should be. 
     * Lines indexes start from 0.
     * @return
     * @throws BadLocationException
     */
    static boolean isOnTokensLine(Document document, TokenSequence<FXDTokenId> ts,
            int startOffset, FXDTokenId tid, int whichTokensLine ) throws BadLocationException{
        ts.move(startOffset);
        FXDTokenId id = ts.moveNext() ? ts.token().id() : null;
        if (id == tid) {
            int tokenRowStart = Utilities.getRowStart((BaseDocument)document, ts.offset());
            int breakRowStart = Utilities.getRowStart((BaseDocument)document, startOffset, 
                    -whichTokensLine);
            if (tokenRowStart == breakRowStart) {
                return true;
            }
        }
        return false;
    }

    /**
     * calculates desired current line indent basing on previous line content.
     * e.g. increases indent if previous line ends with '{' or '['
     * (ignoring whitespaces and comments).
     * @param document
     * @param rootElement root Element of the document
     * @param startOffset document offset of any position on the line which indent should be calculated
     * @return indent 
     * @throws BadLocationException
     */
    static int calculateLineIndent(Document document, int startOffset) throws BadLocationException {
        int indent = 0;
        TokenSequence<FXDTokenId> ts = BracketCompletion.getTokenSequence((BaseDocument)document, startOffset);
        int prevCharIdx = getFirstNonWhiteCharIdxBwd((BaseDocument)document, ts, startOffset);
        if (prevCharIdx == -1){
             indent = getPrevLineIndent(document, startOffset);
             if (indent == -1){
                 indent = getCurrentLineIndent(document, startOffset);
             }
             return indent;
        }
        indent = getCurrentLineIndent(document, prevCharIdx);
        if (stringStartsOnPrevLine(document, ts, startOffset)) {
            return incIndent(document, indent, FormatterUtilities.MULTILINE_STRING_INDENT_STEPS);
        }
        if (isPrevTokenLBracket(ts, startOffset)){
            if (!isNextOnLineRBracket(document, ts, startOffset)){
                indent = incIndent(document, indent);
            }
        }
        if (TokenUtils.isInsideMlComment(ts, startOffset)) {
            indent += FormatterUtilities.MULTILINE_COMMENT_INDENT_CHARS;
        }
        return indent;
    }


    private static int getPrevLineIndent(Document document, int startOffset)
            throws BadLocationException{
        int prevRowStart = Utilities.getRowStart((BaseDocument)document, startOffset, -1);
        if (prevRowStart == -1){
            return -1;
        }
        return IndentUtils.lineIndent(document, prevRowStart);
    }

    /**
     * @param document non-null document.
     * @param offset &gt;= 0 offset anywhere on the line.
     * @return current line indent
     * @throws BadLocationException
     */
    static int getCurrentLineIndent(Document document, int offset)
            throws BadLocationException{
        int rowStart = Utilities.getRowStart((BaseDocument)document, offset);
        return IndentUtils.lineIndent(document, rowStart);
    }

    private static boolean isNextOnLineRBracket(Document document,
            TokenSequence<FXDTokenId> ts, int startOffset)
            throws BadLocationException {
        int eolOffset = Utilities.getRowEnd((BaseDocument) document, startOffset);
        if (startOffset == eolOffset || eolOffset == -1) {
            return false;
        }
        ts.move(startOffset);
        Token<FXDTokenId> t = TokenUtils.getNextNonWhite(ts);
        if(t == null){
            return false;
        }
        if (TokenUtils.isRBracketToken(t) && ts.offset()+t.length() <= eolOffset){
            return true;
        }
        return false;
    }

    private static boolean isPrevTokenLBracket(TokenSequence<FXDTokenId> ts, int startOffset)
            throws BadLocationException {

        ts.move(startOffset);
        Token<FXDTokenId> t = TokenUtils.getPrevNonWhite(ts);
        if(t == null){
            return false;
        }
        if (TokenUtils.isLBracketToken(t)){
            return true;
        }
        return false;
    }

    private static int incIndent(Document document, int indent){
        return incIndent(document, indent, 1);
    }

    private static int incIndent(Document document, int indent, int steps){
        return indent + IndentUtils.indentLevelSize(document) * steps;
    }

    /**
     * finds the first char in backward direction ignoring whitespaces
     * and comments (single- and multi-line)
     * @param doc 
     * @param ts TokenSequence
     * @param offset
     * @return next non white token in backward direction
     */
    static int getFirstNonWhiteCharIdxBwd(BaseDocument doc,
            TokenSequence<FXDTokenId> ts, int offset) throws BadLocationException{

        ts.move(offset);
        if (ts.moveNext() || ts.movePrevious()){
            if (!TokenUtils.isWhiteToken(ts.token())){
                return Utilities.getFirstNonWhiteBwd(doc, offset);
            }
            Token<FXDTokenId> t = TokenUtils.getPrevNonWhite(ts);
            return t != null ? ts.offset() : -1;

        }
        return -1;
    }

}
