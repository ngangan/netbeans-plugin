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
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.editor.indent.api.IndentUtils;
import org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId;

/**
 *
 * @author Andrey Korostelev
 */
class FormatterUtilities {


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
        int prevCharIdx = Utilities.getFirstNonWhiteBwd((BaseDocument) document, startOffset);
        if (prevCharIdx == -1){
             indent = getPrevLineIndent(document, startOffset);
             if (indent == -1){
                 indent = getCurrentLineIndent(document, startOffset);
             }
             return indent;
        }
        
        indent = getCurrentLineIndent(document, prevCharIdx);
        String prevChar = document.getText(prevCharIdx, 1);
        if (prevChar.equals("{") || prevChar.equals("[")) { // NOI18N
            if (!isNextOnLineRBracket(document, startOffset)) {
                indent = incIndent(document, indent);
            }
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

    private static boolean isNextOnLineRBracket(Document document, int startOffset)
            throws BadLocationException {
        int eolOffset = Utilities.getRowEnd((BaseDocument) document, startOffset);
        if (startOffset == eolOffset || eolOffset == -1) {
            return false;
        }
        int nextCharIdx = Utilities.getFirstNonWhiteFwd((BaseDocument) document, startOffset, eolOffset);
        if (nextCharIdx == -1) {
            return false;
        }
        String nextChar = document.getText(nextCharIdx, 1);
        if (nextChar.equals("}") || nextChar.equals("]")) { // NOI18N
            return true;
        }
        return false;
    }

    private static int incIndent(Document document, int indent){
        return indent + IndentUtils.indentLevelSize(document);
    }

    /**
     * finds next token in backward direction skipping whitespaces
     * and comments (single- and multi-line)
     * @param ts TokenSequence
     * @return next non white token in backward direction
     */
    static Token<FXDTokenId> getNextNonWhiteBwd(TokenSequence<FXDTokenId> ts){
        while (ts.movePrevious()){
            if (!isWhiteToken(ts.token()) ){
                return ts.token();
            }
        }
        return null;
    }

    /**
     * finds next token in forward direction skipping whitespaces
     * and comments (single- and multi-line)
     * @param ts TokenSequence
     * @return next non white token in forward direction
     */
    static Token<FXDTokenId> getNextNonWhiteFwd(TokenSequence<FXDTokenId> ts){
        while (ts.moveNext()){
            if (!isWhiteToken(ts.token()) ){
                return ts.token();
            }
        }
        return null;
    }

    static boolean isWhiteToken(Token<FXDTokenId> t){
        return (   t.id() == FXDTokenId.WS
                || t.id() == FXDTokenId.LINE_COMMENT
                || t.id() == FXDTokenId.COMMENT
//                || t.id() == FXDTokenId.EOF
                );
    }

    static boolean isLBracketToken(Token<FXDTokenId> t){
        return (   t.id() == FXDTokenId.LBRACKET
                || t.id() == FXDTokenId.LBRACE
                );
    }

    static boolean isRBracketToken(Token<FXDTokenId> t){
        return (   t.id() == FXDTokenId.RBRACKET
                || t.id() == FXDTokenId.RBRACE
                );
    }

    static boolean isAttribsSeparatorToken(Token<FXDTokenId> t){
        return (   t.id() == FXDTokenId.COMMA
                || t.id() == FXDTokenId.SEMI
                );
    }

}
