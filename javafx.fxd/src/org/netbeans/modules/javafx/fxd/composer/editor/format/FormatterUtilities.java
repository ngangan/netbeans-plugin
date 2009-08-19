/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.fxd.composer.editor.format;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.editor.indent.api.IndentUtils;
import org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId;

/**
 *
 * @author avk
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
