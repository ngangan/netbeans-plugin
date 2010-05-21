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

package org.netbeans.modules.javafx.fxd.composer.editor;

import java.util.Arrays;
import java.util.List;
import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.lexer.TokenId;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId;

/**
 * This static class groups the whole aspect of bracket
 * completion. It is defined to clearly separate the functionality
 * and keep actions clean.
 * The methods of the class are called from different actions as
 * KeyTyped, DeletePreviousChar.
 *
 * @author Andrey Korostelev
 */
public class BracketCompletion {

    // move to fxd specific BracesMatcher implementation if will create id.
    private static final FXDTokenId[] PAIR_BRACES_TOKEN_IDS = new FXDTokenId[]{
            FXDTokenId.LPAREN,      FXDTokenId.RPAREN,
            FXDTokenId.LBRACKET,    FXDTokenId.RBRACKET,
            FXDTokenId.LBRACE,      FXDTokenId.RBRACE,
    };

    public static <T extends TokenId> TokenSequence<T> getTokenSequence(BaseDocument doc, int dotPos) {
        TokenHierarchy<BaseDocument> th = TokenHierarchy.get(doc);
        @SuppressWarnings("unchecked")
        TokenSequence<T> seq = (TokenSequence<T>)th.tokenSequence();
        seq.move(dotPos);
        return seq;
    }

    /**
     * posWithinString(doc, pos) if position *pos* is within a string
     * literal in document doc.
     *
     * @param doc    the document
     * @param dotPos position to be tested
     * @return true if matched.
     */
    static boolean posWithinString(BaseDocument doc, int dotPos) {
        return posWithinQuotes(doc, dotPos, '\"', FXDTokenId.STRING_LITERAL); // NOI18N
    }

    /**
     * Generalized posWithingString to any token and delimiting
     * character. It works for tokens are delimited by *quote* and
     * extend up to the other *quote* or whitespace in case of an
     * incomplete token.
     *
     * @param doc     the document
     * @param dotPos  position to be tested
     * @param quote   expected quote
     * @param tokenID id of expected token
     * @return true if matched.
     */
    static boolean posWithinQuotes(BaseDocument doc, int dotPos, char quote, TokenId... tokenID) {
        try {
            FXDTokenId tid = tokenAt(doc, dotPos);
            return tid != null && Arrays.asList(tokenID).contains(tid) && doc.getChars(dotPos - 1, 1)[0] != quote;
        } catch (BadLocationException ex) {
            return false;
        }
    }

    /**
     * Resolve whether pairing right curly brace or bracket should be added automatically
     * at the caret position or not.
     * <br>
     * There must be only whitespace or line comment or block comment
     * between the caret position
     * and the left brace/bracket and the left brace/bracket must be on the same line
     * where the caret is located.
     * <br>
     * The caret must not be "contained" in the opened block comment token.
     *
     * @param doc         document in which to operate.
     * @param caretOffset offset of the caret.
     * @return FXDTokenId of brace/bracket that should be added.
     * NULL of paired bracket should not be added.
     */
    // copied from org.netbeans.modules.javafx.editor.BracketCompletion with
    // TokenId replacement. [] support is also added
    static FXDTokenId isAddRightBracket(BaseDocument doc, int caretOffset)
            throws BadLocationException {
        boolean addRightBrace = false;
        FXDTokenId leftBracket = null;
        FXDTokenId rightBracket = null;
        if (completionSettingEnabled()) {
            if (caretOffset > 0) {
                // Check whether line ends with '{' ignoring any whitespace
                // or comments
                final TokenSequence<FXDTokenId> ts = getTokenSequence(doc, caretOffset);
                Token<FXDTokenId> token = ts.moveNext() ? ts.token() : null;

                addRightBrace = true; // suppose that right brace should be added

                // Disable right brace adding if caret not positioned within whitespace
                // or line comment
                if (token != null) {  //fix #131648
                    int tsoff = ts.offset();
                    int off = (caretOffset - ts.offset());
                    if (off > 0 && off < token.length()) { // caret contained in token
                        switch (token.id()) {
                            case WS:
                            case LINE_COMMENT:
                            case UNKNOWN:
                                break; // the above tokens are OK

                            default:
                                // Disable brace adding for the remaining ones
                                addRightBrace = false;
                        }
                    }
                }

                if (addRightBrace) { // still candidate for adding
                    int caretRowStartOffset = Utilities.getRowStart(doc, caretOffset);

                    // Check whether there are only whitespace or comment tokens
                    // between caret and left brace and check only on the line
                    // with the caret
                    while (token != null && ts.offset() >= caretRowStartOffset) {
                        boolean ignore = false;
                        // Assuming java token context here
                        switch (token.id()) {
                            case WS:
                            case COMMENT:
                            case LINE_COMMENT:
                            case UNKNOWN:
                                // skip
                                ignore = true;
                                break;
                        }

                        if (ignore) {
                            token = ts.movePrevious() ? ts.token() : null;
                        } else { // break on the current token
                            break;
                        }
                    }
                    //if ((token != null) && ((token.id() != FXDTokenId.LBRACE) || (ts.offset() < caretRowStartOffset))) {
                    //    addRightBrace = false;
                    //}
                    if ((token != null) && ts.offset() >= caretRowStartOffset){
                        if (token.id() == FXDTokenId.LBRACE || token.id() == FXDTokenId.LBRACKET) {
                            leftBracket = token.id();
                            rightBracket = getOpositeBracket(leftBracket, true);
                        } else {
                            addRightBrace = false;
                        }
                    } else {
                        addRightBrace = false;
                    }

                }

                // Finally check the brace balance whether there are any missing right braces
                if (addRightBrace && leftBracket != null && rightBracket != null) {
                    addRightBrace = (tokenBalance(doc, true, leftBracket, rightBracket) > 0);
                }
            }
        }
        return addRightBrace ? rightBracket : null;
    }

    public static <T extends TokenId> T tokenAt(BaseDocument doc, int dotPos) {
        TokenSequence<T> seq = getTokenSequence(doc, dotPos);
        return seq.moveNext() ? seq.token().id() : null;
    }

    /**
     * Check for conditions and possibly complete an already inserted
     * quote .
     *
     * @param doc     the document
     * @param dotPos  position of the opening bracket (already in the doc)
     * @param caret   caret
     * @param bracket the character that was inserted
     */
    public static boolean completeQuote(BaseDocument doc, int dotPos, Caret caret,
                                 char bracket) throws BadLocationException {

        if (!completionSettingEnabled()) {
            return false;
        }

        if (isEscapeSequence(doc, dotPos)) { // \" or \' typed
            return false;
        }

        FXDTokenId token = null;
        if (doc.getLength() > dotPos) {
            token = tokenAt(doc, dotPos);
        }

        int lastNonWhite = Utilities.getRowLastNonWhite(doc, dotPos);
        // eol - true if the caret is at the end of line (ignoring whitespaces)
        boolean eol = lastNonWhite < dotPos;

        if (isComment(token)) {
            return false;
        } else if (token == FXDTokenId.WS && eol && dotPos - 1 > 0) {
            // check if the caret is at the very end of the line comment
            token = tokenAt(doc, dotPos - 1);
            if (token == FXDTokenId.LINE_COMMENT) {
                return false;
            }
        }

        boolean completablePosition = isQuoteCompletablePosition(doc, dotPos);
        boolean insideString = insideString(token);

        if (!insideString) {
            // check if the caret is at the very end of the line and there
            // is an unterminated string literal
            if (token == FXDTokenId.WS && eol) {
                if (dotPos - 1 > 0) {
                    token = tokenAt(doc, dotPos - 1);
                    insideString = insideString(token);
                }
            }
        }

        if (insideString) {
            if (eol) {
                return false; // do not complete
            } else {
                //#69524
                char chr = doc.getChars(dotPos, 1)[0];
                if (chr == bracket) {
                    doc.insertString(dotPos, "" + bracket, null); //NOI18N
                    doc.remove(dotPos, 1);
                    return true;
                }
            }
        }

        if ((completablePosition && !insideString) || eol) {
            doc.insertString(dotPos, "" + bracket + bracket, null); //NOI18N
            return true;
        }

        return false;
    }

    /**
     * A hook method called after a character was inserted into the
     * document. The function checks for special characters for
     * completion ()[]'"{} and other conditions and optionally performs
     * changes to the doc and or caret (complets braces, moves caret,
     * etc.)
     *
     * @param doc    the document where the change occurred
     * @param dotPos position of the character insertion
     * @param caret  caret
     * @param ch     the character that was inserted
     * @throws BadLocationException if dotPos is not correct
     */
    public static void charInserted(BaseDocument doc, int dotPos, Caret caret, char ch) throws BadLocationException {
        if (!completionSettingEnabled()) {
            return;
        }

        if ((ch == ']' || ch == '[') && !posWithinString(doc, caret.getDot())) {  //completion works only outside of string. // NOI18N
            TokenSequence<FXDTokenId> seq = getTokenSequence(doc, dotPos);
            FXDTokenId tidAtDot = seq.moveNext() ? seq.token().id() : null;
            if (tidAtDot == null) return;

            if (tidAtDot == FXDTokenId.RBRACKET) { // NOI18N
                skipClosingBracket(doc, caret, tidAtDot, ch);
            } else if (tidAtDot == FXDTokenId.LBRACKET) {
                completeOpeningBracket(doc, dotPos, caret, ch);
            }
        }
    }

    /**
     * Hook called after a character *ch* was backspace-deleted from
     * *doc*. The function possibly removes bracket or quote pair if
     * appropriate.
     *
     * @param doc    the document
     * @param dotPos position of the change
     * @param ch     the character that was deleted
     * @throws javax.swing.text.BadLocationException
     *          if operation is called out of document range.
     */
    public static void charBackspaced(BaseDocument doc, int dotPos, char ch) throws BadLocationException {
        if (completionSettingEnabled()) {
            if (ch == '(' || ch == '[' || ch == '{') { // NOI18N
                TokenId tidAtDot = tokenAt(doc, dotPos);
                if ((tidAtDot == FXDTokenId.RBRACKET && tokenBalance(doc, FXDTokenId.LBRACKET, FXDTokenId.RBRACKET) != 0) ||
                        (tidAtDot == FXDTokenId.RPAREN && tokenBalance(doc, FXDTokenId.LPAREN, FXDTokenId.RPAREN) != 0)) {
                    doc.remove(dotPos, 1);
                }
            } else if (ch == '\"') { // NOI18N
                char match[] = doc.getChars(dotPos, 1);
                if (match != null && match[0] == '\"') { // NOI18N
                    doc.remove(dotPos, 1);
                }
            } else if (ch == '\'') { // NOI18N
                char match[] = doc.getChars(dotPos, 1);
                if (match != null && match[0] == '\'') { // NOI18N
                    doc.remove(dotPos, 1);
                }
            }
        }
    }

    /**
     * Check whether the typed bracket should stay in the document
     * or be removed.
     * <br>
     * This method is called by <code>skipClosingBracket()</code>.
     *
     * @param caretOffset offset in document
     * @param doc         document into which typing was done.
     * @param bracketId   tokenId of bracket type
     * @param c
     * @return true if skip of bracked is required
     * @throws javax.swing.text.BadLocationException
     *          if operation is called on invalid document position.
     */
    static boolean isSkipClosingBracket(int caretOffset, BaseDocument doc, FXDTokenId bracketId, char c) throws BadLocationException {
        //TODO: [RKo] Make this method more readable. Too high CC, nesting and length
        // First check whether the caret is not after the last char in the document
        // because no bracket would follow then so it could not be skipped.
        if (caretOffset == doc.getLength()) {
            return false; // no skip in this case
        }

        boolean skipClosingBracket = false; // by default do not remove

        // Examine token at the caret offset
//        TokenItem token = ((ExtSyntaxSupport) doc.getSyntaxSupport()).getTokenChain(
//                caretOffset, caretOffset + 1);
        final TokenSequence<?> ts = getTokenSequence(doc, caretOffset);
        Token<?> token = ts.moveNext() ? ts.token() : null;

        // Check whether character follows the bracket is the same bracket
        if (token != null && token.id() == bracketId
                // we are escaping right bracket inserted into RL_SL because it is correct statement.
                // manowar: commented this weird line according issue #156157
//                && !(bracketId == JFXTokenId.RBRACE_LBRACE_STRING_LITERAL && c == '}') // NOI18N
                ) {
            FXDTokenId leftBracketIntId = getOpositeBracket(bracketId, false);

            // Skip all the brackets of the same type that follow the last one

            Token<?> nextToken = ts.moveNext() ? ts.token() : null;
            while (nextToken != null && nextToken.id() == bracketId) {
                nextToken = ts.moveNext() ? ts.token() : null;
            }
            // token var points to the last bracket in a group of two or more right brackets
            // Attempt to find the left matching bracket for it
            // Search would stop on an extra opening left brace if found
            int braceBalance = 0; // balance of '{' and '}'
            int bracketBalance = -1; // balance of the brackets or parenthesis
            token = ts.movePrevious() ? ts.token() : null;
            boolean finished = false;
            while (!finished && token != null) {
                final FXDTokenId tokenIntId = (FXDTokenId) token.id();
                switch (tokenIntId) {
                    case LPAREN:
                    case LBRACKET:
                        if (tokenIntId == bracketId) {
                            bracketBalance++;
                            if (bracketBalance == 0) {
                                if (braceBalance != 0) {
                                    // Here the bracket is matched but it is located
                                    // inside an unclosed brace block
                                    // e.g. ... ->( } a()|)
                                    // which is in fact illegal but it's a question
                                    // of what's best to do in this case.
                                    // We chose to leave the typed bracket
                                    // by setting bracketBalance to 1.
                                    // It can be revised in the future.
                                    bracketBalance = 1;
                                }
                                finished = true;
                            }
                        }
                        break;

                    case RPAREN:
                    case RBRACKET:
                        if (tokenIntId == bracketId) {
                            bracketBalance--;
                        }
                        break;

                    case LBRACE:
                        braceBalance++;
                        if (braceBalance > 0) { // stop on extra left brace
                            finished = true;
                        }
                        break;

                    case RBRACE:
                        braceBalance--;
                        break;


                }

                token = ts.movePrevious() ? ts.token() : null; // done regardless of finished flag state
            }

            if (bracketBalance != 0) { // not found matching bracket
                // Remove the typed bracket as it's unmatched
                skipClosingBracket = true;

            } else {
                // the bracket is matched
                // Now check whether the bracket would be matched
                // when the closing bracket would be removed
                // i.e. starting from the original lastRBracket token
                // and search for the same bracket to the right in the text
                // The search would stop on an extra right brace if found
                braceBalance = 0;
                bracketBalance = 1; // simulate one extra left bracket
                token = ts.moveNext() ? ts.token() : null;
                finished = false;
                while (!finished && token != null) {
                    FXDTokenId tokenIntId = (FXDTokenId) token.id();
                    switch (tokenIntId) {
                        case LPAREN:
                        case LBRACKET:
                            if (tokenIntId == leftBracketIntId) {
                                bracketBalance++;
                            }
                            break;

                        case RPAREN:
                        case RBRACKET:
                            if (tokenIntId == bracketId) {
                                bracketBalance--;
                                if (bracketBalance == 0) {
                                    if (braceBalance != 0) {
                                        // Here the bracket is matched but it is located
                                        // inside an unclosed brace block
                                        // which is in fact illegal but it's a question
                                        // of what's best to do in this case.
                                        // We chose to leave the typed bracket
                                        // by setting bracketBalance to -1.
                                        // It can be revised in the future.
                                        bracketBalance = -1;
                                    }
                                    finished = true;
                                }
                            }
                            break;

                        case LBRACE:
                            braceBalance++;
                            break;

                        case RBRACE:
                            braceBalance--;
                            if (braceBalance < 0) { // stop on extra right brace
                                finished = true;
                            }
                            break;
                    }

                    token = ts.movePrevious() ? ts.token() : null; // done regardless of finished flag state
                }

                // If bracketBalance == 0 the bracket would be matched
                // by the bracket that follows the last right bracket.
                skipClosingBracket = (bracketBalance == 0);
            }
        }
        return skipClosingBracket;
    }

    /**
     * A hook to be called after closing bracket ) or ] was inserted into
     * the document. The method checks if the bracket should stay there
     * or be removed and some exisitng bracket just skipped.
     *
     * @param doc       the document
     * @param caret     caret
     * @param bracketId
     * @param ch
     * @throws javax.swing.text.BadLocationException
     *          if document location is invalid.
     */
    private static void skipClosingBracket(BaseDocument doc, Caret caret, FXDTokenId bracketId, char ch) throws BadLocationException {

        int caretOffset = caret.getDot();
        if (isSkipClosingBracket(caretOffset, doc, bracketId, ch)) {
            doc.remove(caretOffset - 1, 1);
            caret.setDot(caretOffset); // skip closing bracket
        }
    }

    private static boolean isEscapeSequence(BaseDocument doc, int dotPos) throws BadLocationException {
        if (dotPos <= 0) return false;
        char previousChar = doc.getChars(dotPos - 1, 1)[0];
        return previousChar == '\\'; // NOI18N
    }

    private static boolean isComment(FXDTokenId token) {
        return token == FXDTokenId.COMMENT || token == FXDTokenId.LINE_COMMENT;
    }

    private static boolean insideString(FXDTokenId token) {
        return token != null && token == FXDTokenId.STRING_LITERAL;
    }


    /**
     * Returns true if bracket completion is enabled in options.
     *
     * @return true if bracket completion is enabled
     */
    private static boolean completionSettingEnabled() {
        //return ((Boolean)Settings.getValue(JavaFXEditorKit.class, JavaSettingsNames.PAIR_CHARACTERS_COMPLETION)).booleanV
        return true;
    }

    /**
     * Counts the number of braces starting at dotPos to the end of the
     * document. Every occurence of { increses the count by 1, every
     * occurrence of } decreses the count by 1. The result is returned.
     *
     * @param doc document representing source code.
     * @return The number of { - number of } (>0 more { than } ,<0 more } than {)
     */
    private static int braceBalance(BaseDocument doc) {
        return tokenBalance(doc, FXDTokenId.LBRACE, FXDTokenId.RBRACE);
    }

    /**
     * The same as braceBalance but generalized to any pairs of matching
     * tokens.
     *
     * @param doc   document representing source code.
     * @param pairs pairs of oposite tokens to perform balance operation.
     * @return adjusted balance.
     */
    private static int tokenBalance(BaseDocument doc, TokenId... pairs) {
        return tokenBalance(doc, false, pairs);
    }

    /**
     * The same as braceBalance but generalized to any pairs of matching
     * tokens including optional scan of chars associated with 
     * the first two of specified tokens (got using {@link getBracketChar})
     * inside the {@link org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId#UNKNOWN}
     * token text.
     *
     * @param doc                      document representing source code.
     * @param handleSpecialBracesToken if true, method manualy parses
     *                                 {@link org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId#UNKNOWN}
     *                                 tokens to determine correct balance.
     * @param pairs                    pairs of oposite tokens to perform balance operation.
     * @return adjusted balance.
     */
    private static int tokenBalance(BaseDocument doc, boolean handleSpecialBracesToken, TokenId... pairs) {
        if (pairs == null || pairs.length == 0) return 0;
        if (pairs.length % 2 != 0)
            throw new IllegalArgumentException(java.util.ResourceBundle.getBundle("org/netbeans/modules/javafx/editor/Bundle").getString("The_odd_number_of_elements_should_not_be_paired!")); // NOI18N

        final List<TokenId> ids = Arrays.asList(pairs);
        TokenHierarchy<BaseDocument> th = TokenHierarchy.get(doc);
        TokenSequence<?> ts = th.tokenSequence();
        int balance = 0;
        while (ts.moveNext()) {
            final TokenId id = ts.token().id();
            final int index = ids.indexOf(id);
            if (index > -1) {
                if (index % 2 == 0) {
                    balance++;
                } else {
                    balance--;
                }
            } else if (handleSpecialBracesToken && FXDTokenId.UNKNOWN == id) {
                balance = balanceOfBracesInText(balance, ts.token(), pairs[0], pairs[1]);
            
            }

        }
        return balance;
    }

    /**
     * Gets balance of chars associated with specified tokens
     * (got using {@link getBracketChar})
     * in {@link org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId#UNKNOWN} token.
     * This is counted manualy based on characters occurence.
     *
     * @param balance current balance or zero if there is no balance precedens.
     * @param token   to manualy parse.
     * @return balance update by occurence of brackets chars in <code>token</code> text.
     */
    private static int balanceOfBracesInText(int balance, Token<? extends TokenId> token,
            TokenId leftBracket, TokenId rightBracket) {
        char lb = getBracketChar(leftBracket);
        char rb = getBracketChar(rightBracket);
        final CharSequence cs = token.text();
        for (int i = 0; i < cs.length(); i++) {
            final char c = cs.charAt(i);
            if (c == lb) {
                balance++;
            } else if (c == rb) {
                balance--;
            }
        }
        return balance;
    }

    /**
     * Check for various conditions and possibly add a pairing bracket
     * to the already inserted.
     *
     * @param doc     the document
     * @param dotPos  position of the opening bracket (already in the doc)
     * @param caret   caret
     * @param bracket the bracket that was inserted
     */
    private static void completeOpeningBracket(BaseDocument doc,
                                               int dotPos,
                                               Caret caret,
                                               char bracket) throws BadLocationException {
        if (isCompletablePosition(doc, dotPos + 1)) {
            String matchinBracket = "" + matching(bracket); // NOI18N
            doc.insertString(dotPos + 1, matchinBracket, null);
            caret.setDot(dotPos + 1);
        }
    }

    /**
     * Checks whether dotPos is a position at which bracket and quote
     * completion is performed. Brackets and quotes are not completed
     * everywhere but just at suitable places .
     *
     * @param doc    the document
     * @param dotPos position to be tested
     * @return true if we can use completition.
     * @throws javax.swing.text.BadLocationException
     *          if position is out of document range
     */
    private static boolean isCompletablePosition(BaseDocument doc, int dotPos) throws BadLocationException {
        if (dotPos == doc.getLength()) // there's no other character to test
            return true;
        else {
            // test that we are in front of ) , " or '
            char chr = doc.getChars(dotPos, 1)[0];
            return (chr == ')' || // NOI18N
                    chr == ',' || // NOI18N
                    chr == '\"' || // NOI18N
                    chr == '\'' || // NOI18N
                    chr == ' ' || // NOI18N
                    chr == ']' || // NOI18N
                    chr == '}' || // NOI18N
                    chr == '\n' || // NOI18N
                    chr == '\t' || // NOI18N
                    chr == ';'); // NOI18N
        }
//        return true;
    }

    private static boolean isQuoteCompletablePosition(BaseDocument doc, int dotPos)
            throws BadLocationException {
        if (dotPos == doc.getLength()) // there's no other character to test
            return true;
        else {
            // test that we are in front of ) , " or ' ... etc.
            int eol = Utilities.getRowEnd(doc, dotPos);
            if (dotPos == eol || eol == -1) {
                return false;
            }
            int firstNonWhiteFwd = Utilities.getFirstNonWhiteFwd(doc, dotPos, eol);
            if (firstNonWhiteFwd == -1) {
                return false;
            }
            char chr = doc.getChars(firstNonWhiteFwd, 1)[0];
            return (chr == ')' || // NOI18N
                    chr == ',' || // NOI18N
                    chr == '+' || // NOI18N
                    chr == '}' || // NOI18N
                    chr == ';'); // NOI18N
        }
    }

    // move to fxd specific BracesMatcher implementation if will create id.
    private static FXDTokenId getOpositeBracket(FXDTokenId tokenId, boolean isLeftToken) {
        for (int i = (isLeftToken ? 0 : 1); i < PAIR_BRACES_TOKEN_IDS.length; i += 2) {
            if (PAIR_BRACES_TOKEN_IDS[i] == tokenId) {
                return PAIR_BRACES_TOKEN_IDS[(isLeftToken ? i + 1 : i - 1)];
            }
        }
        return null;
    }

    /**
     * returns char corresponding to specified bracket token id.
     * e.g. '{' for FXDTokenId.LBRACE.
     * <p>
     * supported:'{', '}', '[', ']', '(', ')'.

     * @param id
     * @return char or whitespace if specified id is not supported bracket.
     */
    static char getBracketChar(TokenId id){
        if (FXDTokenId.LBRACE == id) {
            return '{';
        } else if (FXDTokenId.RBRACE == id) {
            return '}';
        } else if (FXDTokenId.LBRACKET == id) {
            return '[';
        } else if (FXDTokenId.RBRACKET == id) {
            return ']';
        } else if (FXDTokenId.LPAREN == id) {
            return '(';
        } else if (FXDTokenId.RPAREN == id) {
            return ')';
        }
        return ' ';
    }

    /**
     * Returns for an opening bracket or quote the appropriate closing
     * character.
     *
     * @param bracket bracket to match
     * @return matching opposite bracket
     */
    private static char matching(char bracket) {
        switch (bracket) {
            case '(': // NOI18N
                return ')'; // NOI18N
            case '[': // NOI18N
                return ']'; // NOI18N
            case '\"': // NOI18N
                return '\"'; // NOI18N
            case '\'': // NOI18N
                return '\''; // NOI18N
            case '{': // NOI18N
                return '}'; // NOI18N
            default:
                return ' '; // NOI18N
        }
    }


}
