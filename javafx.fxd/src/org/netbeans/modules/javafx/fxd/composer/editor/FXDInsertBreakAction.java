/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
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
 * Portions Copyrighted 1997-2008 Sun Microsystems, Inc.
 */
package org.netbeans.modules.javafx.fxd.composer.editor;

import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.BaseKit;
import org.netbeans.modules.editor.indent.api.IndentUtils;
import org.openide.util.Exceptions;

import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.JTextComponent;
import java.util.logging.Logger;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @author Andrey Korostelev
 * @todo documentation
 */
 // TODO: support [] the same way as {}
public class FXDInsertBreakAction extends BaseKit.InsertBreakAction {

    static final long serialVersionUID = -1506173310438326380L;

    private static Logger log = Logger.getLogger(FXDInsertBreakAction.class.getName());

    @Override
    protected Object beforeBreak(JTextComponent target, BaseDocument doc, Caret caret) {
        int dotPos = caret.getDot();
        try {
            int lineStart = IndentUtils.lineStartOffset(doc, dotPos);
            int lineIndent = IndentUtils.lineIndent(doc, lineStart);
            if (BracketCompletion.posWithinString(doc, dotPos)) {
                try {
                    // do check before updating document
                    boolean afterEscape = isAfterStringEscape(doc, dotPos); 
                    doc.insertString(dotPos, "\\", null);                       // NOI18N
                    if (!afterEscape){
                        dotPos += 1;
                    }
                    caret.setDot(dotPos);
                    return dotPos + 1 + lineIndent;
                } catch (BadLocationException ex) {
                    log.severe("Exception thrown during InsertBreakAction. " + ex);  // NOI18N
                }
            } else {
                FXDTokenId rightBracket = BracketCompletion.isAddRightBracket(doc, dotPos);
                if (rightBracket != null) {
                    char bracketChar = BracketCompletion.getBracketChar(rightBracket);
                    int innerLineIndent = lineIndent + IndentUtils.indentLevelSize(doc);
                    StringBuilder sb = createAdditiveString(doc, lineIndent, String.valueOf(bracketChar));
                    doc.insertString(dotPos, sb.toString(), null);
                    return dotPos + 1 + innerLineIndent;
                } 
            }
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
        return this.commentBlockCompletion(target, doc, dotPos);
    }

    private boolean isAfterStringEscape(BaseDocument doc, int dotPos)
            throws BadLocationException {
        int lineEnd = Utilities.getRowEnd(doc, dotPos);
        if (dotPos != lineEnd){
            return false;
        }
        int escapesCnt = 0;
        while (doc.getChars(dotPos - 1, 1)[0] == '\\' ){
            dotPos--;
            escapesCnt++;
        }
        return (escapesCnt % 2 != 0);
    }

    private StringBuilder createAdditiveString(BaseDocument doc, int baseIndent, String closingString) {
        StringBuilder sb = new StringBuilder("\n"); // NOI18N
        sb.append(IndentUtils.createIndentString(doc, baseIndent + IndentUtils.indentLevelSize(doc)));
        sb.append("\n"); // NOI18N
        sb.append(IndentUtils.createIndentString(doc, baseIndent)).append(closingString);
        return sb;
    }

    @Override
    protected void afterBreak(JTextComponent target, BaseDocument doc, Caret caret, Object cookie) {
        if (cookie != null) {
            if (cookie instanceof Integer) {
                // integer
                int nowDotPos = (Integer) cookie;
                caret.setDot(nowDotPos);
            }
        }
    }

    private Object commentBlockCompletion(JTextComponent target, BaseDocument doc, final int dotPosition) {
        try {
            TokenHierarchy<BaseDocument> tokens = TokenHierarchy.get(doc);
            TokenSequence<?> ts = tokens.tokenSequence();
            ts.move(dotPosition);
            if (!((ts.moveNext() || ts.movePrevious()) 
                    && (ts.token().id() == FXDTokenId.COMMENT || ts.token().id() == FXDTokenId.UNKNOWN))) {
                return null;
            }

            int jdoffset = dotPosition - 2;
            if (jdoffset >= 0) {
                CharSequence content = org.netbeans.lib.editor.util.swing.DocumentUtilities.getText(doc);
                if (isOpenComment(content, dotPosition - 1) && !isClosedComment(content, dotPosition)) {
                    // complete open comment
                    // note that the formater will add one line of comment
                    doc.insertString(dotPosition, "*/", null); // NOI18N
                    doc.getFormatter().indentNewLine(doc, dotPosition);
                    target.setCaretPosition(dotPosition);

                    return Boolean.TRUE;
                }
            }
        } catch (BadLocationException ex) {
            // ignore
            Exceptions.printStackTrace(ex);
        }
        return null;
    }

    private static boolean isOpenComment(CharSequence content, int pos) {
        for (int i = pos; i >= 0; i--) {
            char c = content.charAt(i);
            if (c == '*' && i - 1 >= 0 && content.charAt(i - 1) == '/') { // NOI18N
                // matched /**
                return true;
            } else if (c == '\n') { // NOI18N
                // no comment, matched start of line
                return false;
            } else if (c == '/' && i - 1 >= 0 && content.charAt(i - 1) == '*') { // NOI18N
                // matched comment enclosing tag
                return false;
            }
        }

        return false;
    }

    private static boolean isClosedComment(CharSequence txt, int pos) {
        int length = txt.length();
        int quotation = 0;
        for (int i = pos; i < length; i++) {
            char c = txt.charAt(i);
            if (c == '*' && i < length - 1 && txt.charAt(i + 1) == '/') { // NOI18N
                if (quotation == 0 || i < length - 2) {
                    return true;
                }
                // guess it is not just part of some text constant
                boolean isClosed = true;
                for (int j = i + 2; j < length; j++) {
                    char cc = txt.charAt(j);
                    if (cc == '\n') { // NOI18N
                        break;
                    } else if (cc == '"' && j < length - 1 && txt.charAt(j + 1) != '\'') { // NOI18N
                        isClosed = false;
                        break;
                    }
                }

                if (isClosed) {
                    return true;
                }
            } else if (c == '/' && i < length - 1 && txt.charAt(i + 1) == '*') { // NOI18N
                // start of another comment block
                return false;
            } else if (c == '\n') { // NOI18N
                quotation = 0;
            } else if (c == '"' && i < length - 1 && txt.charAt(i + 1) != '\'') { // NOI18N
                quotation = ++quotation % 2;
            }
        }

        return false;
    }

}
