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
import org.netbeans.modules.editor.indent.api.Indent;
import org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @author Andrey Korostelev
 * @todo documentation
 */
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
                    return Boolean.TRUE;
                } catch (BadLocationException ex) {
                    log.severe("Exception thrown during InsertBreakAction. " + ex);  // NOI18N
                }
            } else {
                FXDTokenId rightBracket = BracketCompletion.isAddRightBracket(doc, dotPos);
                if (rightBracket != null) {
                    char bracketChar = BracketCompletion.getBracketChar(rightBracket);
                    StringBuilder sb = createAdditiveString(doc, lineIndent, String.valueOf(bracketChar));
                    doc.insertString(dotPos, sb.toString(), null);
                    return IndentUtils.indentLevelSize(doc);
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
                int moveDotPos = (Integer) cookie;
                caret.getDot();
                caret.setDot(caret.getDot() + moveDotPos);
            }
        }
    }

    private Object commentBlockCompletion(JTextComponent target, BaseDocument doc, final int dotPosition) {
        try {
            int jdoffset = dotPosition - 2;
            if (jdoffset >= 0) {
                CharSequence content = org.netbeans.lib.editor.util.swing.DocumentUtilities.getText(doc);
                if(insideComment(doc, dotPosition, content)){
                    if (!isClosedComment(content, dotPosition)){
                        doc.insertString(dotPosition, "\n*/", null); // NOI18N
                        indentLine(doc, dotPosition + 1);
                    }
                    doc.insertString(dotPosition, "* ", null); // NOI18N

                    target.setCaretPosition(dotPosition);
                    return 2; // caret should be after '* ' - move on 2 chars
                }
            }
        } catch (BadLocationException ex) {
            // ignore
            Exceptions.printStackTrace(ex);
        }
        return null;
    }

    private boolean insideComment(BaseDocument doc, final int dotPosition, CharSequence content) {
        TokenSequence ts = BracketCompletion.getTokenSequence(doc, dotPosition);
        if (!(ts.moveNext() || ts.movePrevious())) {
            return false;
        }
        if (ts.token().id() == FXDTokenId.COMMENT && dotPosition >= ts.offset() + 2) { // dot after '/*'
            return true;
        }
        if (ts.token().id() == FXDTokenId.UNKNOWN && isOpenComment(content, dotPosition - 1)) {
            return true;
        }
        return false;
    }

    private static void indentLine(BaseDocument doc, final int offset) throws BadLocationException {
        final Indent indent = Indent.get(doc);
        doc.runAtomic(new Runnable() {

            public void run() {
                try {
                    indent.reindent(offset);
                } catch (BadLocationException ex) {
                    Exceptions.printStackTrace(ex);
                }
            }
        });
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
