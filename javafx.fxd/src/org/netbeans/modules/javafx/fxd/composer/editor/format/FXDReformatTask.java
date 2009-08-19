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
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2008 Sun
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

import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.Position;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.editor.indent.api.IndentUtils;
import org.netbeans.modules.editor.indent.spi.Context;
import org.netbeans.modules.editor.indent.spi.ExtraLock;
import org.netbeans.modules.editor.indent.spi.ReformatTask;
import org.netbeans.modules.javafx.fxd.composer.editor.BracketCompletion;
import org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId;

/**
 *
 * @author Andrey Korostelev
 */
public class FXDReformatTask implements ReformatTask {
    private final Context context;

    private static Logger logger = Logger.getLogger( "org.netbeans.modules.javafx.fxd.composer.editor.format" ); // NOI18N

    public FXDReformatTask(Context context) {
        this.context = context;
    }

    public void reformat() throws BadLocationException {
        final Document document = context.document();
        final BaseDocument bd = (BaseDocument)document;
        bd.runAtomic( new Runnable() {

            public void run() {
                try {
                    final Queue<Adjustment> adjustments = new LinkedList<Adjustment>();
                    List<Context.Region> regions = context.indentRegions();
                    TokenSequence<FXDTokenId> ts = BracketCompletion.getTokenSequence(bd, 0);

                    for (Context.Region region : regions) {
                        int startOffset = region.getStartOffset();
                        int endOffset = region.getEndOffset();
                        ts.move(startOffset);
                        Formatter formatter = new Formatter(context);
                        formatter.scan(adjustments, startOffset, endOffset);
                    }
                    applyAdjustments(adjustments);
                } catch( BadLocationException e ) {
                    logger.log( Level.FINE, e.getLocalizedMessage());
                }
            }
        });
    }

    private void applyAdjustments(Queue<Adjustment> adjustments) throws BadLocationException {
        if (adjustments == null || adjustments.isEmpty()) return;
        logger.info("Applying " + adjustments.size() + " adjustments into source code."); // NOI18N
        while (!adjustments.isEmpty()) {
            final Adjustment adjustment = adjustments.poll();
            adjustment.apply(context);
        }
    }

    private static class Formatter{

        private static final String NEW_LINE_STRING = "\n"; // NOI18N
        private static final int MULTILINE_TOKEN_INDENT_STEPS = 2;

        private final Context m_context;
        private final BaseDocument m_baseDoc;
        private final int m_docIndent;
        private TokenSequence<FXDTokenId> m_ts;
        Queue<Adjustment> m_adjustments;
        private int m_currIndent;
        private int m_rowStart;
        private int m_startOffset;
        private int m_endOffset;
        private int m_lastIndentedRow;

        public Formatter(Context context){
            m_context = context;
            m_baseDoc = (BaseDocument)context.document();
            m_docIndent = IndentUtils.indentLevelSize(m_baseDoc);
        }

        protected void scan(Queue<Adjustment> adjustments, int startOffset, int endOffset) throws BadLocationException{
            m_adjustments = adjustments;
            m_startOffset = startOffset;
            m_endOffset = endOffset;
            m_ts = BracketCompletion.getTokenSequence(m_baseDoc, m_startOffset);
            m_currIndent = initialIndent();
            process();
        }

        private void process() throws BadLocationException{
            Token<FXDTokenId> token = FormatterUtilities.getNextNonWhiteFwd(m_ts);
            Element rootElement = m_baseDoc.getDefaultRootElement();
            //while ((token = FormatterUtilities.getNextNonWhiteFwd(m_ts)) != null){
            while (token != null){
                // indent all white rows before token
                int currRowStart = IndentUtils.lineStartOffset(m_baseDoc, m_ts.offset());
                if (m_rowStart < currRowStart ){
                    indentLines(m_rowStart, currRowStart, rootElement);
                }

                // format before endOffset only
                if (m_rowStart > m_endOffset){
                    return;
                }

                // process left brackets
                if (FormatterUtilities.isLBracketToken(token)) {
                    indentLine(m_rowStart);
                    if ((token = FormatterUtilities.getNextNonWhiteFwd(m_ts)) != null) {
                        //if (!FormatterUtilities.isRBracketToken(token)) {
                        m_currIndent = incIndent(m_currIndent); 
                        //}
                        int nextTokenRowStart = IndentUtils.lineStartOffset(m_baseDoc, m_ts.offset());
                        if (nextTokenRowStart == m_rowStart && !FormatterUtilities.isRBracketToken(token)){
                            m_adjustments.offer(Adjustment.add(createPosition(m_ts.offset()), NEW_LINE_STRING));
                            m_adjustments.offer(Adjustment.indent(createPosition(m_ts.offset() + 1), m_currIndent));
                        } // TODO else {m_rowStart = nextTokenRowStart;}
                        //m_adjustments.offer(Adjustment.indent(createPosition(offset + 1), m_currIndent));
                        continue;
                    }
                }
                    // process right brackets
                if (FormatterUtilities.isRBracketToken(token)){
                    m_currIndent = decIndent(m_currIndent);
                    // intend rbracket line only after decreasing indent size
                    indentLine(m_rowStart);
                    if (addBreakBeforeTokenIfNeed()){
                        m_adjustments.offer(Adjustment.indent(createPosition(m_ts.offset() + 1), m_currIndent));
                        //indentLine(currRowStart);
                    }

                    if ((token = FormatterUtilities.getNextNonWhiteFwd(m_ts)) != null) {
                        if (!FormatterUtilities.isRBracketToken(token)
                                && !FormatterUtilities.isAttribsSeparatorToken(token)) {
                            if (addBreakBeforeTokenIfNeed()) {
                                m_adjustments.offer(Adjustment.indent(createPosition(m_ts.offset() + 1), m_currIndent));
                            }
                        }
                    }
                    continue;
                }
                indentLine(m_rowStart);
                // long -> format line-by-line
                int tokenEndRowStart = IndentUtils.lineStartOffset(m_baseDoc, m_ts.offset()+token.length()-1);
                if (tokenEndRowStart != m_rowStart){
                    m_currIndent = incIndent(m_currIndent, MULTILINE_TOKEN_INDENT_STEPS);
                    indentLines(m_rowStart, tokenEndRowStart, rootElement);
                    indentLine(tokenEndRowStart);
                    m_currIndent = decIndent(m_currIndent, MULTILINE_TOKEN_INDENT_STEPS);

                }
                token = FormatterUtilities.getNextNonWhiteFwd(m_ts);
            }
        }

        /**
         * @return true if break was added. false otherwise.
         * @throws BadLocationException
         */
        private boolean addBreakBeforeTokenIfNeed() throws BadLocationException{
            int tokenRowStart = IndentUtils.lineStartOffset(m_baseDoc, m_ts.offset());
            if (tokenRowStart == m_rowStart) {
                int prevTokenRowStart = getPrevNotWhiteTokenRowStart();
                if(prevTokenRowStart == tokenRowStart){
                    m_adjustments.offer(Adjustment.add(createPosition(m_ts.offset()), NEW_LINE_STRING));
                    return true;
                }
            }
            return false;
        }

        private int getPrevNotWhiteTokenRowStart() throws BadLocationException{
            FormatterUtilities.getNextNonWhiteBwd(m_ts);
            int prevTokenRowStart = IndentUtils.lineStartOffset(m_baseDoc, m_ts.offset());
            FormatterUtilities.getNextNonWhiteFwd(m_ts);
            return prevTokenRowStart;
        }

        private int initialIndent() throws BadLocationException{
            int rowFirstNonWhite = Utilities.getRowFirstNonWhite(m_baseDoc, m_startOffset);
            m_rowStart = IndentUtils.lineStartOffset(m_baseDoc, m_startOffset);
            if ( rowFirstNonWhite >= m_startOffset){
                return FormatterUtilities.calculateLineIndent(m_baseDoc, m_startOffset);
            } else {
                return FormatterUtilities.getCurrentLineIndent(m_baseDoc, m_startOffset);
            }
        }

        private int incIndent(int old){
            return incIndent(old, 1);
        }

        /**
         * @param old old indentation value
         * @param incStep how much dedfault indentation steps should be added
         * @return
         */
        private int incIndent(int old, int incSteps){
            return old + m_docIndent * incSteps;
        }

        private int decIndent(int old){
            return decIndent(old, 1);
        }

        /**
         * @param old old indentation value
         * @param incStep how much default indentation steps should be removed
         * @return
         */
        private int decIndent(int old, int decSteps){
            int newIndent = old - m_docIndent * decSteps;
            return newIndent >= 0 ? newIndent : 0;
        }

        private void indentLines(int firstRowStart, int lastRowStart, Element rootElement)
                throws BadLocationException{
            while (firstRowStart < lastRowStart && firstRowStart <= m_endOffset) {
                // indent current line
                indentLine(firstRowStart);
                // retrieve the next line
                int elementIndex = rootElement.getElementIndex(firstRowStart);
                Element nextElement = rootElement.getElement(elementIndex + 1);
                firstRowStart = IndentUtils.lineStartOffset(m_baseDoc, nextElement.getStartOffset());
            }
            m_rowStart = lastRowStart;
        }

        private void indentLine(int lineStart) throws BadLocationException {
            indentLine(lineStart, m_currIndent);
        }

        private void indentLine(int lineStart, int indent) throws BadLocationException {
            if (lineStart >= m_startOffset && lineStart != m_lastIndentedRow){
                m_lastIndentedRow = lineStart;
                if (m_context.lineIndent(m_context.lineStartOffset(lineStart)) != indent) {
                    m_adjustments.offer(Adjustment.indent(createPosition(lineStart), indent));
                }
            }
        }

        private Position createPosition(int offset) throws BadLocationException {
            return m_context.document().createPosition(offset);
        }
    }


    public ExtraLock reformatLock() {
        return null;
    }

}
