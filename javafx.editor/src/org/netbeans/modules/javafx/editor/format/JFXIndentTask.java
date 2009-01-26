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

package org.netbeans.modules.javafx.editor.format;

import com.sun.javafx.api.tree.FunctionValueTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.javafx.source.TreeUtilities;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.lexer.TokenId;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.editor.BaseDocument;
import org.netbeans.modules.editor.indent.spi.Context;
import org.netbeans.modules.editor.indent.spi.ExtraLock;
import org.netbeans.modules.editor.indent.spi.IndentTask;
import org.netbeans.modules.editor.indent.spi.ReformatTask;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Position;
import java.io.IOException;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

/**
 * @author Rastislav Komara (<a href="mailto:rastislav.komara@sun.com">RKo</a>)
 */
public class JFXIndentTask implements IndentTask, ReformatTask {
    private static Logger log = Logger.getLogger(JFXIndentTask.class.getName());
    private static final Pattern KEEP_LEVEL_PTRN = Pattern.compile("\\s*(}|\\)|\\])\\s*(;|,)?\\s*");

    private final Context context;
    private TokenSequence<JFXTokenId> ts = null;
    private static final int ONE = 1;
    private static final int ZERO = 0;


    public JFXIndentTask(Context context) {
        this.context = context;
    }

    /**
     * Perform reindentation of the line(s) of {@link org.netbeans.modules.editor.indent.spi.Context#document()}
     * between {@link org.netbeans.modules.editor.indent.spi.Context#startOffset()} and {@link org.netbeans.modules.editor.indent.spi.Context#endOffset()}.
     * <br/>
     * It is called from AWT thread and it should process synchronously. It is used
     * after a newline is inserted after the user presses Enter
     * or when a current line must be reindented e.g. when Tab is pressed in emacs mode.
     * <br/>
     * The method should use information from the context and modify
     * indentation at the given offset in the document.
     *
     * @throws javax.swing.text.BadLocationException
     *          in case the indent task attempted to insert/remove
     *          at an invalid offset or e.g. into a guarded section.
     */
    public void reindent() throws BadLocationException {
//        if (context == null) {
//            return;
//        }
//        if (log.isLoggable(Level.FINE)) log.fine("Reindent...");
//
//        final List<Context.Region> regions = context.indentRegions();
//        if (!regions.isEmpty()) {
//            for (Context.Region region : regions) {
//                if (log.isLoggable(Level.FINE))
//                    log.fine("\tRegion: [" + region.getStartOffset() + "," + region.getEndOffset() + "]");
//                indentRegion(region);
//            }
//        } else {
//            if (log.isLoggable(Level.FINE))
//                log.fine("\tLine: [" + context.startOffset() + "," + context.endOffset() + "]");
//            indentLine(context.startOffset());
//        }
//        if (log.isLoggable(Level.FINE)) log.fine("... done!");
        reformat();
    }

    private void indentLine(int offset) throws BadLocationException {
        int lso = context.lineStartOffset(offset);
        int si = getScopeIndent(context.document(), lso) + indentComment(offset, lso);
        if (context.lineIndent(lso) != si) {
            context.modifyIndent(lso, si);
        }
    }

    private int indentComment(int offset, int lso) {
        ts().move(offset);
        Token<JFXTokenId> t = ts().moveNext() ? ts().token() : null;
        if (t != null && JFXTokenId.isComment(t.id())
                && ts().offset() < lso
                && (ts().offset() + t.length()) > lso) {
            return ONE;

        }
        return ZERO;
    }

    private void indentRegion(Context.Region region) throws BadLocationException {
        int offset = region.getStartOffset();
        do {
            indentLine(offset);
            offset = adjustOffsetToNewLine(offset, context.lineStartOffset(offset));
        } while (offset < region.getEndOffset());
    }

    private int adjustOffsetToNewLine(int offset, int lso) throws BadLocationException {
        while (lso == context.lineStartOffset(offset)
                && offset < context.endOffset()) {
            offset++;
        }
        return offset;
    }

    private TokenSequence<JFXTokenId> ts() {
        if (ts != null && ts.isValid()) return ts;
        final BaseDocument doc = (BaseDocument) context.document();
        this.ts = getTokenSequence(doc, this.ts != null ? ts.offset() : context.startOffset());
        return this.ts;
    }

    private int getIndentStepLevel() {
        return 4;
    }

//    private <T> T getSetting(String settingName, Object defVal) {
//        return (T) SettingsUtil.getValue(JavaFXEditorKit.class, settingName, defVal);
//    }

    /**
     * Get an extra locking or null if no extra locking is necessary.
     */
    public ExtraLock indentLock() {
        return null;
    }


    private int getScopeIndent(Document document, int startOffset) throws BadLocationException {
        final Position position = document.getStartPosition();
        if (position.getOffset() == startOffset) {
            return ZERO;
        } else {
            // we simply adapt indent of previous line and adjust if necessary.
            int lso = context.lineStartOffset(startOffset);
            int ls = getPreviousLine(lso);
            if (ls == lso) return ZERO;
            int level = context.lineIndent(ls); //previous line indent

///*
            //verify if we need to adjust level in case of previous line.
            ts().move(ls);
            Token<JFXTokenId> t = ts.moveNext() ? ts.token() : null;
            while (t != null && ts.offset() < lso) {
                if (t.id() == JFXTokenId.LBRACE || t.id() == JFXTokenId.LPAREN || t.id() == JFXTokenId.LBRACKET) {
                    level += getIndentStepLevel();
                } else
                if (t.id() == JFXTokenId.RBRACE || t.id() == JFXTokenId.RPAREN || t.id() == JFXTokenId.RBRACKET) {
                    level -= getIndentStepLevel();
                } else if (t.id() == JFXTokenId.COMMENT || t.id() == JFXTokenId.DOC_COMMENT) {
                    level -= getIndentStepLevel();
                }
                t = ts.moveNext() ? ts.token() : null;
            }

            // Handle special cases. This cases are handled with small guessing.
            int nlo = adjustOffsetToNewLine(lso, lso); //start offset of next line
            if (KEEP_LEVEL_PTRN.matcher(document.getText(lso, nlo - lso)).matches()) {
                // if current line is "closing" line move it -1 level.
//                level -= getIndentStepLevel();
            } else if (KEEP_LEVEL_PTRN.matcher(document.getText(ls, lso - ls)).matches()) {
                // if previous line is "closing" line move this +1 level.
//                level += getIndentStepLevel();
            } else {
                final String previousLineText = document.getText(ls, lso - ls);
                if (previousLineText.trim().endsWith(":")
                        || previousLineText.trim().endsWith("=")) {
                    level += getIndentStepLevel();
                }
            }
            //if we got buggy source code we descent only to zero indent.
            return Math.max(ZERO, level);
        }
    }

    /**
     * Escaping single newline lines.
     *
     * @param lso start of the origin line
     * @return new nonempty line
     * @throws BadLocationException if something goes wrong.
     */
    private int getPreviousLine(int lso) throws BadLocationException {
        while (lso > ZERO) {
            int ls = context.lineStartOffset(Math.max(ZERO, lso - ONE));
            if (lso - ls > ONE) {
                return ls;
            } else {
                lso = ls;
            }
        }
        return ZERO;
    }

    @SuppressWarnings("unchecked")
    private static <T extends TokenId> TokenSequence<T> getTokenSequence(BaseDocument doc, int dotPos) {
        TokenHierarchy<BaseDocument> th = TokenHierarchy.get(doc);
        TokenSequence<T> seq = (TokenSequence<T>) th.tokenSequence();
        seq.move(dotPos);
        return seq;
    }


    /**
     * Perform reformatting of the {@link org.netbeans.modules.editor.indent.spi.Context#document()}
     * between {@link org.netbeans.modules.editor.indent.spi.Context#startOffset()} and {@link org.netbeans.modules.editor.indent.spi.Context#endOffset()}.
     * <br/>
     * This method may be called several times repetitively for different areas
     * of a reformatted area.
     * <br/>
     * It is called from AWT thread and it should process synchronously. It is used
     * after a newline is inserted after the user presses Enter
     * or when a current line must be reindented e.g. when Tab is pressed in emacs mode.
     * <br/>
     * The method should use information from the context and modify
     * indentation at the given offset in the document.
     *
     * @throws javax.swing.text.BadLocationException
     *          in case the formatter attempted to insert/remove
     *          at an invalid offset or e.g. into a guarded section.
     */
    public void reformat() throws BadLocationException {
        reformat(context.indentRegions());
    }

    public void reformat(final List<Context.Region> regions) throws BadLocationException {
        final JavaFXSource s = JavaFXSource.forDocument(context.document());
        try {
            s.runUserActionTask(new Task<CompilationController>() {
                @SuppressWarnings({"MethodWithMultipleLoops", "OverlyComplexMethod"}) // NOI18N
                public void run(CompilationController controller) throws Exception {
                    final long s = System.currentTimeMillis();
                    final JavaFXSource.Phase phase = controller.toPhase(JavaFXSource.Phase.PARSED);
                    if (log.isLoggable(Level.INFO))
                        log.info("Parser time: " + (System.currentTimeMillis() - s) + "ms");
                    if (phase.compareTo(JavaFXSource.Phase.PARSED) >= ZERO) {
                        if (log.isLoggable(Level.INFO))
                            log.info("The " + phase + " phase has been reached ... OK!");


                        final Queue<Adjustment> adjustments;
//                        adjustments = new LinkedList<Adjustment>();
                        adjustments = new MaskedQueue();
                        for (Context.Region region : regions) {
                            if (log.isLoggable(Level.INFO))
                                log.info("Region: [" + region.getStartOffset() + "," + region.getEndOffset() + "]");
                            final int offset = region.getStartOffset();
                            final TreeUtilities tu = controller.getTreeUtilities();
                            JavaFXTreePath path;
                            if (offset > 0) {
                                path = tu.pathFor(offset);
                                if (path.getLeaf() instanceof FunctionValueTree) {
                                    path = path.getParentPath();
                                }
                            } else {
                                path = tu.pathFor(offset);
                                while (path.getParentPath() != null) {
                                    path = path.getParentPath();
                                }
                            }                            
                            final int position = (int) controller.getTrees().getSourcePositions()
                                    .getStartPosition(controller.getCompilationUnit(), path.getLeaf());
                            int dot = offset == ZERO ? ZERO : position < ZERO ? ZERO : context.lineIndent(context.lineStartOffset(position));
                            Visitor visitor = new Visitor(controller, context, dot);
                            visitor.scan(path, adjustments);
                        }
                        applyAdjustments(adjustments);
                    }
                }
            }, true);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private class MaskedQueue implements Queue<Adjustment> {
        private final Queue<Adjustment> queue = new LinkedList<Adjustment>();

        public int size() {
            return queue.size();
        }

        public boolean isEmpty() {
            return queue.isEmpty();
        }

        public boolean contains(Object o) {
            return queue.contains(o);
        }

        public Iterator<Adjustment> iterator() {
            return queue.iterator();
        }

        public Object[] toArray() {
            return queue.toArray();
        }

        public <T> T[] toArray(T[] a) {
            return queue.toArray(a);
        }

        public boolean remove(Object o) {
            return queue.remove(o);
        }

        public boolean containsAll(Collection<?> c) {
            return queue.containsAll(c);
        }

        public boolean addAll(Collection<? extends Adjustment> c) {
            Thread.dumpStack();
            return queue.addAll(c);
        }

        public boolean removeAll(Collection<?> c) {
            return queue.removeAll(c);
        }

        public boolean retainAll(Collection<?> c) {
            return queue.retainAll(c);
        }

        public void clear() {
            queue.clear();
        }

        public boolean equals(Object o) {
            return queue.equals(o);
        }

        public int hashCode() {
            return queue.hashCode();
        }

        public boolean add(Adjustment adjustment) {
            dumpStack();
            return queue.add(adjustment);
        }

        public boolean offer(Adjustment adjustment) {
            dumpStack();
            return queue.offer(adjustment);
        }

        public Adjustment remove() {
            return queue.remove();
        }

        public Adjustment poll() {
            return queue.poll();
        }

        public Adjustment element() {
            return queue.element();
        }

        public Adjustment peek() {
            return queue.peek();
        }

        private void dumpStack() {
            Exception e = new Exception("-------  StackTrace ---------- ");
            StackTraceElement[] ste = e.getStackTrace();
            System.err.println(e.getMessage());
            for (int i = 0; i < Math.min(10, ste.length); i++) {
                System.err.println("\t" + ste[i]);
            }
        }
    }

    private void applyAdjustments(Queue<Adjustment> adjustments) throws BadLocationException {
        if (adjustments == null || adjustments.isEmpty()) return;
        log.info("Applying " + adjustments.size() + " adjustments into source code.");
        while (!adjustments.isEmpty()) {
            final Adjustment adjustment = adjustments.poll();
            adjustment.apply(context);
        }
    }

    /**
     * Get an extra locking or null if no extra locking is necessary.
     */
    public ExtraLock reformatLock() {
        return null;
    }

}

