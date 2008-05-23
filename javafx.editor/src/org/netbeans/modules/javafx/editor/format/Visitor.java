/*
 *
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
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
 *
 */

package org.netbeans.modules.javafx.editor.format;

import com.sun.javafx.api.tree.*;
import com.sun.source.tree.*;
import com.sun.source.util.SourcePositions;
import com.sun.source.util.TreePath;
import org.netbeans.api.java.source.CodeStyle;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.TreeUtilities;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.api.project.Project;
import org.netbeans.modules.editor.indent.spi.Context;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @todo documentation
 */
class Visitor extends JavaFXTreePathScanner<List<Adjustment>, List<Adjustment>> {
    private static Logger log = Logger.getLogger(Visitor.class.getName());
    private final TreeUtilities tu;
    private final CompilationInfo info;
    private final Context ctx;
    private int indentOffset = 0;
    private final CodeStyle cs;
    private static final String NEW_LINE_STRING = System.getProperty("line.separator", "\n");
    protected final DocumentLinesIterator li;


    Visitor(CompilationInfo info, Context ctx, int startOffset, Project project) {
        this(info, ctx, project);
        indentOffset = startOffset;
    }

    Visitor(CompilationInfo info, Context ctx, Project project) {
        this.info = info;
        this.ctx = ctx;
        tu = new TreeUtilities(info);
        cs = CodeStyle.getDefault(project);
        li = new DocumentLinesIterator(ctx);
    }

    private int getIndentStepLevel() {
        return 4;
    }

    @Override
    public List<Adjustment> visitClass(ClassTree classTree, List<Adjustment> adjustments) {
        return super.visitClass(classTree, adjustments);
    }

    private void indentLine(Element line, List<Adjustment> adjustments) throws BadLocationException {
        final int ls = ctx.lineStartOffset(line.getStartOffset());
        indentLine(ls, adjustments);
    }

    private void indentLine(int ls, List<Adjustment> adjustments) throws BadLocationException {
        if (ctx.lineIndent(ls) != indentOffset) {
            adjustments.add(Adjustment.indent(ctx.document().createPosition(ls), indentOffset));
        }
    }


    @Override
    public List<Adjustment> visitObjectLiteralPart(ObjectLiteralPartTree objectLiteralPartTree, List<Adjustment> adjustments) {
        try {
            SourcePositions sps = info.getTrees().getSourcePositions();
            final int offset = (int) sps.getStartPosition(info.getCompilationUnit(), objectLiteralPartTree);
            if (isMultiline(objectLiteralPartTree)) {
                li.moveTo(offset);
                final Element element = li.get();
                if (isFirstOnLine(offset)) {
                    indentLine(element, adjustments);
                }
                indentMultiline(li, (int) sps.getEndPosition(info.getCompilationUnit(), objectLiteralPartTree), adjustments);
            } else {
                if (isFirstOnLine(offset)) {
                    indentLine(offset, adjustments);
                }
            }
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
        return super.visitObjectLiteralPart(objectLiteralPartTree, adjustments);
    }

    private void indentMultiline(LineIterator<Element> li, int endOffset, List<Adjustment> adjustments) throws BadLocationException {
        final Document doc = ctx.document();
        if (li.hasNext()) {
            indentOffset = indentOffset + cs.getContinuationIndentSize();
            while (li.hasNext()) {
                final Element element = li.next();
                if (element.getStartOffset() > endOffset) {
                    break;
                }
                indentLine(element, adjustments);
            }
            indentOffset = indentOffset - cs.getContinuationIndentSize();
        }
    }

    private boolean isMultiline(Tree tree) throws BadLocationException {
        SourcePositions sps = info.getTrees().getSourcePositions();
        return ctx.lineStartOffset((int) sps.getStartPosition(info.getCompilationUnit(), tree))
                != ctx.lineStartOffset((int) sps.getEndPosition(info.getCompilationUnit(), tree));
    }

    @Override
    public List<Adjustment> visitClassDeclaration(ClassDeclarationTree cdt, List<Adjustment> adj) {
        if (tu.isSynthetic(getCurrentPath())) {
            return adj;
        }
        SourcePositions sps = info.getTrees().getSourcePositions();
        final Document doc = ctx.document();
        try {
            int pos = ctx.lineStartOffset((int) sps.getStartPosition(info.getCompilationUnit(), getCurrentPath().getLeaf()));
            if (indentOffset != ctx.lineIndent(pos)) {
                adj.add(Adjustment.indent(doc.createPosition(pos), indentOffset));
            }
            int elc = cs.getBlankLinesBeforeClass();

            int emptyLines = getEmptyLinesBefore(li, pos);

            elc = elc - emptyLines;

            if (elc < 0) {
                Element nth = getNthElement(Math.abs(elc), li);
                if (nth != null) {
                    adj.add(Adjustment.delete(doc.createPosition(nth.getStartOffset()), doc.createPosition(pos)));
                }
            } else if (elc > 0) {
                final String str = NEW_LINE_STRING;
                StringBuilder sb = buildString(elc, str);
                adj.add(Adjustment.add(doc.createPosition(pos), sb.toString()));
            }

            indentOffset = indentOffset + getIndentStepLevel();

            final List<Adjustment> add = super.visitClassDeclaration(cdt, adj);
            if (add != null) {
                adj.addAll(add);
            }
            indentOffset = indentOffset - getIndentStepLevel();
            pos = ctx.lineStartOffset((int) sps.getEndPosition(info.getCompilationUnit(), getCurrentPath().getLeaf()));
            if (indentOffset != ctx.lineIndent(pos)) {
                adj.add(Adjustment.indent(doc.createPosition(pos), indentOffset));
            }

        } catch (BadLocationException e) {
            if (log.isLoggable(Level.SEVERE)) log.severe("Reformat failed. " + e);
        }
        return adj;
    }

    private Tree firstImport;
    private Tree lastImport;

    @Override
    public List<Adjustment> visitImport(ImportTree importTree, List<Adjustment> adjustments) {
        if (log.isLoggable(Level.FINE)) log.fine("Visiting import" + importTree);
        SourcePositions sps = info.getTrees().getSourcePositions();
        try {
            final int ls = ctx.lineStartOffset((int) sps.getStartPosition(info.getCompilationUnit(), getCurrentPath().getLeaf()));
            if (indentOffset != ctx.lineIndent(ls)) {
                adjustments.add(Adjustment.indent(ctx.document().createPosition(ls), indentOffset));
            }
            if (firstImport == null) {
                firstImport = getFirstImport(getCurrentPath().getParentPath());
            }
            if (lastImport == null) {
                lastImport = getLastImport(getCurrentPath().getParentPath());
            }
            if (importTree.equals(firstImport)) {
                li.moveTo(ls);
                final int lines = getEmptyLinesBefore(li, ls);
                final int linesBeforeImports = cs.getBlankLinesBeforeImports();
                if (linesBeforeImports != lines) {
                    adjustLinesBefore(lines, linesBeforeImports, adjustments, li, ls);
                }
            }
            if (importTree.equals(lastImport)) {

            }

        } catch (BadLocationException e) {
            if (log.isLoggable(Level.SEVERE)) log.severe("Reformat failed. " + e);
        }
        return super.visitImport(importTree, adjustments);
    }

    private Tree getLastImport(TreePath path) {
        Tree res = null;
        for (Tree tree : path) {
            if (tree instanceof ImportTree) {
                res = tree;
            }
        }
        return res;
    }

    private Tree getFirstImport(TreePath path) {
        for (Tree tree : path) {
            if (tree instanceof ImportTree) {
                return tree;
            }
        }
        return null;
    }

    private void adjustLinesBefore(int realLines, int linesRequired, List<Adjustment> list, LineIterator<Element> li, int pos) throws BadLocationException {
        linesRequired = linesRequired - realLines;

        if (linesRequired < 0) {
            Element nth = getNthElement(Math.abs(linesRequired), li);
            if (nth != null) {
                list.add(Adjustment.delete(ctx.document().createPosition(nth.getStartOffset()), ctx.document().createPosition(pos)));
            }
        } else if (linesRequired > 0) {
            StringBuilder sb = buildString(linesRequired, NEW_LINE_STRING);
            list.add(Adjustment.add(ctx.document().createPosition(pos), sb.toString()));
        }
    }

    private boolean isFirstOnLine(int offset) throws BadLocationException {
        final int ls = ctx.lineStartOffset(offset);
        final Document doc = ctx.document();
        final String s = doc.getText(ls, offset - ls);
        return isEmpty(s);

    }

    @Override
    public List<Adjustment> visitReturn(ReturnTree returnTree, List<Adjustment> adjustments) {
        SourcePositions sps = info.getTrees().getSourcePositions();
        final int offset = (int) sps.getStartPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());
        try {
            indentLine(ctx.lineStartOffset(offset), adjustments);
        } catch (BadLocationException e) {
            if (log.isLoggable(Level.SEVERE)) log.severe("Reformat failed. " + e);
        }
        return super.visitReturn(returnTree, adjustments);
    }

    @Override
    public List<Adjustment> visitLiteral(LiteralTree literalTree, List<Adjustment> adjustments) {
        SourcePositions sps = info.getTrees().getSourcePositions();
        final int offset = (int) sps.getStartPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());
        try {
            final int ls = ctx.lineStartOffset(offset);
            final Document doc = ctx.document();
            final String s = doc.getText(ls, offset - ls);
            // we work only with literal which stands alone as "return statement"
            if (isEmpty(s) && ctx.lineIndent(ls) != indentOffset) {
                adjustments.add(Adjustment.indent(doc.createPosition(ls), indentOffset));
            }
        } catch (BadLocationException e) {
            if (log.isLoggable(Level.SEVERE)) log.severe("Reformat failed. " + e);
        }
        return super.visitLiteral(literalTree, adjustments);
    }

    @Override
    public List<Adjustment> visitBlock(BlockTree blockTree, List<Adjustment> adjustments) {
        return super.visitBlock(blockTree, adjustments);
    }

    @Override
    //TODO: [RKo] normalize!
    public List<Adjustment> visitStringExpression(StringExpressionTree stringExpressionTree, List<Adjustment> adjustments) {
        SourcePositions sps = info.getTrees().getSourcePositions();
        try {
            final int offset = (int) sps.getStartPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());
            final int endOffset = (int) sps.getEndPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());
            final int start = ctx.lineStartOffset(offset);
            final Document doc = ctx.document();
            if (ctx.lineIndent(start) != indentOffset) {
                adjustments.add(Adjustment.indent(doc.createPosition(start), indentOffset));
            }
            if (start != ctx.lineStartOffset(endOffset)) {
                li.moveTo(offset);
                if (li.hasNext()) {
                    li.next(); // we just skip first line
                    indentOffset = indentOffset + cs.getContinuationIndentSize();
                    while (li.hasNext()) {
                        final Element element = li.next();
                        if (element.getStartOffset() > endOffset) {
                            break;
                        }
                        final int ls = ctx.lineStartOffset(element.getStartOffset());
                        if (ctx.lineIndent(ls) != indentOffset) {
                            adjustments.add(Adjustment.indent(doc.createPosition(ls), indentOffset));
                        }
                    }
                    indentOffset = indentOffset - cs.getContinuationIndentSize();
                }

            }

        } catch (BadLocationException e) {
            if (log.isLoggable(Level.SEVERE)) log.severe("Reformat failed. " + e);
        }
        return super.visitStringExpression(stringExpressionTree, adjustments);
    }

    @Override
    public List<Adjustment> visitBlockExpression(BlockExpressionTree blockExpressionTree, List<Adjustment> adjustments) {
        try {
            SourcePositions sps = info.getTrees().getSourcePositions();
            final int start = ctx.lineStartOffset((int) sps.getStartPosition(info.getCompilationUnit(), getCurrentPath().getLeaf()));
            final Document doc = ctx.document();
            if (indentOffset != ctx.lineIndent(start)) {
                adjustments.add(Adjustment.indent(doc.createPosition(start), indentOffset));
            }
            indentOffset++;
            super.visitBlockExpression(blockExpressionTree, adjustments);
            indentOffset--;
            final int end = ctx.lineStartOffset((int) sps.getEndPosition(info.getCompilationUnit(), getCurrentPath().getLeaf()));
            if (indentOffset != ctx.lineIndent(end)) {
                adjustments.add(Adjustment.indent(doc.createPosition(end), indentOffset));
            }
        } catch (BadLocationException e) {
            if (log.isLoggable(Level.SEVERE)) log.severe("Reformat failed. " + e);
        }
        return adjustments;
    }

    private int getEmptyLinesBefore(LineIterator<Element> iterator, int pos) throws BadLocationException {
        final TreePath parent = getCurrentPath().getParentPath();
        final TokenSequence<JFXTokenId> ts = tu.tokensFor(parent.getLeaf());
        pos = skipPreviousComment(ts, pos);
        iterator.moveTo(pos);
        int res = 0;
        while (iterator.hasPrevious()) {
            final Element line = iterator.previous();
            if (!isEmpty(line)) {
                break;
            }
            res++;
        }
        return res;
    }

    private boolean isEmpty(Element line) throws BadLocationException {
        final Document d = line.getDocument();
        final String s = d.getText(line.getStartOffset(), line.getEndOffset() - line.getStartOffset());
        return isEmpty(s);
    }

    private int skipPreviousComment(TokenSequence<JFXTokenId> ts, int pos) {
        ts.move(pos);
        while (ts.movePrevious()) {
            final Token<JFXTokenId> t = ts.token();
            switch (t.id()) {
                case COMMENT:
                    return ts.offset();
                case WS:
                    continue;
                default:
                    break;
            }
        }
        return pos;
    }

    private Element getNthElement(int n, LineIterator<Element> iterator) {
        Element nth = null;
        while (iterator.hasNext() && n != 0) {
            nth = iterator.next();
            n--;
        }
        return nth;
    }

    private StringBuilder buildString(int elc, String str) {
        StringBuilder sb = new StringBuilder(elc);
        while (elc != 0) {
            sb.append(str);
            elc--;
        }
        return sb;
    }


    private static final Pattern EMPTY_LINE_PTRN = Pattern.compile("\\s+");

    private boolean isEmpty(String text) {
        return EMPTY_LINE_PTRN.matcher(text).matches();
    }
}
