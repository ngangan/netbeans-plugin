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
package org.netbeans.modules.javafx.editor.imports;

import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.lexer.TokenId;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.editor.indent.api.Reformat;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Position;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
* @todo documentation
*/
class Publisher implements Runnable {
    private final Document doc;
    private static Logger log = Logger.getLogger(Publisher.class.getName());
    private final ImportsModel model;

    public Publisher(Document doc, ImportsModel model) {
        this.doc = doc;
        this.model = model;
    }

    public void run() {
        TokenSequence<JFXTokenId> ts = getTokenSequence(doc, 0);
        final int startPos = quessImportsStart(ts);
        final int endPos = quessImportsEnd(ts, startPos);
        Reformat reformat = null;
        try {
            Position end = doc.createPosition(endPos);
            int length = endPos - startPos;
            if (log.isLoggable(Level.INFO)) {
                log.info(doc.getText(startPos, length) + "\n");
                log.info("Publishing following entries:");
            }
            doc.remove(startPos, length);
            int offset = startPos;
            boolean first = true;
            for (ImportsModel.ModelEntry entry : model.getEntries()) {
                if (entry.isUsed()) {
                    log.info("\t" + entry.toImportStatement());
                    String text = (first ? "" : "\n") + entry.toImportStatement();
                    first = false;
                    doc.insertString(offset, text, null);
                    offset += text.length();
                }
            }
            reformat = Reformat.get(doc);
            reformat.lock();
            reformat.reformat(0, end.getOffset());
        } catch (BadLocationException e) {
            log.severe(e.getLocalizedMessage());
        } finally {
            if (reformat != null) {
                reformat.unlock();
            }
        }
    }

    private int quessImportsEnd(TokenSequence<JFXTokenId> ts, int startPos) {
        int result = startPos;
        while (ts.moveNext()) {
            JFXTokenId tid = ts.token().id();
            switch (tid) {
                case IMPORT: {
                    moveTo(ts, JFXTokenId.SEMI);
                    result = ts.offset() + 1;
                    continue;
                }
                case WS:
                    continue;
                default: {
                    return result;
                }
            }
        }
        return result;
    }

    private int quessImportsStart(TokenSequence<JFXTokenId> ts) {
        int posibbleStart = 0;
        while (ts.moveNext()) {
            JFXTokenId tid = ts.token().id();
            switch (tid) {
                case PACKAGE: {
                    moveTo(ts, JFXTokenId.SEMI);
                    posibbleStart = ts.offset() + 1;
                    continue;
                }
                case IMPORT: {
                    posibbleStart = ts.offset();
                    moveTo(ts, JFXTokenId.SEMI);
                    return posibbleStart;
                }
                case WS:
                case COMMENT:
                case LINE_COMMENT:
                case DOC_COMMENT:
                    continue;
                default: {
                    return posibbleStart;
                }
            }
        }
        return posibbleStart;
    }

    private void moveTo(TokenSequence<JFXTokenId> ts, JFXTokenId id) {
        while (ts.moveNext()) {
            if (ts.token().id() == id) return;
        }
    }


    @SuppressWarnings({"unchecked"})
    private static <T extends TokenId> TokenSequence<T> getTokenSequence(Document doc, int dotPos) {
        TokenHierarchy<Document> th = TokenHierarchy.get(doc);
        TokenSequence<T> seq = (TokenSequence<T>) th.tokenSequence();
        seq.move(dotPos);
        return seq;
    }
}
