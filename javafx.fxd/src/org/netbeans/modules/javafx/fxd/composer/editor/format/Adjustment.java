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

import org.netbeans.modules.editor.indent.spi.Context;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Position;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 */
abstract class Adjustment {
    static Adjustment delete(Position start, Position end) {
        return replace(start, end, null);
    }

    static Adjustment add(Position pos, String adjustment) {
        return new AddAdjustment(pos, adjustment);
    }

    static Adjustment replace(Position start, Position end, String replaceWith) {
        return new ReplaceAdjustment(start, end, replaceWith);
    }

    static Adjustment indent(Position start, int newIndent) {
        return new IndentAdjustment(start, newIndent);
    }

    static Adjustment compound(Adjustment... adjustments) {
        return new CompoundAdjustment(adjustments);
    }

    abstract void apply(Context ctx) throws BadLocationException;

    private static class AddAdjustment extends Adjustment {
        private final Position pos;
        private final String adjustment;

        AddAdjustment(Position pos, String adjustment) {
            this.pos = pos;
            this.adjustment = adjustment;
        }

        void apply(Context ctx) throws BadLocationException {
            Document doc = ctx.document();
            doc.insertString(pos.getOffset(), adjustment, null);
        }


        @Override
        public String toString() {
            return "AddAdjustment[" + // NOI18N
                    "pos=" + pos + // NOI18N
                    ", adjustment='" + adjustment + '\'' + // NOI18N
                    ']'; // NOI18N
        }
    }

    private static class ReplaceAdjustment extends Adjustment {
        private final Position start;
        private final Position end;
        private final String replaceWith;

        ReplaceAdjustment(Position start, Position end, String replaceWith) {
            this.start = start;
            this.end = end;
            this.replaceWith = replaceWith;
        }

        void apply(Context ctx) throws BadLocationException {
            Document doc = ctx.document();
            doc.remove(start.getOffset(), end.getOffset() - start.getOffset());
            if (replaceWith != null)
                doc.insertString(start.getOffset(), replaceWith, null);
        }


        @Override
        public String toString() {
            return "ReplaceAdjustment[" + // NOI18N
                    "start=" + start + // NOI18N
                    ", end=" + end + // NOI18N
                    ", replaceWith='" + replaceWith + '\'' + // NOI18N
                    ']'; // NOI18N
        }
    }

    private static class IndentAdjustment extends Adjustment {
        private final Position pos;
        private final int numberOfSpaces;

        IndentAdjustment(Position pos, int numberOfSpaces) {
            this.pos = pos;
            this.numberOfSpaces = numberOfSpaces;
        }

        void apply(Context ctx) throws BadLocationException {
            ctx.modifyIndent(ctx.lineStartOffset(pos.getOffset()), numberOfSpaces);
        }


        @Override
        public String toString() {
            return "IndentAdjustment[" + // NOI18N
                    "pos=" + pos + // NOI18N
                    ", numberOfSpaces=" + numberOfSpaces + // NOI18N
                    ']'; // NOI18N
        }
    }

    private static class CompoundAdjustment extends Adjustment {
        private final Adjustment[] adjs;

        CompoundAdjustment(Adjustment... adjs) {
            this.adjs = adjs;
        }

        void apply(Context ctx) throws BadLocationException {
            for (Adjustment adjustment : adjs) {
                adjustment.apply(ctx);
            }
        }
    }
}
