/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.modules.javafx.editor.format;

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


        public String toString() {
            return "AddAdjustment[" +
                    "pos=" + pos +
                    ", adjustment='" + adjustment + '\'' +
                    ']';
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


        public String toString() {
            return "ReplaceAdjustment[" +
                    "start=" + start +
                    ", end=" + end +
                    ", replaceWith='" + replaceWith + '\'' +
                    ']';
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


        public String toString() {
            return "IndentAdjustment[" +
                    "pos=" + pos +
                    ", numberOfSpaces=" + numberOfSpaces +
                    ']';
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
