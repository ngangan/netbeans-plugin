/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.modules.javafx.editor.format;

import org.netbeans.modules.editor.indent.spi.Context;

import javax.swing.text.Document;
import javax.swing.text.Element;
import java.util.ListIterator;
import java.util.NoSuchElementException;

/**
 * Iterates lines in
 */
class DocumentLinesIterator implements LineIterator<Element> {
    private final Context ctx;
    private final int start;
    private final int end;
    private Element current;
    private final Document doc;
    private final Element root;

    public DocumentLinesIterator(Context ctx, int offset) {
        this.ctx = ctx;
        start = ctx.startOffset();
        end = ctx.endOffset();
        doc = ctx.document();
        root = doc.getDefaultRootElement();
        current = root.getElement(root.getElementIndex(offset));
    }

    public DocumentLinesIterator(Context ctx) {
        this(ctx, 0);
    }

    /**
     * Returns <tt>true</tt> if this list iterator has more elements when
     * traversing the list in the forward direction. (In other words, returns
     * <tt>true</tt> if <tt>next</tt> would return an element rather than
     * throwing an exception.)
     *
     * @return <tt>true</tt> if the list iterator has more elements when
     *         traversing the list in the forward direction.
     */
    public boolean hasNext() {
        return current.getEndOffset() < doc.getLength();
    }

    /**
     * Returns the next element in the list.  This method may be called
     * repeatedly to iterate through the list, or intermixed with calls to
     * <tt>previous</tt> to go back and forth.  (Note that alternating calls
     * to <tt>next</tt> and <tt>previo0us</tt> will return the same element
     * repeatedly.)
     *
     * @return the next element in the list.
     * @throws java.util.NoSuchElementException
     *          if the iteration has no next element.
     */
    public Element next() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        current = root.getElement(root.getElementIndex(current.getEndOffset() + 1));
        return current;
    }

    /**
     * Returns <tt>true</tt> if this list iterator has more elements when
     * traversing the list in the reverse direction.  (In other words, returns
     * <tt>true</tt> if <tt>previous</tt> would return an element rather than
     * throwing an exception.)
     *
     * @return <tt>true</tt> if the list iterator has more elements when
     *         traversing the list in the reverse direction.
     */
    public boolean hasPrevious() {
        return current.getStartOffset() > 0;
    }

    /**
     * Returns the previous element in the list.  This method may be called
     * repeatedly to iterate through the list backwards, or intermixed with
     * calls to <tt>next</tt> to go back and forth.  (Note that alternating
     * calls to <tt>next</tt> and <tt>previous</tt> will return the same
     * element repeatedly.)
     *
     * @return the previous element in the list.
     * @throws java.util.NoSuchElementException
     *          if the iteration has no previous
     *          element.
     */
    public Element previous() {
        if (!hasPrevious()) {
            throw new NoSuchElementException();
        }
        current = root.getElement(root.getElementIndex(current.getStartOffset() - 1));
        return current;
    }

    public void moveTo(int offset) {
        current = root.getElement(root.getElementIndex(offset));
    }

    public Element get() {
        return current;
    }

    public void remove() {
        throw new UnsupportedOperationException("Read only interator!");
    }

}
