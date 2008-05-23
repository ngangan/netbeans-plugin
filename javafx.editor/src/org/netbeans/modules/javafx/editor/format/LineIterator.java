/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.modules.javafx.editor.format;

import java.util.Iterator;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @todo documentation
 */
interface LineIterator<E> extends Iterator<E> {

    boolean hasPrevious();

    E previous();

    void moveTo(int offset);

    /**
     * Special helper method to allow getting current line under cursor.
     * @return
     */
    E get();
    
}
