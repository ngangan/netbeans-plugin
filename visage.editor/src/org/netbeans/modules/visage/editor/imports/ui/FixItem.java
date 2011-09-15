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
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.modules.visage.editor.imports.ui;

import org.netbeans.spi.editor.completion.CompletionItem;
import org.netbeans.spi.editor.completion.CompletionTask;
import org.netbeans.spi.editor.completion.support.CompletionUtilities;
import org.openide.util.ImageUtilities;

import javax.swing.*;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.logging.Logger;

/**
 * Implentation of <code>{@link org.netbeans.spi.editor.completion.CompletionItem} which stores info
 * about one import item. This item has no logic. It just invoke notifyAll() on lock object passed on constructor.
 *
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 */
public class FixItem implements CompletionItem {
    private static final String EMPTY_STRING = ""; // NOI18N
    private static final String ITEM_ICON = "org/netbeans/modules/editor/resources/completion/class_16.png"; // NOI18N
    private static final String ITEM_COLOR = "<font color=#560000>"; // NOI18N
    private static final String ITEM_END = "</font>"; // NOI18N
    public static final ImageIcon icon;
    private static Logger log = Logger.getLogger(FixItem.class.getName());

    static {
        icon = new ImageIcon(ImageUtilities.loadImage(ITEM_ICON));
    }

    private String element;
    private final Object LOCK;
    private String elementHTMLForm;
    private static final int NORMAL = 0;
    private static final int JAVAFX = -100;
    private int sortPriority;

    /**
     * Constructor
     *
     * @param element FQCN representation of imported item.
     * @param lock the lock object hold by any depending thread.
     */
    public FixItem(String element, Object lock) {
        this.element = element;
        this.LOCK = lock;        
        elementHTMLForm = ITEM_COLOR + element + ITEM_END;
        sortPriority = element.startsWith("visage.") ? JAVAFX : NORMAL; // NOI18N
    }

    /**
     * Gets invoked when user presses <code>VK_ENTER</code> key
     * or when she double-clicks on this item with the mouse cursor.
     * <br/>
     * This method gets invoked from AWT thread.
     *
     * @param component non-null text component for which the completion was invoked.
     */
    public void defaultAction(JTextComponent component) {
        synchronized (LOCK) {
            LOCK.notifyAll();
        }
    }

    /**
     * Process the key pressed when this completion item was selected
     * in the completion popup window.
     * <br/>
     * This method gets invoked from AWT thread.
     *
     * @param evt non-null key event of the pressed key. It should be consumed
     *            in case the item is sensitive to the given key. The source of this
     *            event is the text component to which the corresponding action should
     *            be performed.
     */
    public void processKeyEvent(KeyEvent evt) {
        int keyCode = evt.getKeyCode();
        log.info("KeyEvent: " + evt.getKeyCode() + " consumed: " + evt.isConsumed()); // NOI18N
        switch (keyCode) {
            case KeyEvent.VK_ESCAPE:
            case KeyEvent.VK_ENTER: {
                    defaultAction(null);
                    evt.consume();
                    break;
            }
        }
    }


    private ImageIcon getIcon() {
        return icon;
    }

    private String getLeftHtmlText() {
        return elementHTMLForm;
    }

    private String getRightHtmlText() {
        return EMPTY_STRING;
    }


    /**
     * Get the preferred visual width of this item.
     * <br>
     * The visual height of the item is fixed to 16 points.
     *
     * @param g           graphics that can be used for determining the preferred width
     *                    e.g. getting of the font metrics.
     * @param defaultFont default font used for rendering.
     */
    public int getPreferredWidth(Graphics g, Font defaultFont) {
        return CompletionUtilities.getPreferredWidth(getLeftHtmlText(), getRightHtmlText(), g, defaultFont);
    }

    /**
     * Render this item into the given graphics.
     *
     * @param g               graphics to render the item into.
     * @param defaultFont     default font used for rendering.
     * @param defaultColor    default color used for rendering.
     * @param backgroundColor color used for background.
     * @param width           width of the area to render into.
     * @param height          height of the are to render into.
     * @param selected        whether this item is visually selected in the list
     *                        into which the items are being rendered.
     */
    public void render(Graphics g, Font defaultFont, Color defaultColor, Color backgroundColor, int width, int height, boolean selected) {
        CompletionUtilities.renderHtml(getIcon(), getLeftHtmlText(), getRightHtmlText(), g, defaultFont, defaultColor, width, height, selected);
    }

    /**
     * Returns a task used to obtain a documentation associated with the item if there
     * is any.
     */
    public CompletionTask createDocumentationTask() {
        return null;
    }

    /**
     * Returns a task used to obtain a tooltip hint associated with the item if there
     * is any.
     */
    public CompletionTask createToolTipTask() {
        return null;
    }

    public String getElement() {
        return element;
    }

    /**
     * When enabled for the item the instant substitution should process the item
     * in the same way like when the item is displayed and Enter key gets pressed
     * by the user.
     * <br>
     * Instant substitution is invoked when there would be just a single item
     * displayed in the completion popup window.
     * <br>
     * The implementation can invoke the {@link #defaultAction(javax.swing.text.JTextComponent)}
     * if necessary.
     * <br/>
     * This method gets invoked from AWT thread.
     *
     * @param component non-null text component for which the completion was invoked.
     * @return <code>true</code> if the instant substitution was successfully done.
     *         <code>false</code> means that the instant substitution should not be done
     *         for this item and the completion item should normally be displayed.
     */
    public boolean instantSubstitution(JTextComponent component) {
        return false;
    }

    /**
     * Returns the item's priority. A lower value means a lower index of the item
     * in the completion result list.
     */
    public int getSortPriority() {
        return sortPriority;
    }

    /**
     * Returns a text used to sort items alphabetically.
     */
    public CharSequence getSortText() {
        return element;
    }

    /**
     * Returns a text used for finding of a longest common prefix
     * after the <i>TAB</i> gets pressed or when the completion is opened explicitly.
     * <br>
     * The completion infrastructure will evaluate the insert prefixes
     * of all the items present in the visible result and finds the longest
     * common prefix.
     * <p/>
     * <p/>
     * Generally the returned text does not need to contain all the information
     * that gets inserted when the item is selected.
     * <br>
     * For example in java completion the field name should be returned for fields
     * or a method name for methods (but not parameters)
     * or a non-FQN name for classes.
     *
     * @return non-null character sequence containing the insert prefix.
     *         <br>
     *         Returning an empty string will effectively disable the TAB completion
     *         as the longest common prefix will be empty.
     * @since 1.4
     */
    public CharSequence getInsertPrefix() {
        return EMPTY_STRING;
    }
}
