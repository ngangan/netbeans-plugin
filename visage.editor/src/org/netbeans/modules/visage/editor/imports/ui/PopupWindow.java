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

package org.netbeans.modules.javafx.editor.imports.ui;

import org.netbeans.spi.editor.completion.CompletionItem;
import org.openide.util.Utilities;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @todo documentation
 */
abstract class PopupWindow<T extends CompletionItem> implements FocusListener {

    private FixImportsLayout<T> layout;

    private Popup popup;

    /**
     * Bounds at which the visible popup has.
     */
    private Rectangle popupBounds;

    private JComponent contentComponent;

    private int anchorOffset;

    private Rectangle anchorOffsetBounds;

    private boolean displayAboveCaret;

    private boolean preferDisplayAboveCaret;

    private boolean showRetainedPreferredSize;

    private JComponent focusListeningComponent;

    public final boolean isVisible() {
        return (popup != null);
    }

    public final boolean isActive() {
        return (contentComponent != null);
    }

    public final void hide() {

        Runnable runnable = new Runnable() {
            public void run() {
                if (isVisible()) {
                    popup.hide();
                    popup = null;
                    popupBounds = null;
                    if (focusListeningComponent != null) {
                        focusListeningComponent.removeFocusListener(PopupWindow.this);
                        focusListeningComponent = null;
                    }
                    contentComponent = null;
                    anchorOffset = -1;
                    // Reset screen bounds as well to not cache too long
                    ScreenBoundsProvider.clear();
                }
            }
        };
        if (SwingUtilities.isEventDispatchThread()) {
            runnable.run();
        } else {
            SwingUtilities.invokeLater(runnable);
        }

    }

    /**
     * Return true if this popup should be focusable (there is a focusable
     * component in it). The popupFactory.getPopup() will use non-null parent
     * editor pane in such case.
     *
     * @return always false.
     */
    protected boolean isFocusable() {
        return false; // By default not focusable
    }

    /**
     * Get the component to which the focus
     *
     * @return content
     */
    protected JComponent getFocusListeningComponent() {
        return contentComponent;
    }

    public final boolean isDisplayAboveCaret() {
        return displayAboveCaret;
    }

    public final Rectangle getPopupBounds() {
        return popupBounds;
    }

    final void setLayout(FixImportsLayout<T> layout) {
        assert (layout != null);
        this.layout = layout;
    }

    final void setPreferDisplayAboveCaret(boolean preferDisplayAboveCaret) {
        this.preferDisplayAboveCaret = preferDisplayAboveCaret;
    }

    final void setContentComponent(JComponent contentComponent) {
        assert (contentComponent != null);
        this.contentComponent = contentComponent;
    }

    final void setAnchorOffset(int anchorOffset) {
        this.anchorOffset = anchorOffset;
        anchorOffsetBounds = null;
    }

    final int getAnchorOffset() {
        int offset = anchorOffset;
        if (offset == -1) {
            // Get caret position
            JTextComponent editorComponent = getEditorComponent();
            if (editorComponent != null) {
                offset = editorComponent.getSelectionStart();
            }
        }
        return offset;
    }

    final JComponent getContentComponent() {
        return contentComponent;
    }

    final Dimension getPreferredSize() {
        JComponent comp = getContentComponent();

        if (comp == null)
            return new Dimension(0, 0);

        int screenWidth = ScreenBoundsProvider.getScreenBounds(getEditorComponent()).width;

        Dimension maxSize = new Dimension((int) (screenWidth *
                ScreenBoundsProvider.MAX_COMPL_COVERAGE),
                comp.getMaximumSize().height); //set maximum part of screen covered
        setMaxSize(comp, maxSize);

        // if there is space between right CC border and right screen edge,
        // add this gap to maximum width
        int gap = screenWidth - (getAnchorOffsetBounds().x + comp.getPreferredSize().width);
        if (gap > 0) maxSize.width += gap;

        setMaxSize(comp, maxSize);
        return comp.getPreferredSize();
    }

    /**
     * Sets maximum size for appropriate JComponent, depending on
     * wheteher additional items are present
     *
     * @param comp    comp
     * @param maxSize max allowed size
     */
    private void setMaxSize(JComponent comp, Dimension maxSize) {
        if (comp instanceof JPanel) {
            comp.getComponent(0).setMaximumSize(maxSize); // JScrollPane
        } else {
            comp.setMaximumSize(maxSize);
        }
    }

    final void resetPreferredSize() {
        JComponent comp = getContentComponent();
        if (comp == null) {
            return;
        }
        comp.setPreferredSize(null);
    }

    final boolean isShowRetainedPreferredSize() {
        return showRetainedPreferredSize;
    }

    final FixImportsLayout<T> getLayout() {
        return layout;
    }

    final JTextComponent getEditorComponent() {
        return layout.getEditorComponent();
    }

    protected int getAnchorHorizontalShift() {
        return 0;
    }

    final Rectangle getAnchorOffsetBounds() {
        JTextComponent editorComponent = getEditorComponent();
        if (editorComponent == null) {
            return new Rectangle();
        }
        if (anchorOffsetBounds == null) {
            int anchorOffset = getAnchorOffset();
            try {
                anchorOffsetBounds = editorComponent.modelToView(anchorOffset);
                if (anchorOffsetBounds != null) {
                    anchorOffsetBounds.x -= getAnchorHorizontalShift();
                } else {
                    anchorOffsetBounds = new Rectangle(); // use empty rectangle
                }
            } catch (BadLocationException e) {
                anchorOffsetBounds = new Rectangle(); // use empty rectangle
            }
            Point anchorOffsetPoint = anchorOffsetBounds.getLocation();
            SwingUtilities.convertPointToScreen(anchorOffsetPoint, editorComponent);
            anchorOffsetBounds.setLocation(anchorOffsetPoint);
        }
        return anchorOffsetBounds;
    }

    final Popup getPopup() {
        return popup;
    }

    /**
     * Find bounds of the popup based on knowledge of the preferred size
     * of the content component and the preference of the displaying
     * of the popup either above or below the occupied bounds.
     *
     * @param occupiedBounds      bounds of the rectangle above or below which
     *                            the bounds should be found.
     * @param aboveOccupiedBounds whether the bounds should be found for position
     *                            above or below the occupied bounds.
     * @return rectangle with absolute screen bounds of the popup.
     */
    private Rectangle findPopupBounds(Rectangle occupiedBounds, boolean aboveOccupiedBounds) {
        Rectangle screen = ScreenBoundsProvider.getScreenBounds(getEditorComponent());
        Dimension prefSize = getPreferredSize();
        Rectangle popupBounds = new Rectangle();

        popupBounds.x = Math.min(occupiedBounds.x,
                (screen.x + screen.width) - prefSize.width);
        popupBounds.x = Math.max(popupBounds.x, screen.x);
        popupBounds.width = Math.min(prefSize.width, screen.width);

        if (aboveOccupiedBounds) {
            popupBounds.height = Math.min(prefSize.height,
                    occupiedBounds.y - screen.y - FixImportsLayout.POPUP_VERTICAL_GAP);
            popupBounds.y = occupiedBounds.y - FixImportsLayout.POPUP_VERTICAL_GAP - popupBounds.height;
        } else { // below caret
            popupBounds.y = occupiedBounds.y
                    + occupiedBounds.height + FixImportsLayout.POPUP_VERTICAL_GAP;
            popupBounds.height = Math.min(prefSize.height,
                    (screen.y + screen.height) - popupBounds.y);
        }
        return popupBounds;
    }

    /**
     * Create and display the popup at the given bounds.
     *
     * @param popupBounds       location and size of the popup.
     * @param displayAboveCaret whether the popup is displayed above the anchor
     *                          bounds or below them (it does not be right above them).
     */
    private void show(Rectangle popupBounds, boolean displayAboveCaret) {
        // Hide the original popup if exists
        if (popup != null) {
            popup.hide();
            popup = null;
        }

        // Explicitly set the preferred size
        Dimension origPrefSize = getPreferredSize();
        Dimension newPrefSize = popupBounds.getSize();
        JComponent contComp = getContentComponent();
        if (contComp == null) {
            return;
        }
        contComp.setPreferredSize(newPrefSize);
        showRetainedPreferredSize = newPrefSize.equals(origPrefSize);

        focusListeningComponent = getFocusListeningComponent();
        if (focusListeningComponent != null) {
            focusListeningComponent.addFocusListener(this);
        }

        PopupFactory factory = PopupFactory.getSharedInstance();
        // Lightweight completion popups don't work well on the Mac - trying
        // to click on its scrollbars etc. will cause the window to be hidden,
        // so force a heavyweight parent by passing in owner==null. (#96717)

        JTextComponent owner = Utilities.isMac() ? null : layout.getEditorComponent();

        // #76648: Autocomplete box is too close to text
        if (displayAboveCaret && Utilities.isMac()) {
            popupBounds.y -= 10;
        }

        popup = factory.getPopup(isFocusable() ? null : owner, contComp, popupBounds.x, popupBounds.y);
        popup.show();

        this.popupBounds = popupBounds;
        this.displayAboveCaret = displayAboveCaret;
    }

    /**
     * Show the popup along the anchor bounds and take
     * the preferred location (above or below caret) into account.
     */
    void showAlongAnchorBounds() {
        showAlongOccupiedBounds(getAnchorOffsetBounds());
    }

    void showAlongAnchorBounds(boolean aboveCaret) {
        showAlongOccupiedBounds(getAnchorOffsetBounds(), aboveCaret);
    }

    /**
     * Show the popup along the anchor bounds and take
     * the preferred location (above or below caret) into account.
     *
     * @param occupiedBounds bounds that are occupied.
     */
    void showAlongOccupiedBounds(Rectangle occupiedBounds) {
        boolean aboveCaret;
        if (isEnoughSpace(occupiedBounds, preferDisplayAboveCaret)) {
            aboveCaret = preferDisplayAboveCaret;
        } else { // not enough space at preferred location
            // Choose the location with more space
            aboveCaret = isMoreSpaceAbove(occupiedBounds);
        }
        Rectangle bounds = findPopupBounds(occupiedBounds, aboveCaret);
        show(bounds, aboveCaret);
    }

    /**
     * Displays popup right, left of currently occupied bounds if possible,
     * otherwise fallback to above/below
     *
     * @param occupiedBounds bounds of CC popup
     * @param unionBounds    bounds occupied by all popups
     */
    void showAlongOrNextOccupiedBounds(Rectangle occupiedBounds, Rectangle unionBounds) {
        Rectangle screen = ScreenBoundsProvider.getScreenBounds(getEditorComponent());
        Dimension prefSize = getPreferredSize();
        Rectangle bounds = new Rectangle();
        boolean aboveCaret;

        aboveCaret = isEnoughSpace(occupiedBounds, preferDisplayAboveCaret) && preferDisplayAboveCaret;

        boolean left = false;
        boolean right = false;

        // Right of CC
        if (occupiedBounds.x + occupiedBounds.width + prefSize.width < screen.width &&
                occupiedBounds.y + prefSize.height < screen.height) {
            bounds.x = occupiedBounds.x + occupiedBounds.width + FixImportsLayout.POPUP_VERTICAL_GAP;
            right = true;
        }

        // Left of CC
        if (!right && occupiedBounds.x - prefSize.width > 0 && occupiedBounds.y + prefSize.height < screen.height) {
            bounds.x = occupiedBounds.x - prefSize.width - FixImportsLayout.POPUP_VERTICAL_GAP;
            left = true;
        }

        if (right || left) {
            bounds.width = prefSize.width;
            bounds.height = Math.min(prefSize.height, screen.height);
            if (aboveCaret) {
                bounds.y = occupiedBounds.y + occupiedBounds.height - prefSize.height;
            } else {
                bounds.y = occupiedBounds.y;
            }
            show(bounds, aboveCaret);
            return;
        }

        // Fallback to Above/Below
        showAlongOccupiedBounds(unionBounds);
    }

    void showAlongOccupiedBounds(Rectangle occupiedBounds, boolean aboveCaret) {
        Rectangle bounds = findPopupBounds(occupiedBounds, aboveCaret);
        show(bounds, aboveCaret);
    }

    boolean isMoreSpaceAbove(Rectangle bounds) {
        Rectangle screen = ScreenBoundsProvider.getScreenBounds(getEditorComponent());
        int above = bounds.y - screen.y;
        int below = (screen.y + screen.height) - (bounds.y + bounds.height);
        return (above > below);
    }

    /**
     * Check whether there is enough space for this popup
     * on its preferred location related to caret.
     *
     * @param occupiedBounds occupoed bounds
     * @return true if yes
     */
    boolean isEnoughSpace(Rectangle occupiedBounds) {
        return isEnoughSpace(occupiedBounds, preferDisplayAboveCaret);
    }

    /**
     * Check whether there is enough space for this popup above
     * or below the given occupied bounds.
     *
     * @param occupiedBounds      bounds above or below which the available
     *                            space should be determined.
     * @param aboveOccupiedBounds whether the space should be checked above
     *                            or below the occupiedBounds.
     * @return true if there is enough space for the preferred size of this popup
     *         on the requested side or false if not.
     */
    boolean isEnoughSpace(Rectangle occupiedBounds, boolean aboveOccupiedBounds) {
        Rectangle screen = ScreenBoundsProvider.getScreenBounds(getEditorComponent());

        int freeHeight = aboveOccupiedBounds
                ? occupiedBounds.y - screen.y
                : (screen.y + screen.height) - (occupiedBounds.y + occupiedBounds.height);
        Dimension prefSize = getPreferredSize();
        return (prefSize.height < freeHeight);
    }

    boolean isEnoughSpace(boolean aboveCaret) {
        return isEnoughSpace(getAnchorOffsetBounds(), aboveCaret);
    }

    public boolean isOverlapped(Rectangle bounds) {
        return isVisible() && popupBounds.intersects(bounds);
    }

    public boolean isOverlapped(PopupWindow<T> popup) {
        return popup.isVisible() && isOverlapped(popup.getPopupBounds());
    }

    public Rectangle unionBounds(Rectangle bounds) {
        return isVisible() ? bounds.union(getPopupBounds()) : bounds;
    }

    public abstract void processKeyEvent(KeyEvent evt);

    public void focusGained(FocusEvent e) {
    }

    public void focusLost(FocusEvent e) {
    }

}
