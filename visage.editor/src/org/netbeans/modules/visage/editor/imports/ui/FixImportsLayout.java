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

import org.netbeans.editor.GuardedDocument;
import org.netbeans.spi.editor.completion.CompletionItem;
import org.openide.util.NbBundle;

import javax.swing.*;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.*;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

/**
 * Copied from Code Completion and shortened.
 *
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @version 0.9
 */
public final class FixImportsLayout<T extends CompletionItem> implements KeyListener, FocusListener, MouseListener {

    public static final int COMPLETION_ITEM_HEIGHT = 16;

    /**
     * Visual shift of the completion window to the left
     * so that the text in the rendered completion items.aligns horizontally
     * with the text in the document.
     */
    private static final int COMPLETION_ANCHOR_HORIZONTAL_SHIFT = 22;

    /**
     * Gap between caret and the displayed popup.
     */
    static final int POPUP_VERTICAL_GAP = 1;

    private Reference<JTextComponent> editorComponentRef;

    private final FixPopup<T> fixPopup;
    protected static FixImportsLayout<CompletionItem> instance = new FixImportsLayout<CompletionItem>();
    private boolean canceled = false;
    private Object LOCK = null;

    /**
     * Will create a new layout for fix imports popup
     * @param lock The lock to be used to notify all waiting parties when the popup is closed
     * @return Returns new {@linkplain FixImportsLayout} instance
     */
    @SuppressWarnings({"unchecked"})
    public static <T extends CompletionItem> FixImportsLayout<T> create(Object lock) {
        instance.canceled = false;
        instance.LOCK = lock;
        return (FixImportsLayout<T>) instance;
    }

    FixImportsLayout() {
        fixPopup = new FixPopup<T>();
        fixPopup.setLayout(this);
        fixPopup.setPreferDisplayAboveCaret(false);
    }

    /*public*/ JTextComponent getEditorComponent() {
        return (editorComponentRef != null)
                ? editorComponentRef.get()
                : null;
    }

    public void setEditorComponent(JTextComponent editorComponent) {
        unregister();
        JTextComponent component;
        hideAll();
        this.editorComponentRef = new WeakReference<JTextComponent>(editorComponent);
        component = getEditorComponent();
        component.addKeyListener(this);
        component.addMouseListener(this);
        component.addFocusListener(this);
    }

    private void unregister() {
        JTextComponent component = getEditorComponent();
        if (component != null) {
            component.removeKeyListener(this);
            component.removeMouseListener(this);
            component.removeFocusListener(this);
        }
    }

    private void hideAll() {
        hide();
    }

    public boolean isCancelled() {
        return canceled;
    }

    @SuppressWarnings({"MethodWithTooManyParameters"})
    public void show(List<T> data, String title, int anchorOffset, ListSelectionListener lsl,
                     String additionalItemsText, String shortcutHint, int selectedIndex) {
        hideAll();
        canceled = false;
        fixPopup.show(data, title, anchorOffset, lsl, additionalItemsText, shortcutHint, selectedIndex);
        //this is small trick to minimaze impact of change in HiRez mode.
//        fixPopup.getFocusListeningComponent().requestFocusInWindow();
    }

    public boolean hide() {
        if (fixPopup.isVisible()) {
            fixPopup.hide();
            fixPopup.completionScrollPane = null;
            unregister();
            if (LOCK != null) {
                synchronized(LOCK) {
                    LOCK.notifyAll();
                }
            }
            return true;
        } else { // not visible
            return false;
        }
    }

    public boolean isVisible() {
        return fixPopup.isVisible();
    }

    public T getSelectedItem() {
        if (canceled) return null;
        return fixPopup.getSelectedCompletionItem();
    }


    public int getSelectedIndex() {
        if (canceled) return -1;
        return fixPopup.getSelectedIndex();
    }

    public void processKeyEvent(KeyEvent evt) {
        if (fixPopup.isVisible()) {
            log.info("KeyEvent to process on visible popup: " + evt.getKeyChar()); // NOI18N
            if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
                canceled = true;
                hideAll();
            } else {
                fixPopup.processKeyEvent(evt);
            }
            evt.consume();
        } else {
            log.info("KeyEvent to process on invisible popup: " + evt.getKeyChar() + ". Unregistering..."); // NOI18N
            unregister();
        }
    }

    void updateLayout(PopupWindow<T> popup) {
        // Make sure the popup returns its natural preferred size
        popup.resetPreferredSize();
        popup.showAlongAnchorBounds();
    }

    PopupWindow<T> testGetCompletionPopup() {
        return fixPopup;
    }

    /**
     * Invoked when a key has been typed.
     * See the class description for {@link java.awt.event.KeyEvent} for a definition of
     * a key typed event.
     */
    public void keyTyped(KeyEvent e) {
        e.consume();
//        processKeyEvent(e);
    }

    /**
     * Invoked when a key has been pressed.
     * See the class description for {@link java.awt.event.KeyEvent} for a definition of
     * a key pressed event.
     */
    public void keyPressed(KeyEvent e) {
//        e.consume();
        processKeyEvent(e);
    }

    /**
     * Invoked when a key has been released.
     * See the class description for {@link java.awt.event.KeyEvent} for a definition of
     * a key released event.
     */
    public void keyReleased(KeyEvent e) {
        e.consume();
//        processKeyEvent(e);
    }

    /**
     * Invoked when a component gains the keyboard focus.
     */
    public void focusGained(FocusEvent e) {
    }

    private static Logger log = Logger.getLogger(FixImportsLayout.class.getName());
    /**
     * Invoked when a component loses the keyboard focus.
     */
    public void focusLost(FocusEvent e) {
        log.info("Focus has been lost in prospect of " + e.getOppositeComponent()); // NOI18N
        canceled = true;
        if (fixPopup != null && fixPopup.getSelectedCompletionItem() != null) {
            fixPopup.getSelectedCompletionItem()
                    .processKeyEvent(new KeyEvent(getEditorComponent(), 1, 1, 0, KeyEvent.VK_ESCAPE, KeyEvent.CHAR_UNDEFINED));
        }
        hideAll();
    }

    /**
     * Invoked when the mouse button has been clicked (pressed
     * and released) on a component.
     */
    public void mouseClicked(MouseEvent e) {
    }

    /**
     * Invoked when a mouse button has been pressed on a component.
     */
    public void mousePressed(MouseEvent e) {
    }

    /**
     * Invoked when a mouse button has been released on a component.
     */
    public void mouseReleased(MouseEvent e) {
    }

    /**
     * Invoked when the mouse enters a component.
     */
    public void mouseEntered(MouseEvent e) {
    }

    /**
     * Invoked when the mouse exits a component.
     */
    public void mouseExited(MouseEvent e) {
    }

    public void finish() {
        unregister();
        hide();
    }


    private static final class FixPopup<T extends CompletionItem> extends PopupWindow<T> {

        volatile private InnerScrollPane completionScrollPane;

        @SuppressWarnings({"MethodWithTooManyParameters", "MethodWithMoreThanThreeNegations"})
        public void show(List<T> data, String title, int anchorOffset,
                         ListSelectionListener listSelectionListener, String additionalItemsText, String shortcutHint, int selectedIndex) {

            JTextComponent editorComponent = getEditorComponent();
            if (editorComponent == null) {
                return;
            }

            Dimension lastSize;
            int lastAnchorOffset = getAnchorOffset();

            if (isVisible() && ((getContentComponent() == completionScrollPane) ^ (shortcutHint != null))) {
                lastSize = getContentComponent().getSize();
                resetPreferredSize();

            } else { // not yet visible => create completion scrollpane
                lastSize = new Dimension(0, 0); // no last size => use (0,0)

                completionScrollPane = new InnerScrollPane(
                        editorComponent, listSelectionListener,
                        new MouseAdapter() {
                            @Override
                            public void mouseClicked(MouseEvent evt) {
                                JTextComponent c = getEditorComponent();
                                if (SwingUtilities.isLeftMouseButton(evt)) {
                                    if (c != null && evt.getClickCount() == 2) {
                                        CompletionItem selectedItem
                                                = completionScrollPane.getSelectedCompletionItem();
                                        if (selectedItem != null) {
                                            if (c.getDocument() instanceof GuardedDocument && ((GuardedDocument) c.getDocument()).isPosGuarded(c.getSelectionEnd())) {
                                                Toolkit.getDefaultToolkit().beep();
                                            } else {
                                                LogRecord r = new LogRecord(Level.FINE, "COMPL_MOUSE_SELECT"); // NOI18N
                                                r.setParameters(new Object[]{null, completionScrollPane.getSelectedIndex(), selectedItem.getClass().getSimpleName()});
                                                selectedItem.defaultAction(c);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                );

                if (shortcutHint != null) {
                    JPanel panel = new JPanel();
                    panel.setLayout(new BorderLayout());
                    panel.add(completionScrollPane, BorderLayout.CENTER);
                    JLabel label = new JLabel();
                    label.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, Color.white),
                            BorderFactory.createCompoundBorder(BorderFactory.createMatteBorder(0, 1, 1, 1, Color.gray), BorderFactory.createEmptyBorder(2, 2, 2, 2))));
                    label.setFont(label.getFont().deriveFont((float) label.getFont().getSize() - 2));
                    label.setHorizontalAlignment(SwingConstants.RIGHT);
                    label.setText(NbBundle.getMessage(FixImportsLayout.class, "TXT_completion_shortcut_tips", additionalItemsText, shortcutHint)); //NOI18N
                    panel.add(label, BorderLayout.SOUTH);
                    setContentComponent(panel);
                } else {
                    setContentComponent(completionScrollPane);
                }
            }
            // Set the new data
            completionScrollPane.setData(data, title, selectedIndex);
            setAnchorOffset(anchorOffset);

            Dimension prefSize = getPreferredSize();

            boolean changePopupSize;
            changePopupSize = !isVisible() || (prefSize.height != lastSize.height)
                    || (prefSize.width != lastSize.width)
                    || anchorOffset != lastAnchorOffset;

            if (changePopupSize) {
                // Do not change the popup's above/below caret positioning
                // when the popup is already displayed
                //noinspection unchecked
                getLayout().updateLayout(this);

            } // otherwise present popup size will be retained
        }

        @SuppressWarnings({"unchecked"})
        public T getSelectedCompletionItem() {
            return (T) (isVisible() ? completionScrollPane.getSelectedCompletionItem() : null);
        }

        public int getSelectedIndex() {
            return isVisible() ? completionScrollPane.getSelectedIndex() : -1;
        }

        public void processKeyEvent(KeyEvent evt) {
            if (isVisible()) {
                Object actionMapKey = completionScrollPane.getInputMap().get(
                        KeyStroke.getKeyStrokeForEvent(evt));

                if (actionMapKey != null) {
                    Action action = completionScrollPane.getActionMap().get(actionMapKey);
                    if (action != null) {
                        action.actionPerformed(new ActionEvent(completionScrollPane, 0, null));
                        evt.consume();
                    }
                } else {
                    completionScrollPane.processKeyEvent(evt);
                }
            }
        }

        protected int getAnchorHorizontalShift() {
            return COMPLETION_ANCHOR_HORIZONTAL_SHIFT;
        }

        /**
         * Return true if this popup should be focusable (there is a focusable
         * component in it). The popupFactory.getPopup() will use non-null parent
         * editor pane in such case.
         *
         * @return always false.
         */
        @Override
        protected boolean isFocusable() {
            return getFocusListeningComponent() != null;
        }
    }

}
