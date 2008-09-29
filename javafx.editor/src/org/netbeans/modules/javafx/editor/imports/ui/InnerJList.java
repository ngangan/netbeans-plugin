/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.modules.javafx.editor.imports.ui;

import org.netbeans.editor.LocaleSupport;
import org.netbeans.spi.editor.completion.CompletionItem;

import javax.swing.*;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.MouseListener;
import java.util.Collections;
import java.util.Map;

/**
* @author Miloslav Metelka, Dusan Balek
* @version 1.00
*/

class InnerJList extends JList {

    private static final int DARKER_COLOR_COMPONENT = 5;

    private final RenderComponent renderComponent;

    private Graphics cellPreferredSizeGraphics;

    private int fixedItemHeight;
    private int maxVisibleRowCount;
    private int smartIndex;

    public InnerJList(int maxVisibleRowCount, MouseListener mouseListener, JTextComponent editorComponent) {
        this.maxVisibleRowCount = maxVisibleRowCount;
        addMouseListener(mouseListener);
        setFont(editorComponent.getFont());
        setLayoutOrientation(JList.VERTICAL);
        setFixedCellHeight(fixedItemHeight = Math.max(FixImportsLayout.COMPLETION_ITEM_HEIGHT, getFontMetrics(getFont()).getHeight()));
        setModel(new Model(Collections.EMPTY_LIST));
        setFocusable(false);

        renderComponent = new RenderComponent();
        setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        setCellRenderer(new ListCellRenderer() {
            private ListCellRenderer defaultRenderer = new DefaultListCellRenderer();

            public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                if( value instanceof CompletionItem) {
                    CompletionItem item = (CompletionItem)value;
                    renderComponent.setItem(item);
                    renderComponent.setSelected(isSelected);
                    renderComponent.setSeparator(smartIndex > 0 && smartIndex == index);
                    Color bgColor;
                    Color fgColor;
                    if (isSelected) {
                        bgColor = list.getSelectionBackground();
                        fgColor = list.getSelectionForeground();
                    } else { // not selected
                        bgColor = list.getBackground();
                        if ((index % 2) == 0) { // every second item slightly different
                            bgColor = new Color(
                                    Math.abs(bgColor.getRed() - DARKER_COLOR_COMPONENT),
                                    Math.abs(bgColor.getGreen() - DARKER_COLOR_COMPONENT),
                                    Math.abs(bgColor.getBlue() - DARKER_COLOR_COMPONENT)
                            );
                        }
                        fgColor = list.getForeground();
                    }
                    // quick check Component.setBackground() always fires change
                    if (renderComponent.getBackground() != bgColor) {
                        renderComponent.setBackground(bgColor);
                    }
                    if (renderComponent.getForeground() != fgColor) {
                        renderComponent.setForeground(fgColor);
                    }
                    return renderComponent;

                } else {
                    return defaultRenderer.getListCellRendererComponent( list, value, index, isSelected, cellHasFocus);
                }
            }
        });
        getAccessibleContext().setAccessibleName(LocaleSupport.getString("ACSN_CompletionView"));
        getAccessibleContext().setAccessibleDescription(LocaleSupport.getString("ACSD_CompletionView"));
    }

    public @Override void paint(Graphics g) {
        Object value = (Map)(Toolkit.getDefaultToolkit().getDesktopProperty("awt.font.desktophints")); //NOI18N
        Map renderingHints = (value instanceof Map) ? (java.util.Map)value : null;
        if (renderingHints != null && g instanceof Graphics2D) {
            Graphics2D g2d = (Graphics2D) g;
            RenderingHints oldHints = g2d.getRenderingHints();
            g2d.setRenderingHints(renderingHints);
            try {
                super.paint(g2d);
            } finally {
                g2d.setRenderingHints(oldHints);
            }
        } else {
            super.paint(g);
        }
    }

    void setData(java.util.List data) {
        smartIndex = -1;
        if (data != null) {
            int itemCount = data.size();
            ListCellRenderer renderer = getCellRenderer();
            int width = 0;
            int maxWidth = getParent().getParent().getMaximumSize().width;
            boolean stop = false;
            for(int index = 0; index < itemCount; index++) {
                Object value = data.get(index);
                Dimension cellSize;
//                if (value instanceof LazyCompletionItem)
//                    maxWidth = (int)(ScreenBoundsProvider.getScreenBounds(editorComponent).width * ScreenBoundsProvider.MAX_COMPL_COVERAGE);
                Component c = renderer.getListCellRendererComponent(this, value, index, false, false);
                cellSize = c.getPreferredSize();
                if (cellSize.width > width) {
                    width = cellSize.width;
                    if (width >= maxWidth)
                        stop = true;
                }
                if (smartIndex < 0 && value instanceof CompletionItem && ((CompletionItem)value).getSortPriority() >= 0)
                    smartIndex = index;
                if (stop && smartIndex >= 0)
                    break;
            }
            setFixedCellWidth(width);
//            ListModel lm = LazyListModel.create( new Model(data), CompletionImpl.filter, 1.0d, LocaleSupport.getString("completion-please-wait") ); //NOI18N
            ListModel lm = new Model(data);
            setModel(lm);

            if (itemCount > 0) {
                setSelectedIndex(0);
            }
            int visibleRowCount = Math.min(itemCount, maxVisibleRowCount);
            setVisibleRowCount(visibleRowCount);
        }
    }

    public void up() {
        int size = getModel().getSize();
        if (size > 0) {
            int idx = (getSelectedIndex() - 1 + size) % size;
            while(idx > 0 && getModel().getElementAt(idx) == null)
                idx--;
            setSelectedIndex(idx);
            ensureIndexIsVisible(idx);
        }
    }

    public void down() {
        int size = getModel().getSize();
        if (size > 0) {
            int idx = (getSelectedIndex() + 1) % size;
            while(idx < size && getModel().getElementAt(idx) == null)
                idx++;
            if (idx == size)
                idx = 0;
            setSelectedIndex(idx);
            ensureIndexIsVisible(idx);
        }
    }

    public void pageUp() {
        if (getModel().getSize() > 0) {
            int pageSize = Math.max(getLastVisibleIndex() - getFirstVisibleIndex(), 0);
            int idx = Math.max(getSelectedIndex() - pageSize, 0);
            while(idx > 0 && getModel().getElementAt(idx) == null)
                idx--;
            setSelectedIndex(idx);
            ensureIndexIsVisible(idx);
        }
    }

    public void pageDown() {
        int size = getModel().getSize();
        if (size > 0) {
            int pageSize = Math.max(getLastVisibleIndex() - getFirstVisibleIndex(), 0);
            int idx = Math.min(getSelectedIndex() + pageSize, size - 1);
            while(idx < size && getModel().getElementAt(idx) == null)
                idx++;
            if (idx == size) {
                idx = Math.min(getSelectedIndex() + pageSize, size - 1);
                while(idx > 0 && getModel().getElementAt(idx) == null)
                    idx--;
            }
            setSelectedIndex(idx);
            ensureIndexIsVisible(idx);
        }
    }

    public void begin() {
        if (getModel().getSize() > 0) {
            setSelectedIndex(0);
            ensureIndexIsVisible(0);
        }
    }

    public void end() {
        int size = getModel().getSize();
        if (size > 0) {
            int idx = size - 1;
            while(idx > 0 && getModel().getElementAt(idx) == null)
                idx--;
            setSelectedIndex(idx);
            ensureIndexIsVisible(idx);
        }
    }

    private final class Model extends AbstractListModel {

        java.util.List data;

        public Model(java.util.List data) {
            this.data = data;
        }

        public int getSize() {
            return data.size();
        }

        public Object getElementAt(int index) {
            return (index >= 0 && index < data.size()) ? data.get(index) : null;
        }
    }

    private final class RenderComponent extends JComponent {

        private CompletionItem item;

        private boolean selected;
        private boolean separator;

        void setItem(CompletionItem item) {
            this.item = item;
        }

        void setSelected(boolean selected) {
            this.selected = selected;
        }

        void setSeparator(boolean separator) {
            this.separator = separator;
        }

        public @Override void paintComponent(Graphics g) {
            // Although the JScrollPane without horizontal scrollbar
            // is explicitly set with a preferred size
            // it does not force its items with the only width into which
            // they can render (and still leaves them with the preferred width
            // of the widest item).
            // Therefore the item's render width is taken from the viewport's width.
            int itemRenderWidth = ((JViewport) InnerJList.this.getParent()).getWidth();
            Color bgColor = getBackground();
            Color fgColor = getForeground();
            int height = getHeight();

            // Clear the background
            g.setColor(bgColor);
            g.fillRect(0, 0, itemRenderWidth, height);
            g.setColor(fgColor);

            // Render the item
            item.render(g, InnerJList.this.getFont(), getForeground(), bgColor,
                    itemRenderWidth, getHeight(), selected);

            if (separator) {
                g.setColor(Color.gray);
                g.drawLine(0, 0, itemRenderWidth, 0);
                g.setColor(fgColor);
            }
        }

        public @Override Dimension getPreferredSize() {
            if (cellPreferredSizeGraphics == null) {
                // InnerJList.this.getGraphics() is null
                cellPreferredSizeGraphics = java.awt.GraphicsEnvironment.
                        getLocalGraphicsEnvironment().getDefaultScreenDevice().
                        getDefaultConfiguration().createCompatibleImage(1, 1).getGraphics();
                assert (cellPreferredSizeGraphics != null);
            }
            return new Dimension(item.getPreferredWidth(cellPreferredSizeGraphics, InnerJList.this.getFont()),
                    fixedItemHeight);
        }

    }

}
