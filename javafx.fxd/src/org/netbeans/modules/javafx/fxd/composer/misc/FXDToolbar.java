/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.misc;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JSeparator;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.UIManager;
import javax.swing.border.Border;
import org.netbeans.modules.javafx.fxd.composer.model.actions.AbstractFXDToggleAction;
import org.openide.awt.MouseUtils;

/**
 *
 * @author Pavel Benes
 */
public abstract class FXDToolbar extends JToolBar {
    private final ButtonMouseListener m_buttonListener;

    public FXDToolbar() {
        m_buttonListener = new ButtonMouseListener();
        setLayout(new GridBagLayout());
        setFloatable(false);
        setFocusable(true);
        Border b = (Border) UIManager.get("Nb.Editor.Toolbar.border"); //NOI18N
        setBorder(b);        
    }
    
    protected AbstractButton addButton( Action action) {
        boolean isToggle = action instanceof AbstractFXDToggleAction;
        Border buttonBorder = UIManager.getBorder("nb.tabbutton.border"); //NOI18N
        AbstractButton button;

        if (isToggle) {
            final JToggleButton tButton = new JToggleButton(action);
            action.addPropertyChangeListener(new PropertyChangeListener() {

                public void propertyChange(PropertyChangeEvent evt) {
                    if (AbstractFXDToggleAction.SELECTION_STATE.equals(evt.getPropertyName())) {
                        tButton.setSelected(((Boolean) evt.getNewValue()).booleanValue());
                        tButton.repaint();
                    }
                }
            });
            Boolean state = (Boolean) action.getValue(AbstractFXDToggleAction.SELECTION_STATE);
            if (state != null) {
                tButton.setSelected(state.booleanValue());
            }
            button = tButton;
        } else {
            button = new JButton(action);
        }

        if (buttonBorder != null) {
            button.setBorder(buttonBorder);
        }
        GridBagConstraints constrains = new GridBagConstraints();
        constrains.anchor = GridBagConstraints.WEST;

        button.setContentAreaFilled(true);
        button.setBorderPainted(true);
        if (button instanceof JButton) {
            button.addMouseListener(m_buttonListener);
        }
        //@inherited fix of issue #69642. Focus shouldn't stay in toolbar
        button.setFocusable(false);
        add(button, constrains);
        return button;
    }

    protected void addCombo( JComboBox comboBox) {
        GridBagConstraints constrains = new GridBagConstraints();
        constrains.anchor = GridBagConstraints.WEST;

        //@inherited fix of issue #69642. Focus shouldn't stay in toolbar
        comboBox.setFocusable(false);

        Dimension size = comboBox.getPreferredSize();
        comboBox.setPreferredSize(size);
        comboBox.setSize(size);
        comboBox.setMinimumSize(size);
        comboBox.setMaximumSize(size);

        comboBox.setEditable(true);

        add(comboBox, constrains);
    }
    
    protected static JSeparator createToolBarSeparator() {
        JSeparator toolBarSeparator = new JSeparator(JSeparator.VERTICAL);
        Dimension dim = new Dimension(2, 22);
        toolBarSeparator.setPreferredSize(dim);
        toolBarSeparator.setSize(dim);
        toolBarSeparator.setMinimumSize(dim);
        return toolBarSeparator;
    }
    
    protected final class ButtonMouseListener extends MouseUtils.PopupMouseAdapter {
        @Override
        public void mouseEntered(MouseEvent evt) {
            if (evt.getSource() instanceof JButton) {
                JButton button = (JButton) evt.getSource();
                if (button.isEnabled()) {
                    button.setContentAreaFilled(true);
                    button.setBorderPainted(true);
                }
            }
        }

        @Override
        public void mouseExited(MouseEvent evt) {
            if (evt.getSource() instanceof JButton) {
                JButton button = (JButton) evt.getSource();
                if (button.isEnabled()) {
                    button.setContentAreaFilled(false);
                    button.setBorderPainted(false);
                }
            }
        }

        protected void showPopup(MouseEvent evt) {
        }
    }        
}
