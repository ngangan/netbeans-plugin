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
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
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
package org.netbeans.modules.javafx.fxd.composer.misc;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
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
        setFocusable(false);        
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
        constrains.insets = new Insets(0, 3, 0, 2);
        
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

    /**
     * adds comboBox to fxz composer toolbar
     * @param toolbar Toolbar to add comboBox at
     * @param comboBox comboBox to add
     * @param index the position in the container's list at which to insert
     *  the component; <code>-1</code> means insert at the end component
     * @param isEditable is comboBox editable
     */
    public static void addCombo( JToolBar toolbar, JComboBox comboBox, int index, boolean isEditable) {
        GridBagConstraints constrains = new GridBagConstraints();
        constrains.anchor = GridBagConstraints.WEST;
        constrains.insets = new Insets(0, 3, 0, 2);

        //@inherited fix of issue #69642. Focus shouldn't stay in toolbar
        comboBox.setFocusable(false);

        Dimension size = comboBox.getPreferredSize();
        comboBox.setPreferredSize(size);
        comboBox.setSize(size);
        comboBox.setMinimumSize(size);
        comboBox.setMaximumSize(size);

        comboBox.setEditable(isEditable);

       toolbar.add(comboBox, constrains, index);
    }
    
    public static JLabel createLabel( String text) {
        JLabel label = new JLabel(text);
        Dimension size = label.getMinimumSize();
        label.setPreferredSize(size);
        label.setSize(size);
        label.setMinimumSize(size);
        label.setMaximumSize(size);
        return label;
    }
    
    public static JSeparator createToolBarSeparator() {
        JSeparator toolBarSeparator = new JSeparator(JSeparator.VERTICAL);
        Dimension dim = new Dimension(4, 22);
        toolBarSeparator.setPreferredSize(dim);
        toolBarSeparator.setSize(dim);
        toolBarSeparator.setMinimumSize(dim);
        toolBarSeparator.setMaximumSize(dim);
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
