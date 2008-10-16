/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.preview;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.ComboBoxEditor;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.text.JTextComponent;
import org.netbeans.modules.javafx.fxd.composer.misc.ActionLookup;
import org.netbeans.modules.javafx.fxd.composer.misc.FXDToolbar;
import org.netbeans.modules.javafx.fxd.composer.model.actions.ActionController;
import org.netbeans.modules.javafx.fxd.composer.model.actions.HighlightActionFactory;
import org.netbeans.modules.javafx.fxd.composer.model.actions.SelectActionFactory;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;

/**
 *
 * @author Pavel Benes
 */
final class PreviewToolbar extends FXDToolbar {
    private static final String[] ZOOM_VALUES = new String[]{"400%", "300%", "200%", "100%", "75%", "50%", "25%"}; //NOI18N
    
    private final FXZDataObject       m_dObj;
    private final JComboBox           m_zoomComboBox;
    
    public PreviewToolbar( FXZDataObject dObj, ActionLookup lookup) {
        m_dObj = dObj;
        
        GridBagConstraints constrains = new GridBagConstraints();
        constrains.anchor = GridBagConstraints.WEST;
        constrains.insets = new Insets(0, 3, 0, 2);
        add(createToolBarSeparator(), constrains);
        
        addButton( lookup.get( SelectActionFactory.PreviousSelectionAction.class));
        addButton( lookup.get( SelectActionFactory.NextSelectionAction.class));
        addButton( lookup.get( SelectActionFactory.ParentSelectionAction.class));
        add(createToolBarSeparator(), constrains);
        
        addButton( lookup.get( PreviewImagePanel.ZoomToFitAction.class));
        
        addCombo(m_zoomComboBox=createZoomCombo());
        
        addButton( lookup.get(PreviewImagePanel.ZoomInAction.class));
        addButton( lookup.get(PreviewImagePanel.ZoomOutAction.class));
        add(createToolBarSeparator(), constrains);

        addButton( lookup.get(HighlightActionFactory.ToggleTooltipAction.class));
        addButton( lookup.get(HighlightActionFactory.ToggleHighlightAction.class));
        add(createToolBarSeparator(), constrains);
        addButton( lookup.get(ActionController.GenerateUIStubAction.class));
        
        constrains = new GridBagConstraints();
        constrains.anchor = GridBagConstraints.WEST;
        constrains.fill = GridBagConstraints.HORIZONTAL;
        constrains.weightx = 1.0;
        add(new JPanel(), constrains);        
    }

    void refresh() {
        updateZoomCombo();
    }
             
    
    private JComboBox createZoomCombo() {
        final JComboBox zoomComboBox = new JComboBox(ZOOM_VALUES);
        final ComboBoxEditor cbe = zoomComboBox.getEditor();
        
        zoomComboBox.setEditor( new ComboBoxEditor() {
            private String m_lastValue = ""; //NOI18N
            public Component getEditorComponent() {
                return cbe.getEditorComponent();
            }

            public void setItem(Object anObject) {
                cbe.setItem(anObject);
            }

            public Object getItem() {
                Object o = cbe.getItem();
                if ( o != null) {
                    String value = o.toString();
                    if (value != null) {
                        value = value.trim();
                        int len = value.length();
                        if ( len > 0) {
                            if (value.endsWith("%")) { //NOI18N
                                value = value.substring(0, len - 1);
                            }
                            try {
                                float floatValue = Float.parseFloat(value);
                                m_lastValue = Math.round(floatValue) + "%"; //NOI18N
                                return m_lastValue;
                            } catch( NumberFormatException e) { }
                        }
                    }
                }
                SwingUtilities.invokeLater( new Runnable() {
                    public void run() {
                        ((JTextComponent) cbe.getEditorComponent()).setText(m_lastValue);                            
                    }
                });
                return m_lastValue;
            }

            public void selectAll() {
                cbe.selectAll();
            }

            public void addActionListener(ActionListener l) {
                cbe.addActionListener(l);
            }

            public void removeActionListener(ActionListener l) {
                cbe.removeActionListener(l);
            }            
        });
        
        zoomComboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                String selection = (String) zoomComboBox.getSelectedItem();
                if (selection != null) {
                    selection = selection.trim();
                    if (selection.endsWith("%")) {
                        //NOI18N
                        selection = selection.substring(0, selection.length() - 1);
                    }
                    try {
                        float zoom = Float.parseFloat(selection) / 100;
                        if (zoom > 0 && zoom < 100) {
                            m_dObj.getController().setZoomRatio(zoom);
                        }
                    } catch (NumberFormatException e) {
                        e.printStackTrace();
                    }
                }
            }
        });
        
        return zoomComboBox;        
    }
    
    private void updateZoomCombo() {
        m_zoomComboBox.getEditor().setItem(Integer.toString((int) ( m_dObj.getDataModel().getZoomRatio() * 100 + 0.5)) + "%"); //NOI18N
    }            
}
