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

package org.netbeans.modules.visage.fxd.composer.preview;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.JTextComponent;
import org.netbeans.modules.visage.fxd.composer.misc.ActionLookup;
import org.netbeans.modules.visage.fxd.composer.misc.FXDToolbar;
import org.netbeans.modules.visage.fxd.composer.model.actions.ActionController;
import org.netbeans.modules.visage.fxd.composer.model.actions.HighlightActionFactory;
import org.netbeans.modules.visage.fxd.composer.model.actions.SelectActionFactory;
import org.netbeans.modules.visage.fxd.dataloader.fxz.FXZDataObject;
import com.sun.visage.tools.fxd.container.FXDContainer;
import com.sun.visage.tools.fxd.loader.Profile;
import java.util.ArrayList;
import java.util.Collections;
import org.netbeans.modules.visage.fxd.composer.model.FXDComposerModel;
import org.netbeans.modules.visage.fxd.composer.model.FXZArchive;
/**
 *
 * @author Pavel Benes
 */
public final class PreviewToolbar extends FXDToolbar {

    private static final String FXD_EXTENSION = "."+FXDContainer.FXD_EXTENSION; // NOI18N
    private static final String[] ZOOM_VALUES = new String[]{"400%", "300%", "200%", "100%", "75%", "50%", "25%"}; //NOI18N
    
    private final FXZDataObject  m_dObj;
    private final JComboBox      m_zoomComboBox;
    //private final JComboBox      m_profileComboBox;
    private final JComboBox      m_entryComboBox;
    
    public PreviewToolbar( FXZDataObject dObj, ActionLookup lookup) {
        m_dObj = dObj;
        
        GridBagConstraints constrains = new GridBagConstraints();
        constrains.anchor = GridBagConstraints.WEST;
        constrains.insets = new Insets(0, 3, 0, 2);
        add(createToolBarSeparator(), constrains);

        add( createLabel("Scene:"), constrains, -1);
        addCombo( this, m_entryComboBox=createEntryCombo(m_dObj, "preview"), -1, true);

        add(createToolBarSeparator(), constrains);

        addButton( lookup.get( SelectActionFactory.PreviousSelectionAction.class));
        addButton( lookup.get( SelectActionFactory.NextSelectionAction.class));
        addButton( lookup.get( SelectActionFactory.ParentSelectionAction.class));
        add(createToolBarSeparator(), constrains);
        
        addButton( lookup.get( PreviewImagePanel.ZoomToFitAction.class));
        
        addCombo( this, m_zoomComboBox=createZoomCombo(), -1, true);
        
        addButton( lookup.get(PreviewImagePanel.ZoomInAction.class));
        addButton( lookup.get(PreviewImagePanel.ZoomOutAction.class));
        add(createToolBarSeparator(), constrains);

        addButton( lookup.get(HighlightActionFactory.ToggleTooltipAction.class));
        addButton( lookup.get(HighlightActionFactory.ToggleHighlightAction.class));
        add(createToolBarSeparator(), constrains);
        addButton( lookup.get(ActionController.GenerateUIStubAction.class));

        // #183876
        //add(createToolBarSeparator(), constrains);
        //addCombo(this, createProfileCombo(), -1, true);

        constrains = new GridBagConstraints();
        constrains.anchor = GridBagConstraints.WEST;
        constrains.fill = GridBagConstraints.HORIZONTAL;
        constrains.weightx = 1.0;
        add(new JPanel(), constrains);
    }

    void refresh() {
        updateZoomCombo();
        updateEntryCombo(m_dObj, m_entryComboBox);
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
                    if (selection.endsWith("%")) {   //NOI18N
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

    private JComboBox createProfileCombo() {
        final JComboBox profileComboBox = new JComboBox(
            new Object[] {
                Profile.desktop,
                Profile.mobile
            }
        );

        profileComboBox.setSelectedItem( m_dObj.getDataModel().getPreviewProfile());
        
        profileComboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                Profile profile = (Profile) profileComboBox.getSelectedItem();
                m_dObj.getController().setPreviewProfile(profile);
            }
        });

        return profileComboBox;
    }

   
    private static final String PROP_COMBO = "COMBO_NAME";
    
    public static JComboBox createEntryCombo(final FXZDataObject  dObj, String comboName) {
        final JComboBox entryComboBox = new JComboBox();

        assert comboName != null;
        
        updateEntryCombo( dObj, entryComboBox);
        
        entryComboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                String entryName = (String) entryComboBox.getSelectedItem();
                dObj.selectEntry(entryName);
            }
        });
        
        entryComboBox.putClientProperty( PROP_COMBO, comboName);
        return entryComboBox;
    }

    public static void updateEntryCombo( FXZDataObject  dObj, JComboBox entryCombo) {
        String [] entryNames = getSceneEntryNames(dObj);

        boolean syncNeeded = false;

        if ( entryNames.length == entryCombo.getItemCount()) {
            for ( int i = 0; i < entryNames.length; i++) {
                if ( !entryNames[i].equals( entryCombo.getItemAt(i))) {
                    syncNeeded = true;
                    break;
                }
            }
        } else {
            syncNeeded = true;
        }

        if ( syncNeeded) {
            // remove listeners to prevent actions to be fired during the combo update
            ActionListener [] listeners = entryCombo.getActionListeners();
            for ( ActionListener l : listeners) {
                entryCombo.removeActionListener(l);
            }
            entryCombo.removeAllItems();
            for ( String entryName: entryNames) {
                entryCombo.addItem(entryName);
            }
            // put back the temporarily removed listeners
            for ( ActionListener l : listeners) {
                entryCombo.addActionListener(l);
            }
        }

        String selectedEntry = dObj.getDataModel().getSelectedEntry();
        
        //System.out.println("Selecting in the combo: " + entryCombo.getClientProperty( PROP_COMBO) + " - "+ selectedEntry);
        entryCombo.setSelectedItem( selectedEntry);
        entryCombo.setEnabled(entryCombo.getModel().getSize() > 1);
    }

    private static String[] getSceneEntryNames(FXZDataObject  dObj){
        FXDComposerModel model   = dObj.getDataModel();
        FXZArchive       archive = model.getFXDContainer();
        String []        entryNames;

        if ( archive != null) {
            String [] fullEntryNames = archive.getEntryNames();

            ArrayList<String> fxdEntryNames = new ArrayList<String>();

            for (int i = 0; i < fullEntryNames.length; i++){
                String sceneName = getFXDScene(fullEntryNames[i]);
                if (sceneName != null){
                    fxdEntryNames.add(sceneName);
                }
            }
            //sort the fxd entries without the extensions
            Collections.sort(fxdEntryNames);
            entryNames = new String[fxdEntryNames.size()];
            for ( int i = 0; i < entryNames.length; i++) {
                entryNames[i] = fxdEntryNames.get(i) + FXD_EXTENSION;
            }
        } else {
            entryNames = new String[0];
        }

        return entryNames;
    }

    private static String getFXDScene(String name){
        if ( FXZArchive.isFXDEntry(name)) {
            return name.substring(0, name.length() - FXD_EXTENSION.length());
        } else {
            return null;
        }
    }


}
