/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
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

package org.netbeans.modules.javafx.fxd.composer.uistub;

import com.sun.javafx.tools.fxd.container.FXDContainer;
import java.awt.Color;
import java.io.File;
import java.util.HashMap;
import java.util.Map;
import javax.swing.JFileChooser;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import org.openide.util.NbBundle;

/**
 *
 * @author  Pavel Benes
 */
final class UIStubGeneratorPanel extends javax.swing.JPanel {
    private static final int TABLE_INDEX_SELECTED = 0;
    private static final int TABLE_INDEX_ENTRY_NAME = 1;
    private static final int TABLE_INDEX_UISTUB_NAME = 2;

    private final UIStubGenerator m_generator;
    private File m_initialDirectory = null;

    
    /** Creates new form UIStubGeneratorPanel */
    public UIStubGeneratorPanel( UIStubGenerator generator) {
        m_generator = generator;
        initComponents();
        jScrollPane1.getViewport().setBackground(Color.WHITE);
        String[] entryNames = m_generator.getEntryNames();
        DefaultTableModel tm = (DefaultTableModel) archiveContentTable.getModel();
        for ( String entryName : entryNames) {
            if ( entryName.toLowerCase().endsWith( "." + FXDContainer.FXD_EXTENSION)) {
                String uiStubName;
                if ( FXDContainer.MAIN_CONTENT.equals( entryName)) {
                    uiStubName = m_generator.getFileName();
                } else {
                    uiStubName = entryName.substring(0, entryName.length() - FXDContainer.FXD_EXTENSION.length());
                }
                uiStubName+=UIStubGenerator.UI_STUB_EXTENSION;
                uiStubName = normalizeUIStubName( uiStubName, false);
                tm.addRow(new Object[] { true, entryName, uiStubName});
            }
        }
    }
    
    public void setStubLocation( String str) {
        stubLocationTextBox.setText(str);        
    }
    
    public String getStubLocation() {
        return stubLocationTextBox.getText();
    }

    public void setPackagePath( String str) {
        packageTextBox.setText(str);
    }

    public String getPackagePath() {
        return packageTextBox.getText();
    }

    public boolean generateWarnings() {
        return generateWarningCheckBox.isSelected();
    }

    public Map<String, String> getSelectedEntries() {
        TableModel tm = archiveContentTable.getModel();
        int size = tm.getRowCount();

        Map<String,String> selected = new HashMap<String,String>(size);
        for (int row = 0; row < size; row++) {
            if ( Boolean.TRUE.equals(tm.getValueAt(row, TABLE_INDEX_SELECTED) )) {
                selected.put( (String) tm.getValueAt(row, TABLE_INDEX_ENTRY_NAME),
                              (String) tm.getValueAt(row, TABLE_INDEX_UISTUB_NAME));
            }
        }
        return selected;
    }

    private String normalizeUIStubName( String uiStubName, boolean complain) {
        if ( uiStubName == null || uiStubName.length() == 0) {
            throw new IllegalArgumentException("UI Stub name cannot be an empty string");
        }

        if ( !uiStubName.endsWith( UIStubGenerator.JAVAFX_EXTENSION)) {
            throw new IllegalArgumentException("UI Stub name must have the " + UIStubGenerator.JAVAFX_EXTENSION + " extension.");
        }

        StringBuilder sb = new StringBuilder();
        char c = uiStubName.charAt(0);
        if ( !Character.isJavaIdentifierStart(c)) {
            if ( complain) {
                throw new IllegalArgumentException("UI Stub name cannot start with character " + c);
            }
            sb.append("_");
        } else {
            sb.append(c);
        }
        int size = uiStubName.length() - UIStubGenerator.JAVAFX_EXTENSION.length();

        for ( int i = 1; i < size; i++) {
            c = uiStubName.charAt(i);
            if ( !Character.isJavaIdentifierPart(c)) {
                if ( complain) {
                    throw new IllegalArgumentException("UI Stub name cannot contain the character " + c);
                } else {
                    sb.append( "_");
                }
            } else {
                sb.append(c);
            }
        }
        sb.append( UIStubGenerator.JAVAFX_EXTENSION);
        return sb.toString();
    }
    
//    private int getRowIndexByEntryName( String name) {
//        TableModel tm = archiveContentTable.getModel();
//        int size = tm.getRowCount();
//
//        for ( int i = 0; i < size; i++) {
//            if ( name.equals(tm.getValueAt(i, TABLE_INDEX_ENTRY_NAME))) {
//                return i;
//            }
//        }
//        return -1;
//    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        stubLocationTextBox = new javax.swing.JTextField();
        stubLocationButton = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        archiveContentTable = new javax.swing.JTable();
        jLabel2 = new javax.swing.JLabel();
        packageTextBox = new javax.swing.JTextField();
        generateWarningCheckBox = new javax.swing.JCheckBox();

        jLabel1.setText(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.jLabel1.text")); // NOI18N

        stubLocationTextBox.setText(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.stubLocationTextBox.text")); // NOI18N

        stubLocationButton.setText(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.stubLocationButton.text")); // NOI18N
        stubLocationButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                stubLocationButtonActionPerformed(evt);
            }
        });

        jScrollPane1.setBackground(new java.awt.Color(255, 255, 255));

        archiveContentTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {

            },
            new String [] {
                "Generate", "Name", "UIStub "
            }
        ) {
            Class[] types = new Class [] {
                java.lang.Boolean.class, java.lang.String.class, java.lang.String.class
            };
            boolean[] canEdit = new boolean [] {
                true, false, true
            };

            public Class getColumnClass(int columnIndex) {
                return types [columnIndex];
            }

            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return canEdit [columnIndex];
            }
        });
        archiveContentTable.getTableHeader().setReorderingAllowed(false);
        jScrollPane1.setViewportView(archiveContentTable);
        archiveContentTable.getColumnModel().getColumn(0).setMinWidth(60);
        archiveContentTable.getColumnModel().getColumn(0).setPreferredWidth(60);
        archiveContentTable.getColumnModel().getColumn(0).setMaxWidth(60);
        archiveContentTable.getColumnModel().getColumn(0).setHeaderValue(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.archiveContentTable.columnModel.title0")); // NOI18N
        archiveContentTable.getColumnModel().getColumn(1).setHeaderValue(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.archiveContentTable.columnModel.title3")); // NOI18N
        archiveContentTable.getColumnModel().getColumn(2).setHeaderValue(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.archiveContentTable.columnModel.title1")); // NOI18N

        jLabel2.setText(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.jLabel2.text")); // NOI18N

        packageTextBox.setText(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.packageTextBox.text")); // NOI18N

        generateWarningCheckBox.setSelected(true);
        generateWarningCheckBox.setText(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.generateWarningCheckBox.text")); // NOI18N
        generateWarningCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                generateWarningCheckBoxActionPerformed(evt);
            }
        });

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(org.jdesktop.layout.GroupLayout.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.TRAILING)
                    .add(org.jdesktop.layout.GroupLayout.LEADING, jScrollPane1, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 430, Short.MAX_VALUE)
                    .add(org.jdesktop.layout.GroupLayout.LEADING, generateWarningCheckBox)
                    .add(layout.createSequentialGroup()
                        .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                            .add(jLabel1)
                            .add(jLabel2))
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                            .add(packageTextBox, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 309, Short.MAX_VALUE)
                            .add(stubLocationTextBox, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 309, Short.MAX_VALUE))
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(stubLocationButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 41, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(jLabel1)
                    .add(stubLocationButton)
                    .add(stubLocationTextBox, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(jLabel2)
                    .add(packageTextBox, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .add(18, 18, 18)
                .add(jScrollPane1, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 112, Short.MAX_VALUE)
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(generateWarningCheckBox))
        );
    }// </editor-fold>//GEN-END:initComponents

private void stubLocationButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_stubLocationButtonActionPerformed
        JFileChooser chooser = new JFileChooser();
        if ( m_initialDirectory == null) {
            m_initialDirectory = m_generator.getInitialDirectory(); 
        }
        if ( m_initialDirectory != null) {
            chooser.setCurrentDirectory(m_initialDirectory);
        }
        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        int r = chooser.showDialog( SwingUtilities.getWindowAncestor(UIStubGeneratorPanel.this),
                NbBundle.getMessage(UIStubGeneratorPanel.class, "LBL_CHOOSE_STUB_FILE")); //NOI18N
        if (r == JFileChooser.APPROVE_OPTION) {
            File file = chooser.getSelectedFile();
            m_initialDirectory = file.getParentFile();
            if ( UIStubGenerator.PACKAGE_NOT_SET.equals(packageTextBox.getText())) {
                setPackagePath( UIStubGenerator.getPackagePath(file));
            }
            setStubLocation(file.getAbsolutePath());
        }    
}//GEN-LAST:event_stubLocationButtonActionPerformed

private void generateWarningCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_generateWarningCheckBoxActionPerformed
    // TODO add your handling code here:
}//GEN-LAST:event_generateWarningCheckBoxActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTable archiveContentTable;
    private javax.swing.JCheckBox generateWarningCheckBox;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextField packageTextBox;
    private javax.swing.JButton stubLocationButton;
    private javax.swing.JTextField stubLocationTextBox;
    // End of variables declaration//GEN-END:variables

}
