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

package org.netbeans.modules.visage.fxd.composer.archive;

import java.awt.event.ActionEvent;
import java.io.File;
import javax.swing.Action;
import javax.swing.JFileChooser;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableModel;
import org.netbeans.modules.visage.fxd.composer.misc.ActionLookup;
import org.netbeans.modules.visage.fxd.composer.misc.ActionLookupUtils;
import org.netbeans.modules.visage.fxd.composer.model.FXZArchive;
import org.netbeans.modules.visage.fxd.composer.model.actions.AbstractFXDAction;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.util.NbBundle;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.ListSelectionModel;
import org.netbeans.modules.visage.fxd.composer.preview.PreviewToolbar;
import org.netbeans.modules.visage.fxd.dataloader.fxz.FXZDataObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.actions.Presenter;

/**
 *
 * @author  Pavel Benes
 */
final class ArchivePanel extends javax.swing.JPanel implements ActionLookup {

    private final FXZArchive m_archive;
    
    /** Creates new form ArchivePanel */
    public ArchivePanel( FXZArchive archive) {
        m_archive = archive;
        initComponents();
        tableContent.getSelectionModel().setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        tableContent.addMouseListener(new PopupListener());
        jScrollPane1.getViewport().setBackground( Color.WHITE);
        
        tableContent.getSelectionModel().addListSelectionListener( new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                if ( !e.getValueIsAdjusting()) {
                    boolean allowReplace;
                    boolean allowRemove;
                    boolean allowEditView;
                    
                    switch( tableContent.getSelectedRowCount()) {
                        case 0:
                            allowRemove = false;
                            allowReplace = false;
                            allowEditView = false;
                            break;
                        case 1:
                            allowRemove = true;
                            allowReplace = true;
                            allowEditView = isFXDEntrySelected();

                            break;
                        default:
                            allowRemove = true;
                            allowReplace = false;
                            allowEditView = false;
                            break;
                    }
                    int [] rows = tableContent.getSelectedRows();
                    for (int row : rows) {
                        if ( FXDContainer.MAIN_CONTENT.equals(getNameAt(row))) {
                            allowRemove = false;
                            break;
                        }
                    }
                    m_removeAction.setEnabled(allowRemove);
                    m_replaceAction.setEnabled(allowReplace);
                    m_editAction.setEnabled(allowEditView);
                    m_viewAction.setEnabled(allowEditView);
                }
            }
        });
        update();
    }

    public Action get(Class clazz) {
        return ActionLookupUtils.get(m_actions, clazz);
    }
    
    protected void update() {
        int width = jScrollPane1.getViewport().getWidth();
        
        Dimension prefSize = tableContent.getPreferredSize();
        if ( prefSize != null) {
            jScrollPane1.getViewport().setSize( width, prefSize.height);
        }
        labelTotalSize.setText ( FXZArchive.getSizeText(m_archive.getSize()));
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        labelTotalSize = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        tableContent = new javax.swing.JTable();

        jLabel1.setText(org.openide.util.NbBundle.getMessage(ArchivePanel.class, "ArchivePanel.jLabel1.text")); // NOI18N

        labelTotalSize.setText(org.openide.util.NbBundle.getMessage(ArchivePanel.class, "ArchivePanel.labelTotalSize.text")); // NOI18N

        tableContent.setModel((TableModel) m_archive);
        tableContent.getTableHeader().setReorderingAllowed(false);
        jScrollPane1.setViewportView(tableContent);

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(jScrollPane1, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 364, Short.MAX_VALUE)
                    .add(layout.createSequentialGroup()
                        .add(jLabel1)
                        .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                        .add(labelTotalSize)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(jLabel1)
                    .add(labelTotalSize))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jScrollPane1, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 259, Short.MAX_VALUE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    private static final String ENTRY_PATTERN = "content-%d.fxd";
    
    final class NewArchiveEntryAction extends AbstractFXDAction {
        public NewArchiveEntryAction() {  
            super("new_entry", true);  //NOI18N
        }        
        public void actionPerformed(ActionEvent e) {
            String[] entries = m_archive.getEntryNames();
            int index = 0;
            
            loop: while(true)  {
                index++;
                String newEntryName = String.format( ENTRY_PATTERN, index);
                for ( String name : entries) {
                    if ( name.equals( newEntryName)) {
                        continue loop;
                    }
                }
                try {
                    String content = "// empty content";
                    m_archive.add( newEntryName, content.getBytes("UTF-8"));
                    update();
                    return;
                } catch( Exception ex) {
                    ex.printStackTrace();
                    DialogDisplayer.getDefault().notify(new NotifyDescriptor.Message(
                            NbBundle.getMessage(ArchivePanel.class, "ERROR_CANNOT_NEW_ENTRY",  //NOI18N
                            ex.getLocalizedMessage()), NotifyDescriptor.Message.ERROR_MESSAGE));
                }
            }
        }
    };      

    final class AddArchiveEntryAction extends AbstractFXDAction {
        public AddArchiveEntryAction() {  
            super("add_entry", true);  //NOI18N
        }        
        public void actionPerformed(ActionEvent e) {
            JFileChooser chooser = new JFileChooser();
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            int r = chooser.showDialog( SwingUtilities.getWindowAncestor(ArchivePanel.this),
                    NbBundle.getMessage(ArchivePanel.class, "LBL_CHOOSE_FILE")); //NOI18N
            if (r == JFileChooser.APPROVE_OPTION) {
                final File file = chooser.getSelectedFile();
                if (!file.isFile()) {
                    DialogDisplayer.getDefault().notify(new NotifyDescriptor.Message(
                            NbBundle.getMessage(ArchivePanel.class, "ERROR_NOT_A_FILE", file), //NOI18N
                            NotifyDescriptor.Message.ERROR_MESSAGE));
                } else {
                    File fxzFile = FileUtil.toFile( m_archive.getDataObject().getPrimaryFile());
                    if ( file.equals( fxzFile)) {
                        DialogDisplayer.getDefault().notify(new NotifyDescriptor.Message(
                                NbBundle.getMessage(ArchivePanel.class, "ERROR_CANNOT_INSERT_ARCHIVE_ITSELF", file), //NOI18N
                                NotifyDescriptor.Message.ERROR_MESSAGE));
                    } else {
                        try {
                            m_archive.add(file);
                            update();
                        } catch (Exception ex) {
                            ex.printStackTrace();
                            DialogDisplayer.getDefault().notify(new NotifyDescriptor.Message(
                                    NbBundle.getMessage(ArchivePanel.class, "ERROR_CANNOT_READ_FILE",  //NOI18N
                                    ex.getLocalizedMessage()), NotifyDescriptor.Message.ERROR_MESSAGE));
                        }
                    }
                }
            }
        }
    };      

    final class RemoveArchiveEntryAction extends AbstractFXDAction {
        public RemoveArchiveEntryAction() {  
            super("remove_entry", false);  //NOI18N
        }        
        public void actionPerformed(ActionEvent e) {
            int [] selRows = tableContent.getSelectedRows();
            if ( selRows != null && selRows.length > 0) {
                String msg;
                if ( selRows.length == 1) {
                    msg = String.format( NbBundle.getMessage(ArchivePanel.class, "MSG_REMOVE_ENTRY"), getNameAt(selRows[0])); //NOI18N 
                } else {
                    msg = String.format( NbBundle.getMessage(ArchivePanel.class, "MSG_REMOVE_ENTRIES"), selRows.length); //NOI18N 
                }
                NotifyDescriptor d = new NotifyDescriptor.Confirmation( msg,
                    NbBundle.getMessage(ArchivePanel.class, "TITLE_REMOVE_ENTRY"), //NOI18N 
                    NotifyDescriptor.YES_NO_OPTION,   
                    NotifyDescriptor.WARNING_MESSAGE);
                if ( DialogDisplayer.getDefault().notify(d) == NotifyDescriptor.YES_OPTION) {
                    String [] selNames = new String[ selRows.length];
                    for (int i = 0; i < selNames.length; i++) {
                        selNames[i] = getNameAt(selRows[i]);
                    }
                    m_archive.remove( selNames);
                    update();
                }
            }
        }
    };      
    
    final class ReplaceArchiveEntryAction extends AbstractFXDAction {
        public ReplaceArchiveEntryAction() {  
            super("replace_entry", false);  //NOI18N
        }        

        public void actionPerformed(ActionEvent e) {
            JFileChooser chooser = new JFileChooser();
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            int r = chooser.showDialog( SwingUtilities.getWindowAncestor(ArchivePanel.this),
                    NbBundle.getMessage(ArchivePanel.class, "LBL_CHOOSE_FILE")); //NOI18N
            if (r == JFileChooser.APPROVE_OPTION) {
                final File file = chooser.getSelectedFile();
                if (!file.isFile()) {
                    DialogDisplayer.getDefault().notify(new NotifyDescriptor.Message(
                            NbBundle.getMessage(ArchivePanel.class, "ERROR_NOT_A_FILE", file), //NOI18N
                            NotifyDescriptor.Message.WARNING_MESSAGE));
                } else {
                    try {
                        int row = tableContent.getSelectedRow();
                        if ( row >= 0) {
                            String entryName = getNameAt(row);
                            m_archive.replace( entryName, file);
                            update();
                        }
                    } catch (Exception ex) {
                        ex.printStackTrace();
                        DialogDisplayer.getDefault().notify(new NotifyDescriptor.Message(
                                NbBundle.getMessage(ArchivePanel.class, "ERROR_CANNOT_READ_FILE",  //NOI18N
                                ex.getLocalizedMessage()), NotifyDescriptor.Message.ERROR_MESSAGE));
                    }
                }
            }
        }
    };

    final class ViewArchiveEntryAction extends AbstractFXDAction {

        public ViewArchiveEntryAction() {
            super("view_entry", false);  //NOI18N
        }
        public void actionPerformed(ActionEvent e) {
            int row = tableContent.getSelectedRow();
            if (row >= 0) {
                String entryName = getNameAt(row);
                FXZDataObject dObj = m_archive.getDataObject();
                dObj.selectView(FXZDataObject.VISUAL_VIEW_INDEX);
                dObj.selectEntry(entryName);
            }
        }
    };

    final class EditArchiveEntryAction extends AbstractFXDAction {

        public EditArchiveEntryAction() {
            super("edit_entry", false);  //NOI18N
        }
        public void actionPerformed(ActionEvent e) {
            int row = tableContent.getSelectedRow();
            if (row >= 0) {
                String entryName = getNameAt(row);
                FXZDataObject dObj = m_archive.getDataObject();
                dObj.selectView(FXZDataObject.TEXT_VIEW_INDEX);
                dObj.selectEntry(entryName);
            }
        }
    };

    private class PopupListener extends MouseAdapter {
        private JPopupMenu m_entryPopup;

        public PopupListener() {
            m_entryPopup = new JPopupMenu();
            for (int i = 0; i < m_popup_actions.length; i++){
                if (m_popup_actions[i] == null){
                    m_entryPopup.add(new JSeparator());
                } else if (m_popup_actions[i] instanceof Presenter.Popup){
                    m_entryPopup.add(((Presenter.Popup)m_popup_actions[i]).getPopupPresenter());
                } else {
                    m_entryPopup.add(m_popup_actions[i]);
                }
            }
        }

        @Override
        public void mouseClicked(MouseEvent e) {
            if (e.isPopupTrigger()){
                showPopup(e);
            }else {
                if (e.getClickCount() >= 2 && m_editAction.isEnabled()){
                    m_editAction.actionPerformed(null);
                }
            }
        }

        @Override
        public void mousePressed(MouseEvent e) {
            showPopup(e);
        }

        @Override
        public void mouseReleased(MouseEvent e) {
            showPopup(e);
        }

        private void showPopup(MouseEvent e) {
            int row = tableContent.rowAtPoint(e.getPoint());
            if (row == -1) { //clicked on empty area
                return;
            }
            if (!isRowSelected(row)){
                tableContent.getSelectionModel().setSelectionInterval(row, row);
            }
            
            if (e.isPopupTrigger()) {
                m_entryPopup.show(e.getComponent(), e.getX(), e.getY());
            }
        }

        private boolean isRowSelected(int row){
            return tableContent.getSelectionModel().isSelectedIndex(row);
        }
    }


    /*
     * tests if selected entry is FXD.
     * should be invoked after check that only one row is selected.
     */
    private boolean isFXDEntrySelected(){
        int row = tableContent.getSelectedRow();
        String entryName = getNameAt(row);
        return FXZArchive.isFXDEntry(entryName);
    }

    protected String getNameAt(int row) {
        return (String) tableContent.getModel().getValueAt(row, 0);
    }
    
    private final Action m_newEntryAction = new NewArchiveEntryAction();
    private final Action m_addAction      = new AddArchiveEntryAction();
    private final Action m_removeAction   = new RemoveArchiveEntryAction();
    private final Action m_replaceAction  = new ReplaceArchiveEntryAction();
    private final Action m_viewAction  = new ViewArchiveEntryAction();
    private final Action m_editAction  = new EditArchiveEntryAction();
    
    private final Action [] m_actions = new Action[] {
        m_newEntryAction,
        m_addAction,
        m_removeAction,
        m_replaceAction
    };

    private final Action [] m_popup_actions = new Action[] {
        m_editAction,
        m_viewAction,
        null,
        m_newEntryAction,
        m_addAction,
        m_removeAction,
        m_replaceAction
    };
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JLabel labelTotalSize;
    private javax.swing.JTable tableContent;
    // End of variables declaration//GEN-END:variables
}
