/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Oracle and/or its affiliates. All rights reserved.
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
 *
 * Contributor(s):
 *
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

/*
 * SDKSamplesVisualPanel.java
 *
 * Created on Oct 15, 2008, 11:34:28 AM
 */

package org.netbeans.modules.visage.sdksamples;

import java.io.File;
import javax.swing.JFileChooser;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.Document;
import org.netbeans.spi.project.ui.support.ProjectChooser;
import org.openide.WizardDescriptor;
import org.openide.filesystems.FileUtil;

/**
 *
 * @author Michal Skvor
 */
public class SDKSamplesVisualPanel extends javax.swing.JPanel implements DocumentListener {

    public static final String PROP_PROJECT_NAME = "projectName"; // NOI18N
    private SDKSamplesWizardPanel panel;

    /** Creates new form SDKSamplesVisualPanel */
    public SDKSamplesVisualPanel( SDKSamplesWizardPanel panel ) {
        this.panel = panel;
        initComponents();
        projectNameTextField.getDocument().addDocumentListener( this );
        projectLocationTextField.getDocument().addDocumentListener( this );
    }

    boolean isValid( WizardDescriptor wizardDescriptor ) {
        String message = org.openide.util.NbBundle.getBundle(SDKSamplesVisualPanel.class).getString("Project_Folder_is_not_a_valid_path");
        if( projectNameTextField.getText().length() == 0 ) {
            wizardDescriptor.putProperty("WizardPanel_errorMessage", // NOI18N
                    org.openide.util.NbBundle.getBundle(SDKSamplesVisualPanel.class).getString("Project_Name_is_not_a_valid_folder_name"));
            return false; // Display name not specified
        }
        if( projectNameTextField.getText().contains( "<" ) || projectNameTextField.getText().contains( ">" )) { // NOI18N
            wizardDescriptor.putProperty("WizardPanel_errorMessage", // NOI18N
                    org.openide.util.NbBundle.getBundle(SDKSamplesVisualPanel.class).getString("Project_Name_is_not_a_valid_folder_name"));
            return false;
        }
        File f = FileUtil.normalizeFile( new File( projectLocationTextField.getText()).getAbsoluteFile());
        if( !f.isDirectory()) {
            wizardDescriptor.putProperty( "WizardPanel_errorMessage", message ); // NOI18N
            return false;
        }
        final File destFolder = FileUtil.normalizeFile(new File( projectFolderTextField.getText()).getAbsoluteFile());

        File projLoc = destFolder;
        while( projLoc != null && !projLoc.exists()) {
            projLoc = projLoc.getParentFile();
        }
        if( projLoc == null || !projLoc.canWrite()) {
            wizardDescriptor.putProperty( "WizardPanel_errorMessage", // NOI18N
                    org.openide.util.NbBundle.getBundle(SDKSamplesVisualPanel.class).getString("Project_Folder_cannot_be_created") );
            return false;
        }

        if( FileUtil.toFileObject( projLoc ) == null ) {
            wizardDescriptor.putProperty( "WizardPanel_errorMessage", message ); // NOI18N
            return false;
        }

        File[] kids = destFolder.listFiles();
        if( destFolder.exists() && kids != null && kids.length > 0 ) {
            // Folder exists and is not empty
            wizardDescriptor.putProperty( "WizardPanel_errorMessage", // NOI18N
                    org.openide.util.NbBundle.getBundle(SDKSamplesVisualPanel.class).getString("Project_Folder_already_exists_and_is_not_empty") );
            return false;
        }
        wizardDescriptor.putProperty( "WizardPanel_errorMessage", "" ); // NOI18N

        return true;
    }

    void load( WizardDescriptor settings ) {
        File projectLocation = (File) settings.getProperty("projdir"); // NOI18N
        if (projectLocation == null || projectLocation.getParentFile() == null || !projectLocation.getParentFile().isDirectory()) {
            projectLocation = ProjectChooser.getProjectsFolder();
        } else {
            projectLocation = projectLocation.getParentFile();
        }
        this.projectLocationTextField.setText( projectLocation.getAbsolutePath());

        String projectName = (String) settings.getProperty("name"); // NOI18N
        if( projectName == null ) {
            projectName = panel.getFile().getName();
        }
        this.projectNameTextField.setText( projectName );
        this.projectNameTextField.selectAll();
    }

    void store( WizardDescriptor d ) {
        String name = projectNameTextField.getText().trim();
        String folder = projectFolderTextField.getText().trim();

        d.putProperty("projdir", new File(folder)); // NOI18N
        d.putProperty("name", name); // NOI18N
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        projectNameLabel = new javax.swing.JLabel();
        projectLocationLabel = new javax.swing.JLabel();
        projectFolderLabel = new javax.swing.JLabel();
        projectFolderTextField = new javax.swing.JTextField();
        projectLocationTextField = new javax.swing.JTextField();
        projectNameTextField = new javax.swing.JTextField();
        browseButton = new javax.swing.JButton();

        org.openide.awt.Mnemonics.setLocalizedText(projectNameLabel, org.openide.util.NbBundle.getMessage(SDKSamplesVisualPanel.class, "SDKSamplesVisualPanel.projectNameLabel.text")); // NOI18N

        org.openide.awt.Mnemonics.setLocalizedText(projectLocationLabel, org.openide.util.NbBundle.getMessage(SDKSamplesVisualPanel.class, "SDKSamplesVisualPanel.projectLocationLabel.text")); // NOI18N

        org.openide.awt.Mnemonics.setLocalizedText(projectFolderLabel, org.openide.util.NbBundle.getMessage(SDKSamplesVisualPanel.class, "SDKSamplesVisualPanel.projectFolderLabel.text")); // NOI18N

        projectFolderTextField.setText(org.openide.util.NbBundle.getMessage(SDKSamplesVisualPanel.class, "SDKSamplesVisualPanel.projectFolderTextField.text")); // NOI18N
        projectFolderTextField.setEnabled(false);

        projectLocationTextField.setText(org.openide.util.NbBundle.getMessage(SDKSamplesVisualPanel.class, "SDKSamplesVisualPanel.projectLocationTextField.text")); // NOI18N

        projectNameTextField.setEditable(false);
        projectNameTextField.setText(org.openide.util.NbBundle.getMessage(SDKSamplesVisualPanel.class, "SDKSamplesVisualPanel.projectNameTextField.text")); // NOI18N

        org.openide.awt.Mnemonics.setLocalizedText(browseButton, org.openide.util.NbBundle.getMessage(SDKSamplesVisualPanel.class, "SDKSamplesVisualPanel.browseButton.text")); // NOI18N
        browseButton.setActionCommand(org.openide.util.NbBundle.getMessage(SDKSamplesVisualPanel.class, "SDKSamplesVisualPanel.browseButton.actionCommand")); // NOI18N
        browseButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                browseButtonActionPerformed(evt);
            }
        });

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(projectNameLabel)
                    .add(projectLocationLabel)
                    .add(projectFolderLabel))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(projectFolderTextField, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 206, Short.MAX_VALUE)
                    .add(projectLocationTextField, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 206, Short.MAX_VALUE)
                    .add(projectNameTextField, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 206, Short.MAX_VALUE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(browseButton)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(projectNameLabel)
                    .add(projectNameTextField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(projectLocationLabel)
                    .add(browseButton)
                    .add(projectLocationTextField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(projectFolderLabel)
                    .add(projectFolderTextField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(214, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    private void browseButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_browseButtonActionPerformed
        String command = evt.getActionCommand();
        if( "BROWSE".equals( command )) { // NOI18N
            JFileChooser chooser = new JFileChooser();
            FileUtil.preventFileChooserSymlinkTraversal(chooser, null);
            chooser.setDialogTitle(org.openide.util.NbBundle.getBundle(SDKSamplesVisualPanel.class).getString("Select_Project_Location"));
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            String path = this.projectLocationTextField.getText();
            if (path.length() > 0) {
                File f = new File( path );
                if( f.exists()) {
                    chooser.setSelectedFile(f);
                }
            }
            if( JFileChooser.APPROVE_OPTION == chooser.showOpenDialog( this )) {
                File projectDir = chooser.getSelectedFile();
                projectLocationTextField.setText(FileUtil.normalizeFile(projectDir).getAbsolutePath());
            }
            panel.fireChangeEvent();
        }
}//GEN-LAST:event_browseButtonActionPerformed

    public void insertUpdate(DocumentEvent e) {
        changedUpdate( e );
    }

    public void removeUpdate(DocumentEvent e) {
        changedUpdate( e );
    }

    public void changedUpdate(DocumentEvent e) {
        updateTexts( e );
        if( this.projectNameTextField.getDocument() == e.getDocument()) {
            firePropertyChange( PROP_PROJECT_NAME, null, this.projectNameTextField.getText());
        }
    }

    private void updateTexts( DocumentEvent e ) {
        Document doc = e.getDocument();

        if( doc == projectNameTextField.getDocument() || doc == projectLocationTextField.getDocument()) {
            // Change in the project name
            String projectName = projectNameTextField.getText();
            String projectFolder = projectLocationTextField.getText();

            //if (projectFolder.trim().length() == 0 || projectFolder.equals(oldName)) {
            projectFolderTextField.setText(projectFolder + File.separatorChar + projectName);
            //}

        }
        panel.fireChangeEvent();
    }


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton browseButton;
    private javax.swing.JLabel projectFolderLabel;
    private javax.swing.JTextField projectFolderTextField;
    private javax.swing.JLabel projectLocationLabel;
    private javax.swing.JTextField projectLocationTextField;
    private javax.swing.JLabel projectNameLabel;
    private javax.swing.JTextField projectNameTextField;
    // End of variables declaration//GEN-END:variables

}
