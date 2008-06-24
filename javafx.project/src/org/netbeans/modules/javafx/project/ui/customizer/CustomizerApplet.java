/*
 * CustomizerApplet.java
 *
 * Created on June 17, 2008, 7:26 PM
 */

package org.netbeans.modules.javafx.project.ui.customizer;

/**
 *
 * @author  alex
 */
public class CustomizerApplet extends javax.swing.JPanel {

    /** Creates new form CustomizerApplet */
    public CustomizerApplet(JavaFXProjectProperties uiProperties) {
        initComponents();
        
        javaScriptCheckBox.setModel(uiProperties.javaScriptModel);
        jnlpFileCheckBox.setModel(uiProperties.jnlpFileModel);
        draggableCheckBox.setModel(uiProperties.draggableModel);
        runInBrowserCheckBox.setModel(uiProperties.runAppletInBrowser);
        javaArgumentsTextField.setDocument(uiProperties.javaArgumentsDocument);
        boolean isSelected = javaScriptCheckBox.getModel().isSelected();
        noteLabel.setVisible(false);
        boolean enabled = runInBrowserCheckBox.getModel().isSelected();
        setEnabledAllComponents(enabled);
        if (!enabled) {
            setSelectedAllComponents(enabled);
        }
        
    }
    private void setEnabledAllComponents(boolean enabled) {
        javaScriptCheckBox.setEnabled(enabled);
        jnlpFileCheckBox.setEnabled(enabled);
        draggableCheckBox.setEnabled(enabled);
    }
    private void setSelectedAllComponents(boolean selected) {
        javaScriptCheckBox.getModel().setSelected(selected);
        jnlpFileCheckBox.getModel().setSelected(selected);
        draggableCheckBox.getModel().setSelected(selected);
    }
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        javaScriptCheckBox = new javax.swing.JCheckBox();
        jnlpFileCheckBox = new javax.swing.JCheckBox();
        descriptionLabel = new javax.swing.JLabel();
        draggableCheckBox = new javax.swing.JCheckBox();
        javaArgumentsTextField = new javax.swing.JTextField();
        javaArgumentsLabel = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        runInBrowserCheckBox = new javax.swing.JCheckBox();
        noteLabel = new javax.swing.JLabel();

        setMinimumSize(new java.awt.Dimension(499, 239));
        setPreferredSize(new java.awt.Dimension(499, 239));
        setLayout(new java.awt.GridBagLayout());

        javaScriptCheckBox.setText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.javaScriptCheckBox.text")); // NOI18N
        javaScriptCheckBox.setToolTipText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.javaScriptCheckBox.toolTipText")); // NOI18N
        javaScriptCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                javaScriptCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(15, 10, 0, 2);
        add(javaScriptCheckBox, gridBagConstraints);

        jnlpFileCheckBox.setText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.jnlpFileCheckBox.text")); // NOI18N
        jnlpFileCheckBox.setToolTipText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.jnlpFileCheckBox.toolTipText")); // NOI18N
        jnlpFileCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jnlpFileCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(10, 10, 0, 2);
        add(jnlpFileCheckBox, gridBagConstraints);

        descriptionLabel.setText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.descriptionLabel.text")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 9, 0);
        add(descriptionLabel, gridBagConstraints);

        draggableCheckBox.setText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.draggableCheckBox.text")); // NOI18N
        draggableCheckBox.setToolTipText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.draggableCheckBox.toolTipText")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(10, 10, 6, 2);
        add(draggableCheckBox, gridBagConstraints);

        javaArgumentsTextField.setText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.javaArgumentsTextField.text")); // NOI18N
        javaArgumentsTextField.setToolTipText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.javaArgumentsTextField.toolTipText")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(10, 2, 0, 12);
        add(javaArgumentsTextField, gridBagConstraints);

        javaArgumentsLabel.setText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.javaArgumentsLabel.text")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(10, 2, 0, 2);
        add(javaArgumentsLabel, gridBagConstraints);

        jLabel1.setText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.jLabel1.text")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 2);
        add(jLabel1, gridBagConstraints);

        runInBrowserCheckBox.setText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.runInBrowserCheckBox.text")); // NOI18N
        runInBrowserCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                runInBrowserCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 0);
        add(runInBrowserCheckBox, gridBagConstraints);

        noteLabel.setText(org.openide.util.NbBundle.getMessage(CustomizerApplet.class, "CustomizerApplet.noteLabel.text")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(28, 2, 2, 2);
        add(noteLabel, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

private void runInBrowserCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_runInBrowserCheckBoxActionPerformed
// TODO add your handling code here:
    boolean enabled = runInBrowserCheckBox.getModel().isSelected();
    if (!enabled) {
        setSelectedAllComponents(enabled);
    }
    setEnabledAllComponents(enabled);
}//GEN-LAST:event_runInBrowserCheckBoxActionPerformed

private void javaScriptCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_javaScriptCheckBoxActionPerformed
// TODO add your handling code here:
    boolean isSelected = javaScriptCheckBox.getModel().isSelected();
    if (isSelected) {
        jnlpFileCheckBox.getModel().setSelected(!isSelected);
    }
}//GEN-LAST:event_javaScriptCheckBoxActionPerformed
private void jnlpFileCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {
    boolean isSelected = jnlpFileCheckBox.getModel().isSelected();
    if (isSelected) {
        javaScriptCheckBox.getModel().setSelected(!isSelected);
    }
    
}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel descriptionLabel;
    private javax.swing.JCheckBox draggableCheckBox;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel javaArgumentsLabel;
    private javax.swing.JTextField javaArgumentsTextField;
    private javax.swing.JCheckBox javaScriptCheckBox;
    private javax.swing.JCheckBox jnlpFileCheckBox;
    private javax.swing.JLabel noteLabel;
    private javax.swing.JCheckBox runInBrowserCheckBox;
    // End of variables declaration//GEN-END:variables

}
