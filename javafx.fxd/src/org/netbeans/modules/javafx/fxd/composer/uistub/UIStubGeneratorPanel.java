/*
 * UIStubGeneratorPanel.java
 *
 * Created on October 13, 2008, 3:00 PM
 */

package org.netbeans.modules.javafx.fxd.composer.uistub;

import java.io.File;
import javax.swing.JFileChooser;
import javax.swing.SwingUtilities;
import org.openide.util.NbBundle;

/**
 *
 * @author  Pavel Benes
 */
final class UIStubGeneratorPanel extends javax.swing.JPanel {

    private final UIStubGenerator m_generator;
    private File m_initialDirectory = null;
    
    /** Creates new form UIStubGeneratorPanel */
    public UIStubGeneratorPanel( UIStubGenerator generator) {
        m_generator = generator;
        initComponents();
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
        jLabel2 = new javax.swing.JLabel();
        packageTextBox = new javax.swing.JTextField();

        jLabel1.setText(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.jLabel1.text")); // NOI18N

        stubLocationTextBox.setText(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.stubLocationTextBox.text")); // NOI18N

        stubLocationButton.setText(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.stubLocationButton.text")); // NOI18N
        stubLocationButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                stubLocationButtonActionPerformed(evt);
            }
        });

        jLabel2.setText(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.jLabel2.text")); // NOI18N

        packageTextBox.setText(org.openide.util.NbBundle.getMessage(UIStubGeneratorPanel.class, "UIStubGeneratorPanel.packageTextBox.text")); // NOI18N

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(jLabel1)
                    .add(jLabel2))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(packageTextBox, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 300, Short.MAX_VALUE)
                    .add(stubLocationTextBox, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 300, Short.MAX_VALUE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(stubLocationButton, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 41, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
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
                .addContainerGap(45, Short.MAX_VALUE))
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
        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
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

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JTextField packageTextBox;
    private javax.swing.JButton stubLocationButton;
    private javax.swing.JTextField stubLocationTextBox;
    // End of variables declaration//GEN-END:variables

}
