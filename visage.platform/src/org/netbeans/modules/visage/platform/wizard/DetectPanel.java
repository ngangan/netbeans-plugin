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

package org.netbeans.modules.visage.platform.wizard;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileSystemView;
import javax.swing.filechooser.FileView;
import org.netbeans.api.java.platform.JavaPlatform;
import org.netbeans.api.java.platform.JavaPlatformManager;
import org.openide.modules.InstalledFileLocator;
import org.openide.util.NbBundle;
import org.openide.util.HelpCtx;
import org.openide.WizardDescriptor;
import org.openide.filesystems.FileUtil;
import org.openide.util.ChangeSupport;
import org.openide.util.Utilities;

/**
 * This Panel launches autoconfiguration during the New Visage Platform sequence.
 * The UI views properties of the platform, reacts to the end of detection by
 * updating itself. It triggers the detection task when the button is pressed.
 * The inner class WizardPanel acts as a controller, reacts to the UI completness
 * (jdk name filled in) and autoconfig result (passed successfully) - and manages
 * Next/Finish button (valid state) according to those.
 *
 * @author Svata Dedic
 */
public class DetectPanel extends javax.swing.JPanel {

    private static final Icon BADGE = new ImageIcon(Utilities.loadImage("org/netbeans/modules/visage/platform/resources/platformBadge.gif")); // NOI18N
    private static final Icon EMPTY = new ImageIcon(Utilities.loadImage("org/netbeans/modules/visage/platform/resources/empty.gif")); // NOI18N

    private final ChangeSupport cs = new ChangeSupport(this);

    /**
     * Creates a detect panel
     * start the task and update on its completion
     * @param primaryPlatform the platform being customized.
     */
    public DetectPanel() {
        initComponents();
        postInitComponents ();
        putClientProperty("WizardPanel_contentData", // NOI18N
            new String[] {
                NbBundle.getMessage(DetectPanel.class,"TITLE_PlatformName"), // NOI18N
        });
        this.setName (NbBundle.getMessage(DetectPanel.class,"TITLE_PlatformName")); // NOI18N
    }

    public void addNotify() {
        super.addNotify();        
    }    

    private void postInitComponents () {
        DocumentListener lsn = new DocumentListener () {
            public void insertUpdate(DocumentEvent e) {
                cs.fireChange();
            }

            public void removeUpdate(DocumentEvent e) {
                cs.fireChange();
            }

            public void changedUpdate(DocumentEvent e) {
                cs.fireChange();
            }
        };                
        jdkName.getDocument().addDocumentListener(lsn);
        fxFolder.getDocument().addDocumentListener(lsn);
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        buttonGroup1 = new javax.swing.ButtonGroup();
        jLabel3 = new javax.swing.JLabel();
        jdkName = new javax.swing.JTextField();
        jLabel5 = new javax.swing.JLabel();
        fxFolder = new javax.swing.JTextField();
        jButton4 = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();

        setLayout(new java.awt.GridBagLayout());

        jLabel3.setLabelFor(jdkName);
        org.openide.awt.Mnemonics.setLocalizedText(jLabel3, NbBundle.getBundle(DetectPanel.class).getString("LBL_DetailsPanel_Name")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(11, 0, 0, 0);
        add(jLabel3, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(11, 5, 0, 0);
        add(jdkName, gridBagConstraints);
        jdkName.getAccessibleContext().setAccessibleDescription(org.openide.util.NbBundle.getBundle(DetectPanel.class).getString("AD_PlatformName")); // NOI18N

        jLabel5.setLabelFor(fxFolder);
        org.openide.awt.Mnemonics.setLocalizedText(jLabel5, org.openide.util.NbBundle.getMessage(DetectPanel.class, "TXT_FX")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(11, 0, 0, 0);
        add(jLabel5, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(11, 5, 0, 0);
        add(fxFolder, gridBagConstraints);

        org.openide.awt.Mnemonics.setLocalizedText(jButton4, org.openide.util.NbBundle.getMessage(DetectPanel.class, "LBL_BrowseFX")); // NOI18N
        jButton4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton4ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(11, 5, 0, 0);
        add(jButton4, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.weighty = 1.0;
        add(jPanel1, gridBagConstraints);

        getAccessibleContext().setAccessibleDescription(org.openide.util.NbBundle.getBundle(DetectPanel.class).getString("AD_DetectPanel")); // NOI18N
    }// </editor-fold>//GEN-END:initComponents

private void jButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton4ActionPerformed
        JFileChooser chooser = new JFileChooser ();
        final FileSystemView fsv = chooser.getFileSystemView();
        chooser.setFileView(new FileView() {
            private Icon lastOriginal;
            private Icon lastMerged;
            public Icon getIcon(File _f) {
                File f = FileUtil.normalizeFile(_f);
                Icon original = fsv.getSystemIcon(f);
                if (original == null) {
                    // L&F (e.g. GTK) did not specify any icon.
                    original = EMPTY;
                }
                if ((new File(f, "bin/visagepackager.exe").isFile() || new File(f, "bin/visagepackager").isFile()) && new File(f, "lib/shared/visagec.jar").isFile() && new File(f, "lib/shared/visagert.jar").isFile()) { // NOI18N
                    if ( original.equals( lastOriginal ) ) {
                        return lastMerged;
                    }
                    lastOriginal = original;
                    lastMerged = new MergedIcon(original, BADGE, -1, -1);                
                    return lastMerged;
                } else {
                    return original;
                }
            }
        });
        FileUtil.preventFileChooserSymlinkTraversal(chooser, null);
        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        File f = new File (fxFolder.getText());
        chooser.setSelectedFile(f);
        chooser.setDialogTitle (NbBundle.getMessage(DetectPanel.class, "TITLE_SelectFX")); // NOI18N
        if (chooser.showOpenDialog (this) == JFileChooser.APPROVE_OPTION) {
            fxFolder.setText(chooser.getSelectedFile().getAbsolutePath());
        }
}//GEN-LAST:event_jButton4ActionPerformed
    
    public final synchronized void addChangeListener (ChangeListener listener) {
        cs.addChangeListener(listener);
    }

    public final synchronized void removeChangeListener (ChangeListener listener) {
        cs.removeChangeListener(listener);
    }

    public String getPlatformName() {
	 return jdkName.getText();
    }
    
    public File getPlatformFolder() {
        return FileUtil.toFile(JavaPlatformManager.getDefault().getDefaultPlatform().getInstallFolders().iterator().next());
    }
    
    public File getFxFolder() {
 	 return FileUtil.normalizeFile(new File(fxFolder.getText()));
    }
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JTextField fxFolder;
    private javax.swing.JButton jButton4;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JTextField jdkName;
    // End of variables declaration//GEN-END:variables

    /**
     * Controller for the outer class: manages wizard panel's valid state
     * according to the user's input and detection state.
     */
    static class WizardPanel implements WizardDescriptor.Panel<WizardDescriptor>,ChangeListener {
        private DetectPanel         component;
        private final VisageWizardIterator  iterator;
        private final ChangeSupport cs = new ChangeSupport(this);
        private boolean             valid;
        private WizardDescriptor    wiz;

        WizardPanel(VisageWizardIterator iterator) {            
	    this.iterator = iterator;
        }

        public void addChangeListener(ChangeListener l) {
            cs.addChangeListener(l);
        }

        public java.awt.Component getComponent() {
            if (component == null) {
                component = new DetectPanel();
                component.addChangeListener (this);
            }
            return component;
        }

        void setValid(boolean v) {
            if (v == valid) return;
            valid = v;
            cs.fireChange();
        }

        public HelpCtx getHelp() {
            return new HelpCtx (DetectPanel.class);
        }

        public boolean isValid() {
            return valid;
        }

        public void readSettings(WizardDescriptor settings) {           
            this.wiz = settings;
            String name;
            int i = 1;
            while (!checkName(name = NbBundle.getMessage(DetectPanel.class, "TXT_DefaultPlaformName", String.valueOf(i)))) i++; // NOI18N
            component.jdkName.setText(name);
            File fxPath = InstalledFileLocator.getDefault().locate("visage-sdk/lib/shared/visagec.jar", "org.netbeans.modules.visage", false); // NOI18N
            if (fxPath == null) //try to find runtime in the root visage folder as for public compiler
                fxPath = InstalledFileLocator.getDefault().locate("lib/shared/visagec.jar", "org.netbeans.modules.visage", false); // NOI18N
            if (fxPath != null && fxPath.isFile()) component.fxFolder.setText(fxPath.getParentFile().getParentFile().getParent());
            File f = component.getPlatformFolder();
            checkValid();
        }

        public void removeChangeListener(ChangeListener l) {
            cs.removeChangeListener(l);
        }

	/**
	 Updates the Platform's display name with the one the user
	 has entered. Stores user-customized display name into the Platform.
	 */
        public void storeSettings(WizardDescriptor settings) {
            if (isValid()) {                                
                iterator.platformName = component.getPlatformName(); 
                iterator.installFolder = component.getPlatformFolder();
                iterator.fxFolder = component.getFxFolder();
            }
        }

        public void stateChanged(ChangeEvent e) {
             this.checkValid();
        }

        private void setErrorMessage(String key) {
             this.wiz.putProperty("WizardPanel_errorMessage", NbBundle.getMessage(DetectPanel.class, key));    //NOI18N
             setValid(false);
        }
        
        private boolean checkName(String name) {
            JavaPlatform[] platforms = JavaPlatformManager.getDefault().getInstalledPlatforms();                
            for (int i=0; i<platforms.length; i++) {
                if (name.equals (platforms[i].getDisplayName())) {
                    setErrorMessage("ERROR_UsedDisplayName");    //NOI18N
                    return false;
                }
            }
            return true;
        }
        
        private void checkValid () {
            String name = this.component.getPlatformName ();            
            if (name.length() == 0) {
                setErrorMessage("ERROR_InvalidDisplayName"); //NOI18N
                return;
            }                
            if (!checkName(name)) {
                setErrorMessage("ERROR_UsedDisplayName");    //NOI18N
                return;
            }
//            File f = component.getPlatformFolder();
//            if (!new File(f, "bin/java").isFile() && !new File(f, "bin/java.exe").isFile()) {
//                setErrorMessage("ERROR_WrongJavaPlatformLocation"); //NOI18N
//                 return;
//            }
            File f = component.getFxFolder();
//            if (!(new File(f, "bin/visagepackager.exe").isFile() || 
//                  new File(f, "bin/visagepackager").isFile()) || 
//                  !new File(f, "lib/shared/visagec.jar").isFile() || 
//                  !new File(f, "lib/shared/visagert.jar").isFile()) { // NOI18N
            if (!(new File(f, "bin/visage.exe").isFile() || 
                  new File(f, "bin/visage").isFile()) || 
                  !new File(f, "lib/shared/visagec.jar").isFile() || 
                  !new File(f, "lib/shared/visagert.jar").isFile()) { // NOI18N
                setErrorMessage("ERROR_WrongFxLocation"); //NOI18N
                 return;
            }
            this.wiz.putProperty("WizardPanel_errorMessage", ""); //NOI18N
            String pv = "???";//NOI18N
            f = new File(f, "timestamp");
            if (f.isFile()) try {
                FileInputStream in = new FileInputStream(f);
                Properties ts = new Properties();
                ts.load(in);
                in.close();
                pv = ts.getProperty("Product"); //NOI18N
            } catch (IOException e) {
                //ignore
            }
            if (!pv.startsWith("visage-1.3")) this.wiz.putProperty("WizardPanel_errorMessage", NbBundle.getMessage(DetectPanel.class, "WARNING_WrongVersion", pv));    //NOI18N
            setValid(true);            
        }
    }    

    private static class MergedIcon implements Icon {
        
        private Icon icon1;
        private Icon icon2;
        private int xMerge;
        private int yMerge;
        
        MergedIcon( Icon icon1, Icon icon2, int xMerge, int yMerge ) {
            
            this.icon1 = icon1;
            this.icon2 = icon2;
            
            if ( xMerge == -1 ) {
                xMerge = icon1.getIconWidth() - icon2.getIconWidth();
            }
            
            if ( yMerge == -1 ) {
                yMerge = icon1.getIconHeight() - icon2.getIconHeight();
            }
            
            this.xMerge = xMerge;
            this.yMerge = yMerge;
        }
        
        public int getIconHeight() {
            return Math.max( icon1.getIconHeight(), yMerge + icon2.getIconHeight() );
        }
        
        public int getIconWidth() {
            return Math.max( icon1.getIconWidth(), yMerge + icon2.getIconWidth() );
        }
        
        public void paintIcon(java.awt.Component c, java.awt.Graphics g, int x, int y) {
            icon1.paintIcon( c, g, x, y );
            icon2.paintIcon( c, g, x + xMerge, y + yMerge );
        }
        
    }
}
