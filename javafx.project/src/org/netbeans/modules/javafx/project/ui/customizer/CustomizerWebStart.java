/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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

package org.netbeans.modules.javafx.project.ui.customizer;

import java.io.File;

import javax.swing.JPanel;
import javax.swing.filechooser.FileFilter;

import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

import org.netbeans.modules.javafx.project.ui.customizer.WebStartProjectProperties.CodebaseComboBoxModel;

/**
 *
 * @author  Milan Kubec
 */
public class CustomizerWebStart extends JPanel implements HelpCtx.Provider {
    
    private WebStartProjectProperties jwsProps;
    private File lastImageFolder = null;
    
    /** Creates new form JWSCustomizerPanel */
    public CustomizerWebStart(WebStartProjectProperties props) {
        
        this.jwsProps = props;
        
        initComponents();
                
//        codebaseComboBox.setModel(jwsProps.codebaseModel);
//        codebaseTextField.setDocument(jwsProps.codebaseURLDocument);
        jCheckBox1.setModel(jwsProps.enablePack200Model);
        jCheckBox2.setModel(jwsProps.signedModel);
//        setCodebaseComponents();
        
    }
    
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        panelDescLabel = new javax.swing.JLabel();
        jCheckBox2 = new javax.swing.JCheckBox();
        jCheckBox1 = new javax.swing.JCheckBox();

        setLayout(new java.awt.GridBagLayout());

        org.openide.awt.Mnemonics.setLocalizedText(panelDescLabel, org.openide.util.NbBundle.getMessage(CustomizerWebStart.class, "CustomizerWebStart.panelDescLabel.text")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 10, 2);
        add(panelDescLabel, gridBagConstraints);

        org.openide.awt.Mnemonics.setLocalizedText(jCheckBox2, org.openide.util.NbBundle.getMessage(CustomizerWebStart.class, "CustomizerWebStart.jCheckBox2.text")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 6, 0, 0);
        add(jCheckBox2, gridBagConstraints);

        org.openide.awt.Mnemonics.setLocalizedText(jCheckBox1, org.openide.util.NbBundle.getMessage(CustomizerWebStart.class, "CustomizerWebStart.jCheckBox1.text")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 6, 0, 0);
        add(jCheckBox1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    public HelpCtx getHelpCtx() {
        return new HelpCtx(CustomizerWebStart.class);
    }
    
    private static class IconFileFilter extends FileFilter {
        
        // XXX should check size of images?
        public boolean accept(File f) {
            if (f.isDirectory()) {
                return true;
            }
            String name = f.getName();
            int index = name.lastIndexOf('.');
            if (index > 0 && index < name.length() - 1) {
                String ext = name.substring(index+1).toLowerCase();
                if ("gif".equals(ext) || "png".equals(ext) || "jpg".equals(ext)) { // NOI18N
                    return true;
                }
            }
            return false;
        }
        
        public String getDescription() {
            return NbBundle.getMessage(CustomizerWebStart.class, "MSG_IconFileFilter_Description");
        }
        
    }
    
//    private CodebaseComboBoxModel getCBModel() {
//        return (CodebaseComboBoxModel) codebaseComboBox.getModel();
//    }
//    
//    private void setCodebaseComponents() {
//        String value = getCBModel().getSelectedCodebaseItem();
//        if (WebStartProjectProperties.CB_TYPE_LOCAL.equals(value)) {
//            codebaseTextField.setText(jwsProps.getProjectDistDir());
//            codebaseTextField.setEditable(false);
//        } else if (WebStartProjectProperties.CB_TYPE_USER.equals(value)) {
//            codebaseTextField.setText(jwsProps.getCodebaseLocation());
//            codebaseTextField.setEditable(true);
//        }
//    }
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox jCheckBox1;
    private javax.swing.JCheckBox jCheckBox2;
    private javax.swing.JLabel panelDescLabel;
    // End of variables declaration//GEN-END:variables
    
}
