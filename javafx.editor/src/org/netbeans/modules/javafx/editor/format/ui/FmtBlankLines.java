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

package org.netbeans.modules.javafx.editor.format.ui;

import static org.netbeans.modules.javafx.editor.format.ui.FmtOptions.*;
import static org.netbeans.modules.javafx.editor.format.ui.FmtOptions.CategorySupport.OPTION_ID;
import org.netbeans.modules.options.editor.spi.PreferencesCustomizer;

/**
 *
 * @author  phrebejk
 */
public class FmtBlankLines extends javax.swing.JPanel {
    
    /** Creates new form FmtBlankLines */
    public FmtBlankLines() {
        initComponents();
        
        bPackageField.putClientProperty(OPTION_ID, blankLinesBeforePackage );
        aPackageField.putClientProperty(OPTION_ID, blankLinesAfterPackage);
        bImportsField.putClientProperty(OPTION_ID, blankLinesBeforeImports);
        aImportsField.putClientProperty(OPTION_ID, blankLinesAfterImports);
        bClassField.putClientProperty(OPTION_ID, blankLinesBeforeClass);
        aClassField.putClientProperty(OPTION_ID, blankLinesAfterClass);
        aClassHeaderField.putClientProperty(OPTION_ID, blankLinesAfterClassHeader);
        bAttributeField.putClientProperty(OPTION_ID, blankLinesBeforeFields);
        aAttributeField.putClientProperty(OPTION_ID, blankLinesAfterFields);
        bFunctionField.putClientProperty(OPTION_ID, blankLinesBeforeMethods );
        aFunctionField.putClientProperty(OPTION_ID, blankLinesAfterMethods);
        bNonClassExpressionField.putClientProperty(OPTION_ID, blankLinesBeforeNonClassExpression);
        aNonClassExpressionField.putClientProperty(OPTION_ID, blankLinesAfterNonClassExpression);
        bNonClassAttributeField.putClientProperty(OPTION_ID, blankLinesBeforeNonClassAttribute);
        aNonClassAttributeField.putClientProperty(OPTION_ID, blankLinesAfterNonClassAttribute);
        bNonClassFunctionField.putClientProperty(OPTION_ID, blankLinesBeforeNonClassFunction);
        aNonClassFunctionField.putClientProperty(OPTION_ID, blankLinesAfterNonClassFunction);
        
        bPackageField.addKeyListener(new NumericKeyListener());
        aPackageField.addKeyListener(new NumericKeyListener());
        bImportsField.addKeyListener(new NumericKeyListener());
        aImportsField.addKeyListener(new NumericKeyListener());
        bClassField.addKeyListener(new NumericKeyListener());
        aClassField.addKeyListener(new NumericKeyListener());
        aClassHeaderField.addKeyListener(new NumericKeyListener());
        bAttributeField.addKeyListener(new NumericKeyListener());
        aAttributeField.addKeyListener(new NumericKeyListener());
        bFunctionField.addKeyListener(new NumericKeyListener());
        aFunctionField.addKeyListener(new NumericKeyListener());
        bNonClassExpressionField.addKeyListener(new NumericKeyListener());
        aNonClassExpressionField.addKeyListener(new NumericKeyListener());
        bNonClassAttributeField.addKeyListener(new NumericKeyListener());
        aNonClassAttributeField.addKeyListener(new NumericKeyListener());
        bNonClassFunctionField.addKeyListener(new NumericKeyListener());
        aNonClassFunctionField.addKeyListener(new NumericKeyListener());

    }
    
    public static PreferencesCustomizer.Factory getController() {
        return new CategorySupport.Factory("blank-lines", FmtBlankLines.class, //NOI18N
                 org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "SAMPLE_BlankLines")); // NOI18N
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        bPackageLabel = new javax.swing.JLabel();
        bPackageField = new javax.swing.JTextField();
        aPackageLabel = new javax.swing.JLabel();
        aPackageField = new javax.swing.JTextField();
        bImportsLabel = new javax.swing.JLabel();
        bImportsField = new javax.swing.JTextField();
        aImports = new javax.swing.JLabel();
        aImportsField = new javax.swing.JTextField();
        bClassLabel = new javax.swing.JLabel();
        bClassField = new javax.swing.JTextField();
        aClassLabel = new javax.swing.JLabel();
        aClassField = new javax.swing.JTextField();
        aClassHeaderLabel = new javax.swing.JLabel();
        aClassHeaderField = new javax.swing.JTextField();
        bAttributeLabel = new javax.swing.JLabel();
        bAttributeField = new javax.swing.JTextField();
        aAttributeLabel = new javax.swing.JLabel();
        aAttributeField = new javax.swing.JTextField();
        bFunctionLabel = new javax.swing.JLabel();
        bFunctionField = new javax.swing.JTextField();
        aFunctionLabel = new javax.swing.JLabel();
        aFunctionField = new javax.swing.JTextField();
        bNonClassExpressionLabel = new javax.swing.JLabel();
        bNonClassExpressionField = new javax.swing.JTextField();
        aNonClassExpressionLabel = new javax.swing.JLabel();
        aNonClassExpressionField = new javax.swing.JTextField();
        bNonClassAttributeLabel = new javax.swing.JLabel();
        bNonClassAttributeField = new javax.swing.JTextField();
        aNonClassAttributeLabel = new javax.swing.JLabel();
        aNonClassAttributeField = new javax.swing.JTextField();
        bNonClassFunctionLabel = new javax.swing.JLabel();
        bNonClassFunctionField = new javax.swing.JTextField();
        aNonClassFunctionLabel = new javax.swing.JLabel();
        aNonClassFunctionField = new javax.swing.JTextField();

        setName(org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "LBL_BlankLines")); // NOI18N
        setOpaque(false);

        bPackageLabel.setLabelFor(bPackageField);
        org.openide.awt.Mnemonics.setLocalizedText(bPackageLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "LBL_blBeforePackage")); // NOI18N

        bPackageField.setColumns(5);

        aPackageLabel.setLabelFor(aPackageField);
        org.openide.awt.Mnemonics.setLocalizedText(aPackageLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "LBL_blAfterPackage")); // NOI18N

        aPackageField.setColumns(5);

        bImportsLabel.setLabelFor(bImportsField);
        org.openide.awt.Mnemonics.setLocalizedText(bImportsLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "LBL_blBeforeImports")); // NOI18N

        bImportsField.setColumns(5);

        aImports.setLabelFor(aImportsField);
        org.openide.awt.Mnemonics.setLocalizedText(aImports, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "LBL_blAfterImports")); // NOI18N

        aImportsField.setColumns(5);

        bClassLabel.setLabelFor(bClassField);
        org.openide.awt.Mnemonics.setLocalizedText(bClassLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "LBL_blBeforeClass")); // NOI18N

        bClassField.setColumns(5);

        aClassLabel.setLabelFor(aClassField);
        org.openide.awt.Mnemonics.setLocalizedText(aClassLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "LBL_blAfterClass")); // NOI18N

        aClassField.setColumns(5);

        aClassHeaderLabel.setLabelFor(aClassHeaderField);
        org.openide.awt.Mnemonics.setLocalizedText(aClassHeaderLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "LBL_blAfterClassHeader")); // NOI18N

        aClassHeaderField.setColumns(5);

        bAttributeLabel.setLabelFor(bAttributeField);
        org.openide.awt.Mnemonics.setLocalizedText(bAttributeLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "LBL_blBeforeFields")); // NOI18N

        bAttributeField.setColumns(5);

        aAttributeLabel.setLabelFor(aAttributeField);
        org.openide.awt.Mnemonics.setLocalizedText(aAttributeLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "LBL_blAfterFields")); // NOI18N

        aAttributeField.setColumns(5);

        bFunctionLabel.setLabelFor(bFunctionField);
        org.openide.awt.Mnemonics.setLocalizedText(bFunctionLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "LBL_blBeforeMethods")); // NOI18N

        bFunctionField.setColumns(5);

        aFunctionLabel.setLabelFor(aFunctionField);
        org.openide.awt.Mnemonics.setLocalizedText(aFunctionLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "LBL_blAfterMethods")); // NOI18N

        aFunctionField.setColumns(5);

        org.openide.awt.Mnemonics.setLocalizedText(bNonClassExpressionLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "FmtBlankLines.bNonClassExpressionLabel.text")); // NOI18N

        bNonClassExpressionField.setColumns(5);
        bNonClassExpressionField.setText(org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "FmtBlankLines.bNonClassExpressionField.text")); // NOI18N

        org.openide.awt.Mnemonics.setLocalizedText(aNonClassExpressionLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "FmtBlankLines.aNonClassExpressionLabel.text")); // NOI18N

        aNonClassExpressionField.setColumns(5);
        aNonClassExpressionField.setText(org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "FmtBlankLines.aNonClassExpressionField.text")); // NOI18N

        org.openide.awt.Mnemonics.setLocalizedText(bNonClassAttributeLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "FmtBlankLines.bNonClassAttributeLabel.text")); // NOI18N

        bNonClassAttributeField.setColumns(5);
        bNonClassAttributeField.setText(org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "FmtBlankLines.bNonClassAttributeField.text")); // NOI18N

        org.openide.awt.Mnemonics.setLocalizedText(aNonClassAttributeLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "FmtBlankLines.aNonClassAttributeLabel.text")); // NOI18N

        aNonClassAttributeField.setColumns(5);
        aNonClassAttributeField.setText(org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "FmtBlankLines.aNonClassAttributeField.text")); // NOI18N

        org.openide.awt.Mnemonics.setLocalizedText(bNonClassFunctionLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "FmtBlankLines.bNonClassFunctionLabel.text")); // NOI18N

        bNonClassFunctionField.setColumns(5);
        bNonClassFunctionField.setText(org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "FmtBlankLines.bNonClassFunctionField.text")); // NOI18N

        org.openide.awt.Mnemonics.setLocalizedText(aNonClassFunctionLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "FmtBlankLines.aNonClassFunctionLabel.text")); // NOI18N

        aNonClassFunctionField.setColumns(5);
        aNonClassFunctionField.setText(org.openide.util.NbBundle.getMessage(FmtBlankLines.class, "FmtBlankLines.aNonClassFunctionField.text")); // NOI18N

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(bPackageLabel)
                    .add(aPackageLabel)
                    .add(bImportsLabel)
                    .add(aImports)
                    .add(bClassLabel)
                    .add(aClassLabel)
                    .add(aClassHeaderLabel)
                    .add(bAttributeLabel)
                    .add(aAttributeLabel)
                    .add(bFunctionLabel)
                    .add(aFunctionLabel)
                    .add(bNonClassExpressionLabel)
                    .add(aNonClassExpressionLabel)
                    .add(bNonClassAttributeLabel)
                    .add(aNonClassAttributeLabel)
                    .add(bNonClassFunctionLabel)
                    .add(aNonClassFunctionLabel))
                .add(6, 6, 6)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING, false)
                    .add(aNonClassFunctionField, 0, 0, Short.MAX_VALUE)
                    .add(bNonClassFunctionField, 0, 0, Short.MAX_VALUE)
                    .add(aNonClassAttributeField, 0, 0, Short.MAX_VALUE)
                    .add(bNonClassAttributeField, 0, 0, Short.MAX_VALUE)
                    .add(aNonClassExpressionField, 0, 0, Short.MAX_VALUE)
                    .add(bNonClassExpressionField, 0, 0, Short.MAX_VALUE)
                    .add(aFunctionField)
                    .add(bFunctionField)
                    .add(aAttributeField)
                    .add(bAttributeField)
                    .add(aClassHeaderField)
                    .add(aClassField)
                    .add(bClassField)
                    .add(aImportsField)
                    .add(bImportsField)
                    .add(aPackageField)
                    .add(bPackageField)))
        );

        layout.linkSize(new java.awt.Component[] {aAttributeField, aClassField, aClassHeaderField, aFunctionField, aImportsField, aPackageField, bAttributeField, bClassField, bFunctionField, bImportsField, bPackageField}, org.jdesktop.layout.GroupLayout.HORIZONTAL);

        layout.setVerticalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(bPackageField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(bPackageLabel))
                .add(4, 4, 4)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(aPackageField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(aPackageLabel))
                .add(4, 4, 4)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(bImportsField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(bImportsLabel))
                .add(4, 4, 4)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(aImportsField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(aImports))
                .add(4, 4, 4)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(bClassField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(bClassLabel))
                .add(4, 4, 4)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(aClassField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(aClassLabel))
                .add(4, 4, 4)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(aClassHeaderField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(aClassHeaderLabel))
                .add(4, 4, 4)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(bAttributeField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(bAttributeLabel))
                .add(4, 4, 4)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(aAttributeField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(aAttributeLabel))
                .add(4, 4, 4)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(bFunctionField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(bFunctionLabel))
                .add(4, 4, 4)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(aFunctionField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(aFunctionLabel))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(bNonClassExpressionLabel)
                    .add(bNonClassExpressionField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(aNonClassExpressionLabel)
                    .add(aNonClassExpressionField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(bNonClassAttributeLabel)
                    .add(bNonClassAttributeField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(aNonClassAttributeLabel)
                    .add(aNonClassAttributeField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(bNonClassFunctionLabel)
                    .add(bNonClassFunctionField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(aNonClassFunctionLabel)
                    .add(aNonClassFunctionField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(93, Short.MAX_VALUE))
        );

        layout.linkSize(new java.awt.Component[] {aAttributeField, aClassField, aClassHeaderField, aFunctionField, aImportsField, aPackageField, bAttributeField, bClassField, bFunctionField, bImportsField, bPackageField}, org.jdesktop.layout.GroupLayout.VERTICAL);

    }// </editor-fold>//GEN-END:initComponents
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTextField aAttributeField;
    private javax.swing.JLabel aAttributeLabel;
    private javax.swing.JTextField aClassField;
    private javax.swing.JTextField aClassHeaderField;
    private javax.swing.JLabel aClassHeaderLabel;
    private javax.swing.JLabel aClassLabel;
    private javax.swing.JTextField aFunctionField;
    private javax.swing.JLabel aFunctionLabel;
    private javax.swing.JLabel aImports;
    private javax.swing.JTextField aImportsField;
    private javax.swing.JTextField aNonClassAttributeField;
    private javax.swing.JLabel aNonClassAttributeLabel;
    private javax.swing.JTextField aNonClassExpressionField;
    private javax.swing.JLabel aNonClassExpressionLabel;
    private javax.swing.JTextField aNonClassFunctionField;
    private javax.swing.JLabel aNonClassFunctionLabel;
    private javax.swing.JTextField aPackageField;
    private javax.swing.JLabel aPackageLabel;
    private javax.swing.JTextField bAttributeField;
    private javax.swing.JLabel bAttributeLabel;
    private javax.swing.JTextField bClassField;
    private javax.swing.JLabel bClassLabel;
    private javax.swing.JTextField bFunctionField;
    private javax.swing.JLabel bFunctionLabel;
    private javax.swing.JTextField bImportsField;
    private javax.swing.JLabel bImportsLabel;
    private javax.swing.JTextField bNonClassAttributeField;
    private javax.swing.JLabel bNonClassAttributeLabel;
    private javax.swing.JTextField bNonClassExpressionField;
    private javax.swing.JLabel bNonClassExpressionLabel;
    private javax.swing.JTextField bNonClassFunctionField;
    private javax.swing.JLabel bNonClassFunctionLabel;
    private javax.swing.JTextField bPackageField;
    private javax.swing.JLabel bPackageLabel;
    // End of variables declaration//GEN-END:variables
    
}
