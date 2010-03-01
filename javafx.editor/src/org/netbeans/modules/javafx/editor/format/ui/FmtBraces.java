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

import org.netbeans.modules.javafx.editor.format.CodeStyle.BracePlacement;
import static org.netbeans.modules.javafx.editor.format.ui.FmtOptions.*;
import static org.netbeans.modules.javafx.editor.format.ui.FmtOptions.CategorySupport.OPTION_ID;
import org.netbeans.modules.javafx.editor.format.CodeStyle.WrapStyle;
import org.netbeans.modules.options.editor.spi.PreferencesCustomizer;


/**
 *
 * @author  phrebejk
 */
public class FmtBraces extends javax.swing.JPanel {
    
    public FmtBraces() {
        initComponents();
        classDeclCombo.putClientProperty(OPTION_ID, classDeclBracePlacement);
        functionDeclCombo.putClientProperty(OPTION_ID, functionDeclBracePlacement);
        objectLiteralCombo.putClientProperty(OPTION_ID, objectLiteralBracePlacement);
        onReplaceCombo.putClientProperty(OPTION_ID, onReplacePlacement);
        otherCombo.putClientProperty(OPTION_ID, otherBracePlacement);
        specialElseIfCheckBox.putClientProperty(OPTION_ID, specialElseIf);
        ifBracesCombo.putClientProperty(OPTION_ID, redundantIfBraces);
        forBracesCombo.putClientProperty(OPTION_ID, redundantForBraces);
        whileBracesCombo.putClientProperty(OPTION_ID, redundantWhileBraces);
    }
    
    public static PreferencesCustomizer.Factory getController() {
        return new CategorySupport.Factory("braces", FmtBraces.class, //NOI18N
                org.openide.util.NbBundle.getMessage(FmtBraces.class, "SAMPLE_AlignBraces"), // NOI18N
                new String[]{FmtOptions.rightMargin, "30"}, //NOI18N
                new String[]{FmtOptions.wrapSequenceInit, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapAssignOps, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapBinaryOps, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapChainedFunctionCalls, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapExtendsImplementsKeyword, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapExtendsImplementsList, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapFor, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapForStatement, WrapStyle.WRAP_ALWAYS.name()},
                new String[]{FmtOptions.wrapIfExpression, WrapStyle.WRAP_ALWAYS.name()},
                new String[]{FmtOptions.wrapFunctionCallArgs, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapFunctionParams, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapThrowsKeyword, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapThrowsList, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapWhileStatement, WrapStyle.WRAP_ALWAYS.name()},
                new String[]{FmtOptions.alignMultilineAssignment, Boolean.FALSE.toString()},
                new String[]{FmtOptions.alignMultilineBinaryOp, Boolean.FALSE.toString()},
                new String[]{FmtOptions.alignMultilineCallArgs, Boolean.FALSE.toString()},
                new String[]{FmtOptions.alignMultilineImplements, Boolean.FALSE.toString()},
                new String[]{FmtOptions.alignMultilineMethodParams, Boolean.FALSE.toString()},
                new String[]{FmtOptions.alignMultilineThrows, Boolean.FALSE.toString()},
                new String[]{FmtOptions.functionDeclBracePlacement, BracePlacement.SAME_LINE.name()},
                new String[]{FmtOptions.spaceBeforeFunctionDeclLeftBrace, Boolean.TRUE.toString()});
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        bracesPlacementLabel = new javax.swing.JLabel();
        classDeclLabel = new javax.swing.JLabel();
        classDeclCombo = new javax.swing.JComboBox();
        functionDeclLabel = new javax.swing.JLabel();
        functionDeclCombo = new javax.swing.JComboBox();
        objectLiteralLabel = new javax.swing.JLabel();
        objectLiteralCombo = new javax.swing.JComboBox();
        onReplaceLabel = new javax.swing.JLabel();
        onReplaceCombo = new javax.swing.JComboBox();
        otherLabel = new javax.swing.JLabel();
        otherCombo = new javax.swing.JComboBox();
        specialElseIfCheckBox = new javax.swing.JCheckBox();
        bracesGenerationLabel = new javax.swing.JLabel();
        ifBracesLabel = new javax.swing.JLabel();
        ifBracesCombo = new javax.swing.JComboBox();
        forBracesLabel = new javax.swing.JLabel();
        forBracesCombo = new javax.swing.JComboBox();
        whileBracesLabel = new javax.swing.JLabel();
        whileBracesCombo = new javax.swing.JComboBox();
        jSeparator1 = new javax.swing.JSeparator();
        jSeparator2 = new javax.swing.JSeparator();

        setName(org.openide.util.NbBundle.getMessage(FmtBraces.class, "LBL_Braces")); // NOI18N
        setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(bracesPlacementLabel, org.openide.util.NbBundle.getMessage(FmtBraces.class, "LBL_br_bracesPlacement")); // NOI18N

        classDeclLabel.setLabelFor(classDeclCombo);
        org.openide.awt.Mnemonics.setLocalizedText(classDeclLabel, org.openide.util.NbBundle.getMessage(FmtBraces.class, "LBL_bp_ClassDecl")); // NOI18N

        classDeclCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        functionDeclLabel.setLabelFor(functionDeclCombo);
        org.openide.awt.Mnemonics.setLocalizedText(functionDeclLabel, org.openide.util.NbBundle.getMessage(FmtBraces.class, "LBL_bp_MethodDecl")); // NOI18N

        functionDeclCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        functionDeclCombo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                functionDeclComboActionPerformed(evt);
            }
        });

        org.openide.awt.Mnemonics.setLocalizedText(objectLiteralLabel, org.openide.util.NbBundle.getMessage(FmtBraces.class, "FmtBraces.objectLiteralLabel.text")); // NOI18N

        objectLiteralCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        org.openide.awt.Mnemonics.setLocalizedText(onReplaceLabel, org.openide.util.NbBundle.getMessage(FmtBraces.class, "FmtBraces.onReplaceLabel.text")); // NOI18N

        onReplaceCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        otherLabel.setLabelFor(otherCombo);
        org.openide.awt.Mnemonics.setLocalizedText(otherLabel, org.openide.util.NbBundle.getMessage(FmtBraces.class, "LBL_bp_Other")); // NOI18N

        otherCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        org.openide.awt.Mnemonics.setLocalizedText(specialElseIfCheckBox, org.openide.util.NbBundle.getMessage(FmtBraces.class, "LBL_bp_SpecialElseIf")); // NOI18N
        specialElseIfCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        specialElseIfCheckBox.setMargin(new java.awt.Insets(0, 0, 0, 0));
        specialElseIfCheckBox.setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(bracesGenerationLabel, org.openide.util.NbBundle.getMessage(FmtBraces.class, "LBL_br_bracesGeneration")); // NOI18N

        ifBracesLabel.setLabelFor(ifBracesCombo);
        org.openide.awt.Mnemonics.setLocalizedText(ifBracesLabel, org.openide.util.NbBundle.getMessage(FmtBraces.class, "LBL_bg_If")); // NOI18N

        ifBracesCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        forBracesLabel.setLabelFor(forBracesCombo);
        org.openide.awt.Mnemonics.setLocalizedText(forBracesLabel, org.openide.util.NbBundle.getMessage(FmtBraces.class, "LBL_bg_For")); // NOI18N

        forBracesCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        whileBracesLabel.setLabelFor(whileBracesCombo);
        org.openide.awt.Mnemonics.setLocalizedText(whileBracesLabel, org.openide.util.NbBundle.getMessage(FmtBraces.class, "LBL_bg_While")); // NOI18N

        whileBracesCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                        .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                            .addComponent(bracesPlacementLabel)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jSeparator1, javax.swing.GroupLayout.DEFAULT_SIZE, 113, Short.MAX_VALUE))
                        .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                            .addContainerGap()
                            .addComponent(classDeclLabel))
                        .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                            .addContainerGap()
                            .addComponent(ifBracesLabel))
                        .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                            .addContainerGap()
                            .addComponent(forBracesLabel))
                        .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                            .addContainerGap()
                            .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                .addGroup(layout.createSequentialGroup()
                                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(functionDeclLabel)
                                        .addComponent(whileBracesLabel)
                                        .addComponent(objectLiteralLabel)
                                        .addComponent(onReplaceLabel))
                                    .addGap(12, 12, 12)
                                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                            .addComponent(classDeclCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                            .addComponent(ifBracesCombo, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                            .addComponent(forBracesCombo, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                            .addComponent(whileBracesCombo, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                        .addComponent(functionDeclCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(otherCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(objectLiteralCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(onReplaceCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 21, Short.MAX_VALUE))
                                .addGroup(layout.createSequentialGroup()
                                    .addComponent(bracesGenerationLabel)
                                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jSeparator2, javax.swing.GroupLayout.DEFAULT_SIZE, 99, Short.MAX_VALUE)))))
                    .addGroup(layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(specialElseIfCheckBox))
                    .addGroup(layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(otherLabel)))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {classDeclCombo, forBracesCombo, functionDeclCombo, ifBracesCombo, otherCombo, whileBracesCombo});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(bracesPlacementLabel)
                    .addComponent(jSeparator1, javax.swing.GroupLayout.PREFERRED_SIZE, 10, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(classDeclLabel, javax.swing.GroupLayout.PREFERRED_SIZE, 18, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(classDeclCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(6, 6, 6)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(functionDeclLabel)
                    .addComponent(functionDeclCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(7, 7, 7)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(objectLiteralLabel)
                    .addComponent(objectLiteralCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(onReplaceCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(onReplaceLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(otherLabel)
                    .addComponent(otherCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(specialElseIfCheckBox)
                .addGap(31, 31, 31)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(bracesGenerationLabel)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED))
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                        .addComponent(jSeparator2, javax.swing.GroupLayout.PREFERRED_SIZE, 2, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(23, 23, 23)))
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(ifBracesLabel)
                    .addComponent(ifBracesCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(forBracesLabel, javax.swing.GroupLayout.PREFERRED_SIZE, 16, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(forBracesCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(whileBracesLabel)
                    .addComponent(whileBracesCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(23, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    private void functionDeclComboActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_functionDeclComboActionPerformed
    // TODO add your handling code here:
}//GEN-LAST:event_functionDeclComboActionPerformed
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel bracesGenerationLabel;
    private javax.swing.JLabel bracesPlacementLabel;
    private javax.swing.JComboBox classDeclCombo;
    private javax.swing.JLabel classDeclLabel;
    private javax.swing.JComboBox forBracesCombo;
    private javax.swing.JLabel forBracesLabel;
    private javax.swing.JComboBox functionDeclCombo;
    private javax.swing.JLabel functionDeclLabel;
    private javax.swing.JComboBox ifBracesCombo;
    private javax.swing.JLabel ifBracesLabel;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JComboBox objectLiteralCombo;
    private javax.swing.JLabel objectLiteralLabel;
    private javax.swing.JComboBox onReplaceCombo;
    private javax.swing.JLabel onReplaceLabel;
    private javax.swing.JComboBox otherCombo;
    private javax.swing.JLabel otherLabel;
    private javax.swing.JCheckBox specialElseIfCheckBox;
    private javax.swing.JComboBox whileBracesCombo;
    private javax.swing.JLabel whileBracesLabel;
    // End of variables declaration//GEN-END:variables
    
}
