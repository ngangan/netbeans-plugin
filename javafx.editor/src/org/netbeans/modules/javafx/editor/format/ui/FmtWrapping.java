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

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import org.netbeans.api.java.source.CodeStyle;
import org.netbeans.modules.javafx.editor.format.CodeStyle.BracePlacement;
import org.netbeans.modules.javafx.editor.format.CodeStyle.WrapStyle;
import static org.netbeans.modules.javafx.editor.format.ui.FmtOptions.*;
import static org.netbeans.modules.javafx.editor.format.ui.FmtOptions.CategorySupport.OPTION_ID;
import org.netbeans.modules.options.editor.spi.PreferencesCustomizer;


/**
 *
 * @author  phrebejk
 */
public class FmtWrapping extends javax.swing.JPanel implements FocusListener {
    
    public FmtWrapping() {
        initComponents();
        
        scrollPane.getViewport().setBackground(java.awt.SystemColor.controlLtHighlight);
        
        extendsImplementsKeywordCombo.putClientProperty(OPTION_ID, wrapExtendsImplementsKeyword);
        extendsImplementsKeywordCombo.addFocusListener(this);
        extendsImplementsListCombo.putClientProperty(OPTION_ID, wrapExtendsImplementsList);
        extendsImplementsListCombo.addFocusListener(this);
        methodParamsCombo.putClientProperty(OPTION_ID, wrapFunctionParams);
        methodParamsCombo.addFocusListener(this);
        methodCallArgsCombo.putClientProperty(OPTION_ID, wrapFunctionCallArgs);
        methodCallArgsCombo.addFocusListener(this);
        chainedMethodCallsCombo.putClientProperty(OPTION_ID, wrapChainedFunctionCalls);
        chainedMethodCallsCombo.addFocusListener(this);
        throwsKeywordCombo.putClientProperty(OPTION_ID, wrapThrowsKeyword);
        throwsKeywordCombo.addFocusListener(this);
        throwsListCombo.putClientProperty(OPTION_ID, wrapThrowsList);
        throwsListCombo.addFocusListener(this);
        sequenceInitCombo.putClientProperty(OPTION_ID, wrapSequenceInit);
        sequenceInitCombo.addFocusListener(this);
        forCombo.putClientProperty(OPTION_ID, wrapFor);
        forCombo.addFocusListener(this);
        forStatementCombo.putClientProperty(OPTION_ID, wrapForStatement );
        forStatementCombo.addFocusListener(this);
        ifStatementCombo.putClientProperty(OPTION_ID, wrapIfExpression);
        ifStatementCombo.addFocusListener(this);
        whileStatementComboBox.putClientProperty(OPTION_ID, wrapWhileStatement);
        whileStatementComboBox.addFocusListener(this);
        binaryOpsCombo.putClientProperty(OPTION_ID, wrapBinaryOps);
        binaryOpsCombo.addFocusListener(this);
        assignOpsCombo.putClientProperty(OPTION_ID, wrapAssignOps);
        assignOpsCombo.addFocusListener(this);
    }
    
    public static PreferencesCustomizer.Factory getController() {
        return new CategorySupport.Factory("wrapping", FmtWrapping.class, //NOI18N
                org.openide.util.NbBundle.getMessage(FmtWrapping.class, "SAMPLE_Wrapping"), //NOI18N
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
                new String[]{FmtOptions.spaceBeforeFunctionDeclLeftBrace, Boolean.TRUE.toString()},
                new String[]{FmtOptions.redundantForBraces, CodeStyle.BracesGenerationStyle.GENERATE.name()},
                new String[]{FmtOptions.redundantIfBraces, CodeStyle.BracesGenerationStyle.GENERATE.name()},
                new String[]{FmtOptions.redundantWhileBraces, CodeStyle.BracesGenerationStyle.GENERATE.name()});
    }

    public void focusGained(FocusEvent e) {
        scrollPane.getViewport().scrollRectToVisible(e.getComponent().getBounds());
    }

    public void focusLost(FocusEvent e) {
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        scrollPane = new javax.swing.JScrollPane();
        panel1 = new javax.swing.JPanel();
        extendsImplemetsKeywordLabel = new javax.swing.JLabel();
        extendsImplementsKeywordCombo = new javax.swing.JComboBox();
        extendsImplementsListLabel = new javax.swing.JLabel();
        extendsImplementsListCombo = new javax.swing.JComboBox();
        methodParamsLabel = new javax.swing.JLabel();
        methodParamsCombo = new javax.swing.JComboBox();
        methodCallArgsLabel = new javax.swing.JLabel();
        methodCallArgsCombo = new javax.swing.JComboBox();
        chainedMethodCallsLabel = new javax.swing.JLabel();
        chainedMethodCallsCombo = new javax.swing.JComboBox();
        throwsKeywordLabel = new javax.swing.JLabel();
        throwsKeywordCombo = new javax.swing.JComboBox();
        throwsListLabel = new javax.swing.JLabel();
        throwsListCombo = new javax.swing.JComboBox();
        arrayInitLabel = new javax.swing.JLabel();
        sequenceInitCombo = new javax.swing.JComboBox();
        forLabel = new javax.swing.JLabel();
        forCombo = new javax.swing.JComboBox();
        forStatementLabel = new javax.swing.JLabel();
        forStatementCombo = new javax.swing.JComboBox();
        ifStatementLabel = new javax.swing.JLabel();
        ifStatementCombo = new javax.swing.JComboBox();
        whileStatementLabel = new javax.swing.JLabel();
        whileStatementComboBox = new javax.swing.JComboBox();
        binaryOpsLabel = new javax.swing.JLabel();
        binaryOpsCombo = new javax.swing.JComboBox();
        assignOpsLabel = new javax.swing.JLabel();
        assignOpsCombo = new javax.swing.JComboBox();
        spacerPanel1 = new javax.swing.JPanel();

        setName(org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_Wrapping")); // NOI18N
        setOpaque(false);
        setLayout(new java.awt.BorderLayout());

        scrollPane.setBackground(java.awt.SystemColor.controlLtHighlight);
        scrollPane.setMinimumSize(new java.awt.Dimension(300, 200));
        scrollPane.setPreferredSize(new java.awt.Dimension(350, 600));

        panel1.setOpaque(false);
        panel1.setLayout(new java.awt.GridBagLayout());

        extendsImplemetsKeywordLabel.setLabelFor(extendsImplementsKeywordCombo);
        org.openide.awt.Mnemonics.setLocalizedText(extendsImplemetsKeywordLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_extendsImplementsKeyword")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 4, 0);
        panel1.add(extendsImplemetsKeywordLabel, gridBagConstraints);

        extendsImplementsKeywordCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 6, 4, 8);
        panel1.add(extendsImplementsKeywordCombo, gridBagConstraints);

        extendsImplementsListLabel.setLabelFor(extendsImplementsListCombo);
        org.openide.awt.Mnemonics.setLocalizedText(extendsImplementsListLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_extendsImplementsList")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(extendsImplementsListLabel, gridBagConstraints);

        extendsImplementsListCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(extendsImplementsListCombo, gridBagConstraints);

        methodParamsLabel.setLabelFor(methodParamsCombo);
        org.openide.awt.Mnemonics.setLocalizedText(methodParamsLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_methodParameters")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(methodParamsLabel, gridBagConstraints);

        methodParamsCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(methodParamsCombo, gridBagConstraints);

        methodCallArgsLabel.setLabelFor(methodCallArgsCombo);
        org.openide.awt.Mnemonics.setLocalizedText(methodCallArgsLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_methodCallArgs")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(methodCallArgsLabel, gridBagConstraints);

        methodCallArgsCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(methodCallArgsCombo, gridBagConstraints);

        chainedMethodCallsLabel.setLabelFor(chainedMethodCallsCombo);
        org.openide.awt.Mnemonics.setLocalizedText(chainedMethodCallsLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_chainedFunctionCalls")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(chainedMethodCallsLabel, gridBagConstraints);

        chainedMethodCallsCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(chainedMethodCallsCombo, gridBagConstraints);

        throwsKeywordLabel.setLabelFor(throwsKeywordCombo);
        org.openide.awt.Mnemonics.setLocalizedText(throwsKeywordLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_throwsKeyword")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(throwsKeywordLabel, gridBagConstraints);

        throwsKeywordCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(throwsKeywordCombo, gridBagConstraints);

        throwsListLabel.setLabelFor(throwsListCombo);
        org.openide.awt.Mnemonics.setLocalizedText(throwsListLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_throwsList")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(throwsListLabel, gridBagConstraints);

        throwsListCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(throwsListCombo, gridBagConstraints);

        arrayInitLabel.setLabelFor(sequenceInitCombo);
        org.openide.awt.Mnemonics.setLocalizedText(arrayInitLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_arrayInit")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(arrayInitLabel, gridBagConstraints);

        sequenceInitCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(sequenceInitCombo, gridBagConstraints);

        forLabel.setLabelFor(forCombo);
        org.openide.awt.Mnemonics.setLocalizedText(forLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_for")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(forLabel, gridBagConstraints);

        forCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(forCombo, gridBagConstraints);

        forStatementLabel.setLabelFor(forStatementCombo);
        org.openide.awt.Mnemonics.setLocalizedText(forStatementLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_forStatement")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(forStatementLabel, gridBagConstraints);

        forStatementCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(forStatementCombo, gridBagConstraints);

        ifStatementLabel.setLabelFor(ifStatementCombo);
        org.openide.awt.Mnemonics.setLocalizedText(ifStatementLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_ifStatement")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(ifStatementLabel, gridBagConstraints);

        ifStatementCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(ifStatementCombo, gridBagConstraints);

        whileStatementLabel.setLabelFor(whileStatementComboBox);
        org.openide.awt.Mnemonics.setLocalizedText(whileStatementLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_whileStatement")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(whileStatementLabel, gridBagConstraints);

        whileStatementComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(whileStatementComboBox, gridBagConstraints);

        binaryOpsLabel.setLabelFor(binaryOpsCombo);
        org.openide.awt.Mnemonics.setLocalizedText(binaryOpsLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_binaryOps")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(binaryOpsLabel, gridBagConstraints);

        binaryOpsCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(binaryOpsCombo, gridBagConstraints);

        assignOpsLabel.setLabelFor(assignOpsCombo);
        org.openide.awt.Mnemonics.setLocalizedText(assignOpsLabel, org.openide.util.NbBundle.getMessage(FmtWrapping.class, "LBL_wrp_assignOps")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 4, 0);
        panel1.add(assignOpsLabel, gridBagConstraints);

        assignOpsCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 4, 8);
        panel1.add(assignOpsCombo, gridBagConstraints);

        spacerPanel1.setOpaque(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.gridheight = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 8);
        panel1.add(spacerPanel1, gridBagConstraints);

        scrollPane.setViewportView(panel1);

        add(scrollPane, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel arrayInitLabel;
    private javax.swing.JComboBox assignOpsCombo;
    private javax.swing.JLabel assignOpsLabel;
    private javax.swing.JComboBox binaryOpsCombo;
    private javax.swing.JLabel binaryOpsLabel;
    private javax.swing.JComboBox chainedMethodCallsCombo;
    private javax.swing.JLabel chainedMethodCallsLabel;
    private javax.swing.JComboBox extendsImplementsKeywordCombo;
    private javax.swing.JComboBox extendsImplementsListCombo;
    private javax.swing.JLabel extendsImplementsListLabel;
    private javax.swing.JLabel extendsImplemetsKeywordLabel;
    private javax.swing.JComboBox forCombo;
    private javax.swing.JLabel forLabel;
    private javax.swing.JComboBox forStatementCombo;
    private javax.swing.JLabel forStatementLabel;
    private javax.swing.JComboBox ifStatementCombo;
    private javax.swing.JLabel ifStatementLabel;
    private javax.swing.JComboBox methodCallArgsCombo;
    private javax.swing.JLabel methodCallArgsLabel;
    private javax.swing.JComboBox methodParamsCombo;
    private javax.swing.JLabel methodParamsLabel;
    private javax.swing.JPanel panel1;
    private javax.swing.JScrollPane scrollPane;
    private javax.swing.JComboBox sequenceInitCombo;
    private javax.swing.JPanel spacerPanel1;
    private javax.swing.JComboBox throwsKeywordCombo;
    private javax.swing.JLabel throwsKeywordLabel;
    private javax.swing.JComboBox throwsListCombo;
    private javax.swing.JLabel throwsListLabel;
    private javax.swing.JComboBox whileStatementComboBox;
    private javax.swing.JLabel whileStatementLabel;
    // End of variables declaration//GEN-END:variables

}
