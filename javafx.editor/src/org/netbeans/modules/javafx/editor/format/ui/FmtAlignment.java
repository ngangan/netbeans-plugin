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
import org.netbeans.modules.javafx.editor.format.CodeStyle.WrapStyle;
import static org.netbeans.modules.javafx.editor.format.ui.FmtOptions.*;
import static org.netbeans.modules.javafx.editor.format.ui.FmtOptions.CategorySupport.OPTION_ID;
import org.netbeans.modules.options.editor.spi.PreferencesCustomizer;


/**
 *
 * @author  phrebejk
 */
public class FmtAlignment extends javax.swing.JPanel {
    
    public FmtAlignment() {
        initComponents();
        nlElseCheckBox.putClientProperty(OPTION_ID, placeElseOnNewLine);
        nlCatchCheckBox.putClientProperty(OPTION_ID, placeCatchOnNewLine);
        nlFinallyCheckBox.putClientProperty(OPTION_ID, placeFinallyOnNewLine);
        nlModifiersCheckBox.putClientProperty(OPTION_ID, placeNewLineAfterModifiers);
        amFunctionParamsCheckBox.putClientProperty(OPTION_ID, alignMultilineMethodParams);
        amCallArgsCheckBox.putClientProperty(OPTION_ID, alignMultilineCallArgs);
        amAssignCheckBox1.putClientProperty(OPTION_ID, alignMultilineAssignment);
        amBinaryOpCheckBox1.putClientProperty(OPTION_ID, alignMultilineBinaryOp);
        amForCheckBox1.putClientProperty(OPTION_ID, alignMultilineFor);
        amExtendsCheckBox1.putClientProperty(OPTION_ID, alignMultilineImplements);
        amParenthesizedCheckBox1.putClientProperty(OPTION_ID, alignMultilineParenthesized);
    }
    
    public static PreferencesCustomizer.Factory getController() {
        return new CategorySupport.Factory("alignment", FmtAlignment.class, //NOI18N
                org.openide.util.NbBundle.getMessage(FmtAlignment.class, "SAMPLE_AlignBraces"), // NOI18N
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
                new String[]{FmtOptions.alignMultilineFor, Boolean.FALSE.toString()},
                new String[]{FmtOptions.alignMultilineImplements, Boolean.FALSE.toString()},
                new String[]{FmtOptions.alignMultilineMethodParams, Boolean.FALSE.toString()},
                new String[]{FmtOptions.alignMultilineParenthesized, Boolean.FALSE.toString()},
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

        newLinesLabel = new javax.swing.JLabel();
        nlElseCheckBox = new javax.swing.JCheckBox();
        nlCatchCheckBox = new javax.swing.JCheckBox();
        nlFinallyCheckBox = new javax.swing.JCheckBox();
        nlModifiersCheckBox = new javax.swing.JCheckBox();
        multilineAlignmentLabel = new javax.swing.JLabel();
        amFunctionParamsCheckBox = new javax.swing.JCheckBox();
        amCallArgsCheckBox = new javax.swing.JCheckBox();
        amExtendsCheckBox1 = new javax.swing.JCheckBox();
        amBinaryOpCheckBox1 = new javax.swing.JCheckBox();
        amAssignCheckBox1 = new javax.swing.JCheckBox();
        amForCheckBox1 = new javax.swing.JCheckBox();
        amParenthesizedCheckBox1 = new javax.swing.JCheckBox();
        jSeparator1 = new javax.swing.JSeparator();
        jSeparator2 = new javax.swing.JSeparator();

        setName(org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_Alignment")); // NOI18N
        setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(newLinesLabel, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_al_newLines")); // NOI18N

        org.openide.awt.Mnemonics.setLocalizedText(nlElseCheckBox, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_nl_Else")); // NOI18N
        nlElseCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        nlElseCheckBox.setMargin(new java.awt.Insets(0, 0, 0, 0));
        nlElseCheckBox.setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(nlCatchCheckBox, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_nl_Catch")); // NOI18N
        nlCatchCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        nlCatchCheckBox.setMargin(new java.awt.Insets(0, 0, 0, 0));
        nlCatchCheckBox.setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(nlFinallyCheckBox, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_nl_Finally")); // NOI18N
        nlFinallyCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        nlFinallyCheckBox.setMargin(new java.awt.Insets(0, 0, 0, 0));
        nlFinallyCheckBox.setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(nlModifiersCheckBox, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_nl_Modifiers")); // NOI18N
        nlModifiersCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        nlModifiersCheckBox.setMargin(new java.awt.Insets(0, 0, 0, 0));
        nlModifiersCheckBox.setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(multilineAlignmentLabel, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_al_multilineAlignment")); // NOI18N

        org.openide.awt.Mnemonics.setLocalizedText(amFunctionParamsCheckBox, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_am_MethodParams")); // NOI18N
        amFunctionParamsCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        amFunctionParamsCheckBox.setMargin(new java.awt.Insets(0, 0, 0, 0));
        amFunctionParamsCheckBox.setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(amCallArgsCheckBox, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_am_CallArgs")); // NOI18N
        amCallArgsCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        amCallArgsCheckBox.setMargin(new java.awt.Insets(0, 0, 0, 0));
        amCallArgsCheckBox.setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(amExtendsCheckBox1, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_an_Implements")); // NOI18N
        amExtendsCheckBox1.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        amExtendsCheckBox1.setMargin(new java.awt.Insets(0, 0, 0, 0));
        amExtendsCheckBox1.setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(amBinaryOpCheckBox1, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_am_BinaryOp")); // NOI18N
        amBinaryOpCheckBox1.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        amBinaryOpCheckBox1.setMargin(new java.awt.Insets(0, 0, 0, 0));
        amBinaryOpCheckBox1.setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(amAssignCheckBox1, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_am_Assign")); // NOI18N
        amAssignCheckBox1.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        amAssignCheckBox1.setMargin(new java.awt.Insets(0, 0, 0, 0));
        amAssignCheckBox1.setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(amForCheckBox1, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_am_For")); // NOI18N
        amForCheckBox1.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        amForCheckBox1.setMargin(new java.awt.Insets(0, 0, 0, 0));
        amForCheckBox1.setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(amParenthesizedCheckBox1, org.openide.util.NbBundle.getMessage(FmtAlignment.class, "LBL_am_Paren")); // NOI18N
        amParenthesizedCheckBox1.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        amParenthesizedCheckBox1.setMargin(new java.awt.Insets(0, 0, 0, 0));
        amParenthesizedCheckBox1.setOpaque(false);

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.TRAILING, false)
                        .add(org.jdesktop.layout.GroupLayout.LEADING, layout.createSequentialGroup()
                            .add(newLinesLabel)
                            .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                            .add(jSeparator1))
                        .add(org.jdesktop.layout.GroupLayout.LEADING, layout.createSequentialGroup()
                            .addContainerGap()
                            .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                                .add(nlElseCheckBox)
                                .add(nlCatchCheckBox))
                            .add(80, 80, 80)
                            .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                                .add(nlModifiersCheckBox)
                                .add(nlFinallyCheckBox))
                            .add(46, 46, 46)))
                    .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.TRAILING, false)
                        .add(org.jdesktop.layout.GroupLayout.LEADING, layout.createSequentialGroup()
                            .add(multilineAlignmentLabel)
                            .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                            .add(jSeparator2))
                        .add(org.jdesktop.layout.GroupLayout.LEADING, layout.createSequentialGroup()
                            .addContainerGap()
                            .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                                .add(amFunctionParamsCheckBox)
                                .add(amBinaryOpCheckBox1)
                                .add(amAssignCheckBox1)
                                .add(amParenthesizedCheckBox1))
                            .add(14, 14, 14)
                            .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                                .add(amForCheckBox1)
                                .add(amCallArgsCheckBox)
                                .add(amExtendsCheckBox1)))))
                .addContainerGap(org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(layout.createSequentialGroup()
                        .addContainerGap()
                        .add(newLinesLabel))
                    .add(layout.createSequentialGroup()
                        .add(17, 17, 17)
                        .add(jSeparator1, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 10, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(nlElseCheckBox)
                    .add(nlFinallyCheckBox))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(nlModifiersCheckBox)
                    .add(nlCatchCheckBox))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(multilineAlignmentLabel)
                    .add(layout.createSequentialGroup()
                        .add(5, 5, 5)
                        .add(jSeparator2, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 10, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(amFunctionParamsCheckBox)
                    .add(amCallArgsCheckBox))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(amExtendsCheckBox1)
                    .add(amBinaryOpCheckBox1))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(amAssignCheckBox1)
                    .add(amForCheckBox1))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(amParenthesizedCheckBox1)
                .addContainerGap(86, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox amAssignCheckBox1;
    private javax.swing.JCheckBox amBinaryOpCheckBox1;
    private javax.swing.JCheckBox amCallArgsCheckBox;
    private javax.swing.JCheckBox amExtendsCheckBox1;
    private javax.swing.JCheckBox amForCheckBox1;
    private javax.swing.JCheckBox amFunctionParamsCheckBox;
    private javax.swing.JCheckBox amParenthesizedCheckBox1;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JLabel multilineAlignmentLabel;
    private javax.swing.JLabel newLinesLabel;
    private javax.swing.JCheckBox nlCatchCheckBox;
    private javax.swing.JCheckBox nlElseCheckBox;
    private javax.swing.JCheckBox nlFinallyCheckBox;
    private javax.swing.JCheckBox nlModifiersCheckBox;
    // End of variables declaration//GEN-END:variables
    
}
