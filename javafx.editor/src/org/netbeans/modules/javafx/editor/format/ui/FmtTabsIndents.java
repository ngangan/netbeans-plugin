/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
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
import org.netbeans.modules.javafx.editor.format.CodeStyle.WrapStyle;
import org.netbeans.modules.options.editor.spi.PreferencesCustomizer;

/**
 *
 * @author  phrebejk
 */
public class FmtTabsIndents extends javax.swing.JPanel {
   
    /** Creates new form FmtTabsIndents */
    public FmtTabsIndents() {
        initComponents();
        
//        expandTabCheckBox.putClientProperty(OPTION_ID, expandTabToSpaces);
//        tabSizeField.putClientProperty(OPTION_ID, tabSize);
//        indentSizeField.putClientProperty(OPTION_ID, new String [] { indentSize, spacesPerTab });
        continuationIndentSizeField.putClientProperty(OPTION_ID, continuationIndentSize);
        labelIndentField.putClientProperty(OPTION_ID, labelIndent);
        absoluteLabelIndentCheckBox.putClientProperty(OPTION_ID, absoluteLabelIndent);
        indentTopLevelClassMembersCheckBox.putClientProperty(OPTION_ID, indentTopLevelClassMembers);
        indentCasesFromSwitchCheckBox.putClientProperty(OPTION_ID, indentCasesFromSwitch);
        addLeadingStarInCommentCheckBox.putClientProperty(OPTION_ID, addLeadingStarInComment);
//        rightMarginField.putClientProperty(OPTION_ID, rightMargin);
    }
    
    public static PreferencesCustomizer.Factory getController() {
        return new CategorySupport.Factory(PreferencesCustomizer.TABS_AND_INDENTS_ID, FmtTabsIndents.class, //NOI18N
                org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "SAMPLE_TabsIndents"), // NOI18N
                new String[] { FmtOptions.rightMargin, "30" }, //NOI18N
                new String[] { FmtOptions.wrapAnnotations, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapArrayInit, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapAssert, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapAssignOps, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapBinaryOps, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapChainedFunctionCalls, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapDoWhileStatement, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapEnumConstants, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapExtendsImplementsKeyword, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapExtendsImplementsList, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapFor, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapForStatement, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapIfStatement, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapFunctionCallArgs, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapFunctionParams, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapTernaryOps, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapThrowsKeyword, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapThrowsList, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.wrapWhileStatement, WrapStyle.WRAP_ALWAYS.name() },
                new String[] { FmtOptions.alignMultilineArrayInit, Boolean.FALSE.toString() },
                new String[] { FmtOptions.alignMultilineAssignment, Boolean.FALSE.toString() },
                new String[] { FmtOptions.alignMultilineBinaryOp, Boolean.FALSE.toString() },
                new String[] { FmtOptions.alignMultilineCallArgs, Boolean.FALSE.toString() },
                new String[] { FmtOptions.alignMultilineFor, Boolean.FALSE.toString() },
                new String[] { FmtOptions.alignMultilineImplements, Boolean.FALSE.toString() },
                new String[] { FmtOptions.alignMultilineMethodParams, Boolean.FALSE.toString() },
                new String[] { FmtOptions.alignMultilineParenthesized, Boolean.FALSE.toString() },
                new String[] { FmtOptions.alignMultilineTernaryOp, Boolean.FALSE.toString() },
                new String[] { FmtOptions.alignMultilineThrows, Boolean.FALSE.toString() }
                );
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        continuationIndentSizeLabel = new javax.swing.JLabel();
        continuationIndentSizeField = new javax.swing.JTextField();
        labelIndentLabel = new javax.swing.JLabel();
        labelIndentField = new javax.swing.JTextField();
        absoluteLabelIndentCheckBox = new javax.swing.JCheckBox();
        indentTopLevelClassMembersCheckBox = new javax.swing.JCheckBox();
        indentCasesFromSwitchCheckBox = new javax.swing.JCheckBox();
        addLeadingStarInCommentCheckBox = new javax.swing.JCheckBox();

        setName(org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "LBL_TabsAndIndents")); // NOI18N
        setOpaque(false);

        continuationIndentSizeLabel.setLabelFor(continuationIndentSizeField);
        org.openide.awt.Mnemonics.setLocalizedText(continuationIndentSizeLabel, org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "LBL_ContinuationIndentSize")); // NOI18N

        labelIndentLabel.setLabelFor(labelIndentField);
        org.openide.awt.Mnemonics.setLocalizedText(labelIndentLabel, org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "LBL_LabelIndent")); // NOI18N

        org.openide.awt.Mnemonics.setLocalizedText(absoluteLabelIndentCheckBox, org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "LBL_AbsoluteLabelIndent")); // NOI18N
        absoluteLabelIndentCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));

        org.openide.awt.Mnemonics.setLocalizedText(indentTopLevelClassMembersCheckBox, org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "LBL_IndentTopLevelClassMemberts")); // NOI18N
        indentTopLevelClassMembersCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));

        org.openide.awt.Mnemonics.setLocalizedText(indentCasesFromSwitchCheckBox, org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "LBL_IndentCasesFromSwitch")); // NOI18N
        indentCasesFromSwitchCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));

        org.openide.awt.Mnemonics.setLocalizedText(addLeadingStarInCommentCheckBox, org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "LBL_AddLeadingStarInComment")); // NOI18N
        addLeadingStarInCommentCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(continuationIndentSizeLabel, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 200, Short.MAX_VALUE)
                    .add(labelIndentLabel))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.TRAILING)
                    .add(continuationIndentSizeField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 36, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                    .add(labelIndentField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 36, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
            .add(indentCasesFromSwitchCheckBox)
            .add(indentTopLevelClassMembersCheckBox)
            .add(absoluteLabelIndentCheckBox)
            .add(layout.createSequentialGroup()
                .add(addLeadingStarInCommentCheckBox)
                .addContainerGap())
        );

        layout.linkSize(new java.awt.Component[] {continuationIndentSizeField, labelIndentField}, org.jdesktop.layout.GroupLayout.HORIZONTAL);

        layout.setVerticalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(layout.createSequentialGroup()
                .addContainerGap()
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(continuationIndentSizeLabel)
                    .add(continuationIndentSizeField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.BASELINE)
                    .add(labelIndentLabel)
                    .add(labelIndentField, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.UNRELATED)
                .add(absoluteLabelIndentCheckBox)
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(indentTopLevelClassMembersCheckBox)
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(indentCasesFromSwitchCheckBox)
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(addLeadingStarInCommentCheckBox)
                .addContainerGap(org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        continuationIndentSizeField.getAccessibleContext().setAccessibleDescription(org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "FmtTabsIndents.continuationIndentSizeField.AccessibleContext.accessibleDescription")); // NOI18N
        labelIndentField.getAccessibleContext().setAccessibleDescription(org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "FmtTabsIndents.labelIndentField.AccessibleContext.accessibleDescription")); // NOI18N
        absoluteLabelIndentCheckBox.getAccessibleContext().setAccessibleDescription(org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "FmtTabsIndents.absoluteLabelIndentCheckBox.AccessibleContext.accessibleDescription")); // NOI18N
        indentTopLevelClassMembersCheckBox.getAccessibleContext().setAccessibleDescription(org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "FmtTabsIndents.indentTopLevelClassMembersCheckBox.AccessibleContext.accessibleDescription")); // NOI18N
        indentCasesFromSwitchCheckBox.getAccessibleContext().setAccessibleDescription(org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "FmtTabsIndents.indentCasesFromSwitchCheckBox.AccessibleContext.accessibleDescription")); // NOI18N
        addLeadingStarInCommentCheckBox.getAccessibleContext().setAccessibleDescription(org.openide.util.NbBundle.getMessage(FmtTabsIndents.class, "FmtTabsIndents.addLeadingStarInCommentCheckBox.AccessibleContext.accessibleDescription")); // NOI18N
    }// </editor-fold>//GEN-END:initComponents
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox absoluteLabelIndentCheckBox;
    private javax.swing.JCheckBox addLeadingStarInCommentCheckBox;
    private javax.swing.JTextField continuationIndentSizeField;
    private javax.swing.JLabel continuationIndentSizeLabel;
    private javax.swing.JCheckBox indentCasesFromSwitchCheckBox;
    private javax.swing.JCheckBox indentTopLevelClassMembersCheckBox;
    private javax.swing.JTextField labelIndentField;
    private javax.swing.JLabel labelIndentLabel;
    // End of variables declaration//GEN-END:variables
    
}