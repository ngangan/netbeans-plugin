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
public class FmtBlankLines extends javax.swing.JPanel {
    
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
        
        bPackageField.addKeyListener(NumericKeyListener.getInstance());
        aPackageField.addKeyListener(NumericKeyListener.getInstance());
        bImportsField.addKeyListener(NumericKeyListener.getInstance());
        aImportsField.addKeyListener(NumericKeyListener.getInstance());
        bClassField.addKeyListener(NumericKeyListener.getInstance());
        aClassField.addKeyListener(NumericKeyListener.getInstance());
        aClassHeaderField.addKeyListener(NumericKeyListener.getInstance());
        bAttributeField.addKeyListener(NumericKeyListener.getInstance());
        aAttributeField.addKeyListener(NumericKeyListener.getInstance());
        bFunctionField.addKeyListener(NumericKeyListener.getInstance());
        aFunctionField.addKeyListener(NumericKeyListener.getInstance());

    }
    
    public static PreferencesCustomizer.Factory getController() {
        return new CategorySupport.Factory("blank-lines", FmtBlankLines.class, //NOI18N
                org.openide.util.NbBundle.getMessage(FmtAlignment.class, "SAMPLE_BlankLines"), // NOI18N
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
                    .add(aFunctionLabel))
                .add(42, 42, 42)
                .add(layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING, false)
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
                .addContainerGap(org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
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
    private javax.swing.JTextField bPackageField;
    private javax.swing.JLabel bPackageLabel;
    // End of variables declaration//GEN-END:variables
    
}
