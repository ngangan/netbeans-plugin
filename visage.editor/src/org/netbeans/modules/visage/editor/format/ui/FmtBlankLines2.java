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

package org.netbeans.modules.visage.editor.format.ui;

import org.netbeans.modules.visage.editor.format.CodeStyle.BracePlacement;
import org.netbeans.modules.visage.editor.format.CodeStyle.WrapStyle;
import static org.netbeans.modules.visage.editor.format.ui.FmtOptions.*;
import static org.netbeans.modules.visage.editor.format.ui.FmtOptions.CategorySupport.OPTION_ID;
import org.netbeans.modules.options.editor.spi.PreferencesCustomizer;

public class FmtBlankLines2 extends javax.swing.JPanel {
    
    public FmtBlankLines2() {
        initComponents();
        
        bNonClassExpressionField.putClientProperty(OPTION_ID, blankLinesBeforeNonClassExpression);
        aNonClassExpressionField.putClientProperty(OPTION_ID, blankLinesAfterNonClassExpression);
        
        bNonClassExpressionField.addKeyListener(NumericKeyListener.getInstance());
        aNonClassExpressionField.addKeyListener(NumericKeyListener.getInstance());

    }
    
    public static PreferencesCustomizer.Factory getController() {
        return new CategorySupport.Factory("blank-lines2", FmtBlankLines2.class, //NOI18N
                org.openide.util.NbBundle.getMessage(FmtBlankLines2.class, "SAMPLE_BlankLines2"), // NOI18N
                new String[]{FmtOptions.rightMargin, "30"}, //NOI18N
                new String[]{FmtOptions.wrapSequenceInit, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapAssignOps, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapBinaryOps, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapChainedFunctionCalls, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapExtendsImplementsKeyword, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapExtendsImplementsList, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapForStatement, WrapStyle.WRAP_ALWAYS.name()},
                new String[]{FmtOptions.wrapIfExpression, WrapStyle.WRAP_ALWAYS.name()},
                new String[]{FmtOptions.wrapFunctionCallArgs, WrapStyle.WRAP_NEVER.name()},
                new String[]{FmtOptions.wrapFunctionParams, WrapStyle.WRAP_NEVER.name()},
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

        bNonClassExpressionLabel = new javax.swing.JLabel();
        bNonClassExpressionField = new javax.swing.JTextField();
        aNonClassExpressionLabel = new javax.swing.JLabel();
        aNonClassExpressionField = new javax.swing.JTextField();

        setName(org.openide.util.NbBundle.getMessage(FmtBlankLines2.class, "LBL_BlankLines2")); // NOI18N
        setOpaque(false);

        org.openide.awt.Mnemonics.setLocalizedText(bNonClassExpressionLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines2.class, "FmtBlankLines2.bNonClassExpressionLabel.text")); // NOI18N

        bNonClassExpressionField.setColumns(5);
        bNonClassExpressionField.setText(org.openide.util.NbBundle.getMessage(FmtBlankLines2.class, "FmtBlankLines2.bNonClassExpressionField.text")); // NOI18N

        org.openide.awt.Mnemonics.setLocalizedText(aNonClassExpressionLabel, org.openide.util.NbBundle.getMessage(FmtBlankLines2.class, "FmtBlankLines2.aNonClassExpressionLabel.text")); // NOI18N

        aNonClassExpressionField.setColumns(5);
        aNonClassExpressionField.setText(org.openide.util.NbBundle.getMessage(FmtBlankLines2.class, "FmtBlankLines2.aNonClassExpressionField.text")); // NOI18N

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(bNonClassExpressionLabel)
                    .addComponent(aNonClassExpressionLabel))
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(aNonClassExpressionField)
                    .addComponent(bNonClassExpressionField)))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(bNonClassExpressionLabel)
                    .addComponent(bNonClassExpressionField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(aNonClassExpressionLabel)
                    .addComponent(aNonClassExpressionField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTextField aNonClassExpressionField;
    private javax.swing.JLabel aNonClassExpressionLabel;
    private javax.swing.JTextField bNonClassExpressionField;
    private javax.swing.JLabel bNonClassExpressionLabel;
    // End of variables declaration//GEN-END:variables
    
}
