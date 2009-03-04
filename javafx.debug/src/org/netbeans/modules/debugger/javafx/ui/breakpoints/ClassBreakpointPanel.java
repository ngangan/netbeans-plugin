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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
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

package org.netbeans.modules.debugger.javafx.ui.breakpoints;

import java.awt.Dimension;
import java.beans.PropertyChangeListener;
import javax.swing.JPanel;

import org.netbeans.api.debugger.DebuggerManager;
import org.netbeans.api.debugger.Breakpoint.HIT_COUNT_FILTERING_STYLE;
import org.netbeans.api.debugger.javafx.ClassLoadUnloadBreakpoint;
import org.netbeans.modules.debugger.javafx.ui.EditorContextBridge;
import org.netbeans.spi.debugger.ui.Controller;

import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.util.NbBundle;

/**
 * @author  Jan Jancura
 */
// <RAVE>
// Implement HelpCtx.Provider interface to provide help ids for help system
// public class ClassBreakpointPanel extends JPanel implements Controller {
// ====
public class ClassBreakpointPanel extends JPanel implements Controllable, org.openide.util.HelpCtx.Provider {
// </RAVE>
    
    private ConditionsPanel             conditionsPanel;
    private ActionsPanel                actionsPanel; 
    private ClassLoadUnloadBreakpoint   breakpoint;
    private boolean                     createBreakpoint = false;

    private Controller                  controller = new ControllerImpl();
    
    private static ClassLoadUnloadBreakpoint creteBreakpoint () {
        String className;
        try {
            className = EditorContextBridge.getContext().getCurrentClassName();
        } catch (java.awt.IllegalComponentStateException icsex) {
            className = "";
        }
        ClassLoadUnloadBreakpoint mb = ClassLoadUnloadBreakpoint.create (
            className,
            false, 
            ClassLoadUnloadBreakpoint.TYPE_CLASS_LOADED_UNLOADED
        );
        mb.setPrintText (
            NbBundle.getBundle (ClassBreakpointPanel.class).getString 
                ("CTL_Class_Breakpoint_Print_Text")	//NOI18N
        );
        return mb;
    }
    
    
    /** Creates new form LineBreakpointPanel */
    public ClassBreakpointPanel () {
        this (creteBreakpoint ());
        createBreakpoint = true;
    }
    
    /** Creates new form LineBreakpointPanel */
    public ClassBreakpointPanel (ClassLoadUnloadBreakpoint b) {
        breakpoint = b;
        initComponents ();
        
        String[] cf = b.getClassFilters ();
        tfClassName.setText(concatClassFilters(cf));
        
        cbBreakpointType.addItem (NbBundle.getMessage(ClassBreakpointPanel.class, "LBL_Class_Breakpoint_Type_Prepare"));
        cbBreakpointType.addItem (NbBundle.getMessage(ClassBreakpointPanel.class, "LBL_Class_Breakpoint_Type_Unload"));
        cbBreakpointType.addItem (NbBundle.getMessage(ClassBreakpointPanel.class, "LBL_Class_Breakpoint_Type_Prepare_or_Unload"));
        switch (b.getBreakpointType ()) {
            case ClassLoadUnloadBreakpoint.TYPE_CLASS_LOADED:
                cbBreakpointType.setSelectedIndex (0);
                break;
            case ClassLoadUnloadBreakpoint.TYPE_CLASS_UNLOADED:
                cbBreakpointType.setSelectedIndex (1);
                break;
            case ClassLoadUnloadBreakpoint.TYPE_CLASS_LOADED_UNLOADED:
                cbBreakpointType.setSelectedIndex (2);
                break;
        }
        
        conditionsPanel = new ConditionsPanel();
        cPanel.add(conditionsPanel, "Center");	//NOI18N
        conditionsPanel.showExclusionClassFilter(true);
        conditionsPanel.showCondition(false);
        conditionsPanel.setClassExcludeFilter(b.getClassExclusionFilters());
        conditionsPanel.setHitCountFilteringStyle(b.getHitCountFilteringStyle());
        conditionsPanel.setHitCount(b.getHitCountFilter());
        
        actionsPanel = new ActionsPanel (b);
        pActions.add (actionsPanel, "Center");	//NOI18N
        // <RAVE>
        // The help IDs for the AddBreakpointPanel panels have to be different from the
        // values returned by getHelpCtx() because they provide different help
        // in the 'Add Breakpoint' dialog and when invoked in the 'Breakpoints' view
        putClientProperty("HelpID_AddBreakpointPanel", "debug.add.breakpoint.java.class"); // NOI18N
        // </RAVE>
    }

    public Controller getController() {
        return controller;
    }

    // <RAVE>
    // Implement getHelpCtx() with the correct helpID
    public org.openide.util.HelpCtx getHelpCtx() {
       return new org.openide.util.HelpCtx("NetbeansDebuggerBreakpointClassJavaFX"); // NOI18N
    }
    // </RAVE>


    static String concatClassFilters(String[] cf) {
        if (cf.length > 0) {
            StringBuilder sb = new StringBuilder(cf[0]);
            for (int i = 1; i < cf.length; i++) {
                sb.append(", ");	//NOI18N
                sb.append(cf[i]);
            }
            return sb.toString();
        } else {
            return "";
        }
    }

    static String[] parseClassFilters(String classFilter) {
        int numFilters = 1;
        int length = classFilter.length();
        if (length == 0) {
            return new String[0];
        }
        for (int i = 0; i < length; i++) {
            if (classFilter.charAt(i) == ',') numFilters++;
        }
        String[] classFilters = new String[numFilters];
        if (numFilters == 1) {
            classFilters[0] = classFilter;
        } else {
            int i = 0;
            int pos = 0;
            while (pos < length) {
                int end = classFilter.indexOf(",");	//NOI18N
                if (end < 0) end = length;
                classFilters[i] = classFilter.substring(pos, end).trim();
                i++;
                pos = end + 1;
            }
        }
        return classFilters;
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc=" Generated Code ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        pSettings = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        tfClassName = new javax.swing.JTextField();
        jLabel4 = new javax.swing.JLabel();
        cbBreakpointType = new javax.swing.JComboBox();
        cPanel = new javax.swing.JPanel();
        pActions = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();

        setLayout(new java.awt.GridBagLayout());

        java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("org/netbeans/modules/debugger/javafx/ui/breakpoints/Bundle"); // NOI18N
        pSettings.setBorder(javax.swing.BorderFactory.createTitledBorder(bundle.getString("L_Class_Breakpoint_BorderTitle"))); // NOI18N
        pSettings.setLayout(new java.awt.GridBagLayout());

        jLabel3.setLabelFor(tfClassName);
        org.openide.awt.Mnemonics.setLocalizedText(jLabel3, bundle.getString("L_Class_Breakpoint_Class_Name")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(3, 3, 3, 3);
        pSettings.add(jLabel3, gridBagConstraints);
        jLabel3.getAccessibleContext().setAccessibleDescription(bundle.getString("ACSD_L_Class_Breakpoint_Class_Name")); // NOI18N

        tfClassName.setToolTipText(bundle.getString("TTT_TF_Class_Breakpoint_Class_Name")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(3, 3, 3, 3);
        pSettings.add(tfClassName, gridBagConstraints);
        tfClassName.getAccessibleContext().setAccessibleDescription(bundle.getString("ACSD_TF_Class_Breakpoint_Class_Name")); // NOI18N

        jLabel4.setLabelFor(cbBreakpointType);
        org.openide.awt.Mnemonics.setLocalizedText(jLabel4, bundle.getString("L_Class_Breakpoint_Type")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(3, 3, 3, 3);
        pSettings.add(jLabel4, gridBagConstraints);
        jLabel4.getAccessibleContext().setAccessibleDescription(bundle.getString("ASCD_L_Class_Breakpoint_Type")); // NOI18N

        cbBreakpointType.setToolTipText(bundle.getString("TTT_CB_Class_Breakpoint_Type")); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(3, 3, 3, 3);
        pSettings.add(cbBreakpointType, gridBagConstraints);
        cbBreakpointType.getAccessibleContext().setAccessibleDescription(bundle.getString("ACSD_CB_Class_Breakpoint_Type")); // NOI18N

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        add(pSettings, gridBagConstraints);

        cPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        add(cPanel, gridBagConstraints);

        pActions.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        add(pActions, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jPanel1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

       
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel cPanel;
    private javax.swing.JComboBox cbBreakpointType;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel pActions;
    private javax.swing.JPanel pSettings;
    private javax.swing.JTextField tfClassName;
    // End of variables declaration//GEN-END:variables

    /**
     * Controller implementation
     */
    private class ControllerImpl implements Controller {

        public boolean ok() {
            String msg = valiadateMsg();
            if (msg == null) {
                msg = conditionsPanel.valiadateMsg();
            }
            if (msg != null) {
                DialogDisplayer.getDefault().notify(new NotifyDescriptor.Message(msg));
                return false;
            }
            actionsPanel.ok ();

            String className = tfClassName.getText ().trim ();
            breakpoint.setClassFilters(parseClassFilters(className));
            breakpoint.setClassExclusionFilters(conditionsPanel.getClassExcludeFilter());//parseClassFilters(className));

            switch (cbBreakpointType.getSelectedIndex ()) {
                case 0:
                    breakpoint.setBreakpointType (ClassLoadUnloadBreakpoint.TYPE_CLASS_LOADED);
                    break;
                case 1:
                    breakpoint.setBreakpointType (ClassLoadUnloadBreakpoint.TYPE_CLASS_UNLOADED);
                    break;
                case 2:
                    breakpoint.setBreakpointType (ClassLoadUnloadBreakpoint.TYPE_CLASS_LOADED_UNLOADED);
                    break;
            }
            breakpoint.setHitCountFilter(conditionsPanel.getHitCount(),
                    conditionsPanel.getHitCountFilteringStyle());
            if (createBreakpoint)
                DebuggerManager.getDebuggerManager ().addBreakpoint (breakpoint);
            return true;
        }

        public boolean cancel() {
            return true;
        }

        public boolean isValid() {
            return true;
        }

        public void addPropertyChangeListener(PropertyChangeListener arg0) {
//            throw new UnsupportedOperationException("Not supported yet.");
        }

        public void removePropertyChangeListener(PropertyChangeListener arg0) {
//            throw new UnsupportedOperationException("Not supported yet.");
        }

        private String valiadateMsg () {
            if (tfClassName.getText().trim ().length() == 0) {
                return NbBundle.getMessage(ClassBreakpointPanel.class, "MSG_No_Class_Name_Spec");
            }
            return null;
        }
    }
}