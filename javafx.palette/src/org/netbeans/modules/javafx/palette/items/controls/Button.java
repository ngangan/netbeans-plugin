/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.palette.items.controls;

import javax.swing.text.JTextComponent;
import org.netbeans.api.javafx.source.Imports;
import org.netbeans.lib.editor.codetemplates.api.CodeTemplate;
import org.netbeans.lib.editor.codetemplates.api.CodeTemplateManager;
import org.openide.text.ActiveEditorDrop;
import org.openide.util.NbBundle;

/**
 *
 * @author Michal Skvor
 */
public class Button implements ActiveEditorDrop {

    public boolean handleTransfer( JTextComponent targetComponent ) {
        String code = NbBundle.getMessage( Button.class, "TEMPLATE_Button" ); // NOI18N
        CodeTemplateManager ctm = CodeTemplateManager.get( targetComponent.getDocument());
        CodeTemplate template = ctm.createTemporary( code );
        template.insert( targetComponent );

        // Imports
        Imports.addImport( targetComponent, "javafx.scene.control.Button" ); // NOI18N

        return true;
    }
}
