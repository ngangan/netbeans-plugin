/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.palette.items.controls;

import javax.swing.text.JTextComponent;
import org.netbeans.modules.javafx.palette.JavaFXPaletteUtilities;
import org.openide.text.ActiveEditorDrop;


public class PasswordBox implements ActiveEditorDrop {

    public boolean handleTransfer( JTextComponent targetComponent ) {
        return JavaFXPaletteUtilities.insertSnippet(PasswordBox.class, "TEMPLATE_PasswordBox" , targetComponent, "javafx.scene.control.PasswordBox"); //NOI18N
    }
}
