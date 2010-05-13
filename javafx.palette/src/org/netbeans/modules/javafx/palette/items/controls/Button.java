/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.palette.items.controls;

import javax.swing.text.JTextComponent;
import org.netbeans.modules.javafx.palette.JavaFXPaletteUtilities;
import org.openide.text.ActiveEditorDrop;

/**
 *
 * @author Michal Skvor
 */
public class Button implements ActiveEditorDrop {

    private static final String templateName = "TEMPLATE_Button"; //NOI18N

    public boolean handleTransfer( JTextComponent targetComponent ) {
        return JavaFXPaletteUtilities.insertSnippet(Button.class, templateName, targetComponent, "javafx.scene.control.Button"); //NOI18N
    }
}
