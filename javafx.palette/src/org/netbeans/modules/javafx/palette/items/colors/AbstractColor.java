/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.palette.items.colors;

import javax.swing.text.JTextComponent;
import org.netbeans.modules.javafx.palette.JavaFXPaletteUtilities;
import org.openide.text.ActiveEditorDrop;

/**
 *
 * @author karol harezlak
 */

abstract class AbstractColor implements ActiveEditorDrop {

    private String code;

    public AbstractColor(String code) {
        this.code = code;
    }
    
    public final boolean handleTransfer(JTextComponent targetComponent) {
        return JavaFXPaletteUtilities.insertSnippet(code, targetComponent, "javafx.scene.paint.Color"); //NOI18N;
    }

}
