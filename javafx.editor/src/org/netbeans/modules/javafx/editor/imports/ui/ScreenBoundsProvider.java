/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.modules.javafx.editor.imports.ui;

import javax.swing.text.JTextComponent;
import java.awt.*;

/**
 * Provides screen bounds
 * @author Max Sauer
 */
class ScreenBoundsProvider {

    /** Relative maximum width of screen covered by CC */
    static final double MAX_COMPL_COVERAGE = 0.4;

    private static Rectangle screenBounds;

    static Rectangle getScreenBounds(JTextComponent editorComponent) {
        if (screenBounds == null) {
            GraphicsConfiguration configuration = editorComponent != null
                    ? editorComponent.getGraphicsConfiguration() : null;
            screenBounds = configuration != null
                    ? configuration.getBounds() : new Rectangle();
        }
        return screenBounds;
    }

    static void clear() {
        screenBounds = null;
    }
}

