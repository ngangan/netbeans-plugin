/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.modules.javafx.editor;

import javax.swing.text.Document;

/**
 * Manager of imports in JavaFX source code.
 *
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 */
public interface JFXImportManager {


    /**
     * Fix imports within source code.
     * @param document document containing source code.
     */
    void fixImports(Document document);
}
