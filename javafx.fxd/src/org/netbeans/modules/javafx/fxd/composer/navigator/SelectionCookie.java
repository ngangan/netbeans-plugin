/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.navigator;

import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.javafx.fxd.dataloader.FXDZDataObject;
import org.openide.nodes.Node;

/**
 *
 * @author Pavel Benes
 */
public interface SelectionCookie extends Node.Cookie {
    public void updateSelection(FXDZDataObject doj, DocumentElement de, boolean doubleClick);
}    