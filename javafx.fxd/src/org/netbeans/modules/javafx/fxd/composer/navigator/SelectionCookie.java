/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.navigator;

import org.netbeans.modules.javafx.fxd.composer.model.FXDElement;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.openide.nodes.Node;

/**
 *
 * @author Pavel Benes
 */
public interface SelectionCookie extends Node.Cookie {
    public void updateSelection(FXZDataObject doj, FXDElement elem, boolean doubleClick);
}    