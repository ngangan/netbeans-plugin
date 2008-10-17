/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.archive;

import java.awt.GridBagConstraints;
import java.awt.Insets;
import javax.swing.JPanel;
import org.netbeans.modules.javafx.fxd.composer.misc.ActionLookup;
import org.netbeans.modules.javafx.fxd.composer.misc.FXDToolbar;
import org.netbeans.modules.javafx.fxd.composer.model.actions.ActionController;

/**
 *
 * @author Pavel Benes
 */
final class ArchiveToolbar extends FXDToolbar {
    
    ArchiveToolbar(ActionLookup lookup) {
        GridBagConstraints constrains = new GridBagConstraints();
        constrains.anchor = GridBagConstraints.WEST;
        constrains.insets = new Insets(0, 3, 0, 2);
        add(createToolBarSeparator(), constrains);
        
        addButton( lookup.get( ArchivePanel.AddArchiveEntryAction.class));
        addButton( lookup.get( ArchivePanel.RemoveArchiveEntryAction.class));
        addButton( lookup.get( ArchivePanel.ReplaceArchiveEntryAction.class));
        
        add(createToolBarSeparator(), constrains);
        addButton( lookup.get(ActionController.GenerateUIStubAction.class));
        
        constrains = new GridBagConstraints();
        constrains.anchor = GridBagConstraints.WEST;
        constrains.fill = GridBagConstraints.HORIZONTAL;
        constrains.weightx = 1.0;
        add(new JPanel(), constrains);        
    }
}
