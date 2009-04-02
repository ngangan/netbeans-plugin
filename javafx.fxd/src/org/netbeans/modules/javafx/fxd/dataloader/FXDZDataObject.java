/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.dataloader;

import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObjectExistsException;
import org.openide.loaders.MultiDataObject;
import org.openide.loaders.MultiFileLoader;
import org.openide.text.DataEditorSupport;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */
public abstract class FXDZDataObject extends MultiDataObject {
    protected FXDZDataObject( FileObject pf, MultiFileLoader loader) throws DataObjectExistsException {
        super(pf, loader);
    }
    
    public abstract DataEditorSupport getEditorSupport();
    public abstract String            getEntryName();
    public abstract TopComponent getMVTC();
}
