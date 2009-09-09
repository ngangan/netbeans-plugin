/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.fxd.dataloader.fxd;

import java.io.IOException;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObjectExistsException;
import org.openide.loaders.MultiDataObject;
import org.openide.loaders.UniFileLoader;
import org.openide.util.NbBundle;

public final class FXDDataLoader extends UniFileLoader {
    public static final String REQUIRED_MIME = "text/x-fxd";   //NOI18N
    private static final long serialVersionUID = 2L;

    public FXDDataLoader() {
        super("org.netbeans.modules.javafx.fxd.dataloader.fxd.FXDDataObject");  //NOI18N
    }

    @Override
    protected String defaultDisplayName() {
        return NbBundle.getMessage(FXDDataLoader.class, "LBL_FXD_loader_name");  //NOI18N
    }

    @Override
    protected void initialize() {
        super.initialize();
        getExtensions().addMimeType(REQUIRED_MIME);
    }

    protected MultiDataObject createMultiObject(FileObject primaryFile) throws DataObjectExistsException, IOException {
        return new FXDDataObject(primaryFile, this);
    }

    @Override
    protected String actionsContext() {
        return "Loaders/" + REQUIRED_MIME + "/Actions";   //NOI18N
    }
}
