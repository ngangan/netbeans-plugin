/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.fxd.dataloader.fxd;

import java.io.IOException;
import org.netbeans.modules.javafx.fxd.dataloader.FXDZDataObject;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObjectExistsException;
import org.openide.nodes.CookieSet;
import org.openide.nodes.Node;
import org.openide.util.Lookup;
import org.openide.windows.TopComponent;

public final class FXDDataObject extends FXDZDataObject {
    private FXDEditorSupport m_edSup;
    
    public FXDDataObject(FileObject pf, FXDDataLoader loader) throws DataObjectExistsException, IOException {
        super(pf, loader);
        CookieSet cookies = getCookieSet();
        //cookies.add((Node.Cookie) DataEditorSupport.create(this, getPrimaryEntry(), cookies));
        cookies.add( m_edSup = new FXDEditorSupport(this, getPrimaryEntry(), cookies));

        cookies.add(this);
    }

    @Override
    protected Node createNodeDelegate() {
        return new FXDDataNode(this, getLookup());
    }

    @Override
    public Lookup getLookup() {
        return getCookieSet().getLookup();
    }

    @Override
    public TopComponent getMVTC() {
        return m_edSup.getTopComponent();
    }
}
