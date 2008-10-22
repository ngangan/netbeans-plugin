/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.dataloader.fxz;

import java.io.IOException;
import java.io.InputStream;
import javax.swing.text.StyledDocument;
import org.netbeans.core.spi.multiview.MultiViewDescription;
import org.netbeans.core.spi.multiview.MultiViewFactory;
import org.netbeans.modules.javafx.fxd.composer.archive.ArchiveViewDescription;
import org.netbeans.modules.javafx.fxd.composer.preview.PreviewViewDescription;
import org.netbeans.modules.javafx.fxd.composer.source.SourceViewDescription;
import org.openide.cookies.EditCookie;
import org.openide.cookies.EditorCookie;
import org.openide.cookies.OpenCookie;
import org.openide.cookies.SaveCookie;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.text.CloneableEditorSupport;
import org.openide.text.DataEditorSupport;
import org.openide.windows.CloneableTopComponent;
import com.sun.javafx.tools.fxd.container.FXDContainer;
import java.io.Serializable;

/**
 *
 * @author Pavel Benes
 */
public final class FXZEditorSupport extends DataEditorSupport implements Serializable, OpenCookie, EditorCookie, EditCookie {
    private static final long  serialVersionUID = 1L;
        
    protected CloneableTopComponent    m_mvtc  = null;
    protected  MultiViewDescription [] m_views = null;
    
    public FXZEditorSupport( FXZDataObject dObj) {
        super(dObj, new FXDEnv(dObj));
    }
        
    @Override
    public StyledDocument openDocument() throws IOException {
        StyledDocument doc = super.openDocument();
        synchronized(this) {
            ((FXZDataObject) getDataObject()).getDataModel().getFXDContainer().documentOpened(doc);
        }
        return doc;
    }
    
    @Override
    public void saveDocument() throws IOException {
        if( env.isModified() && getDataObject().getPrimaryFile().canWrite()) {
            super.saveDocument();
        }
        try {
            ((FXZDataObject) getDataObject()).getDataModel().getFXDContainer().save();
        } catch( IOException e) {
            throw e;
        } catch (Exception e) {
            throw new IOException( "Archive save failed - [" + e.getClass() + "-" + e.getMessage() + "]");
        }
    }
    
    @Override
    protected boolean notifyModified() {
        boolean retValue = super.notifyModified();
        if ( retValue) {
            FXZDataObject dObj = (FXZDataObject) getDataObject();
            dObj.m_ic.add(env);
        }
        return retValue;
    }
    
    @Override
    protected void notifyUnmodified() {
        super.notifyUnmodified();
            FXZDataObject dObj = (FXZDataObject) getDataObject();
            dObj.m_ic.remove(env);
    }
    
    @Override
    protected CloneableEditorSupport.Pane createPane() {
        MultiViewDescription [] views = getViewDescriptions();
        int                     defView = ((FXZDataObject) getDataObject()).getDefaultView();
        m_mvtc = MultiViewFactory.createCloneableMultiView(views, views[ defView]);
        return (CloneableEditorSupport.Pane)m_mvtc;
    }
    
    public CloneableTopComponent getMVTC() {
        return m_mvtc;
    }
    
    @Override
    public String messageHtmlName() {
        return super.messageHtmlName();
    }

    @Override
    public String messageName() {
        return super.messageName();        
    }
    
    protected synchronized MultiViewDescription [] getViewDescriptions() {
        if ( m_views == null) {
            m_views = new MultiViewDescription[] {
                new PreviewViewDescription(this),        
                new SourceViewDescription(this),
                new ArchiveViewDescription(this)
            };
        }
        return m_views;
    }
        
    private static final class FXDEnv extends DataEditorSupport.Env implements SaveCookie {
        private static final long  serialVersionUID = 1L;
        
        public FXDEnv( FXZDataObject obj) {
            super(obj);
        }

        public void save() throws IOException {
            FXZEditorSupport ed = (FXZEditorSupport)this.findCloneableOpenSupport();
            ed.saveDocument();
        }
        
        @Override
        public InputStream inputStream() throws IOException {
            FXDContainer container = ((FXZDataObject)getDataObject()).getDataModel().getFXDContainer();
            return container.open();
        }        
        
        @Override
        protected FileObject getFile() {
            return getDataObject().getPrimaryFile();
        }

        @Override
        protected FileLock takeLock() throws IOException {
            return ((FXZDataObject)getDataObject()).getPrimaryEntry().takeLock();
        }
    }
}
