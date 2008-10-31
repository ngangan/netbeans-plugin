/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.archive;

import java.awt.BorderLayout;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import javax.swing.Action;
import javax.swing.JComponent;
import org.netbeans.core.spi.multiview.CloseOperationState;
import org.netbeans.core.spi.multiview.MultiViewElement;
import org.netbeans.core.spi.multiview.MultiViewElementCallback;
import org.netbeans.core.spi.multiview.MultiViewFactory;
import org.netbeans.modules.javafx.fxd.composer.model.FXZArchive;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZEditorSupport;
import org.openide.awt.UndoRedo;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.util.Mutex;
import org.openide.util.RequestProcessor;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */
final class ArchiveElement extends TopComponent implements MultiViewElement, Runnable {
    protected final FXZEditorSupport         m_edSup;
    private         MultiViewElementCallback m_callback;
    private         ArchivePanel             m_panel;
    private         ArchiveToolbar           m_toolbar;
    
    public ArchiveElement( final FXZEditorSupport edSup) {
        m_edSup = edSup;   
        FXZArchive fxzArchive = ((FXZDataObject) edSup.getDataObject()).getDataModel().getFXDContainer();
        if ( fxzArchive != null) {
            m_panel = new ArchivePanel(fxzArchive);
            setLayout( new BorderLayout());
            add( m_panel, BorderLayout.CENTER);
        } else {
            m_panel = null;
        }
        setFocusable(true);
    }
                
    public JComponent getVisualRepresentation() {
        return this;
    }

    public JComponent getToolbarRepresentation() {
        if ( m_toolbar == null) {
            m_toolbar = new ArchiveToolbar(m_panel);
        }
        return m_toolbar;
    }

    
    @Override
    public Action[] getActions() {
        return new Action[0];
        //throw new UnsupportedOperationException("Not supported yet.");
    }
    
    @Override
    public void componentOpened() {
        ((FXZDataObject) m_edSup.getDataObject()).init();
        addFocusListener(m_focusListener);
    }

    @Override
    public void componentClosed() {
        removeFocusListener(m_focusListener);
        ((FXZDataObject) m_edSup.getDataObject()).reset();
    }
    
    @Override
    public void componentShowing() {
        //refresh();
    }

    @Override
    public void componentHidden() {
    }

    @Override
    public void componentActivated() {
//        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void componentDeactivated() {
       // throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public UndoRedo getUndoRedo() {
        return null;
        //throw new UnsupportedOperationException("Not supported yet.");
    }

    
    public void setMultiViewCallback(MultiViewElementCallback callback) {
        m_callback = callback;
        updateName();
    }
    
    public void updateName() {
        Mutex.EVENT.readAccess(this);
    }

    public void run() {
        MultiViewElementCallback c = m_callback;
        if ( c == null) {
            return;
        }
        TopComponent tc = c.getTopComponent();
        if ( tc == null) {
            return;            
        }
        
        String name = m_edSup.messageName();
        tc.setName(name);
        tc.setDisplayName(name);
        name = m_edSup.messageHtmlName();
        tc.setHtmlDisplayName( name);
    }

    public CloseOperationState canCloseElement() {
        return MultiViewFactory.createUnsafeCloseState(
            "ID_FXZ_CLOSING", // NOI18N
            MultiViewFactory.NOOP_CLOSE_ACTION,
            MultiViewFactory.NOOP_CLOSE_ACTION);
    }
    
    private final FocusListener m_focusListener = new FocusAdapter() { 
        public @Override void focusGained(FocusEvent evt) {
            // Refresh file object when component made active
            DataObject dob = m_edSup.getDataObject();
            if (dob != null) {
                final FileObject fo = dob.getPrimaryFile();
                if (fo != null) {
                    RequestProcessor.getDefault().post(new Runnable() {
                        public void run() {
                            fo.refresh();
                        }
                    });
                }
            }
        }
    };    
    
}
