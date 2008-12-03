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
import org.netbeans.modules.javafx.fxd.composer.model.FXZArchive;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZEditorSupport;
import org.openide.awt.UndoRedo;
import org.openide.filesystems.FileObject;
import org.openide.util.Lookup;
import org.openide.util.Mutex;
import org.openide.util.RequestProcessor;
import org.openide.util.lookup.ProxyLookup;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */
final class ArchiveTopComponent extends TopComponent implements Runnable {
    protected transient final FXZDataObject  m_dObj;
    protected transient       ArchivePanel   m_panel;
    protected transient       ArchiveToolbar m_toolbar;
    protected transient       Lookup         m_lookup;
    
    public ArchiveTopComponent( final FXZDataObject dObj) {
        m_dObj = dObj;   
        FXZArchive fxzArchive = dObj.getDataModel().getFXDContainer();
        if ( fxzArchive != null) {
            m_panel = new ArchivePanel(fxzArchive);
            setLayout( new BorderLayout());
            add( m_panel, BorderLayout.CENTER);
        } else {
            m_panel = null;
        }
        setFocusable(true);
    }
                
    public JComponent getToolbar() {
        if ( m_toolbar == null) {
            m_toolbar = new ArchiveToolbar(m_panel);
        }
        return m_toolbar;
    }

    @Override
    public synchronized Lookup getLookup() {
        if ( m_lookup == null) {
            m_lookup = new ProxyLookup(new org.openide.util.Lookup[] {
                m_dObj.getLookup(),
                m_dObj.getNodeDelegate().getLookup()
            });
        }

        return m_lookup;
    }

    @Override
    public Action[] getActions() {
        return new Action[0];
        //throw new UnsupportedOperationException("Not supported yet.");
    }
    
    @Override
    public void componentOpened() {
        m_dObj.init();
        addFocusListener(m_focusListener);
    }

    @Override
    public void componentClosed() {
        removeFocusListener(m_focusListener);
        m_dObj.reset();
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
    
    protected void updateName() {
        Mutex.EVENT.readAccess(this);
    }    
    
    public void run() {
        m_dObj.updateTCName();
    }    
    
    private final FocusListener m_focusListener = new FocusAdapter() { 
        public @Override void focusGained(FocusEvent evt) {
            // Refresh file object when component made active
            final FileObject fo = m_dObj.getPrimaryFile();
            if (fo != null) {
                RequestProcessor.getDefault().post(new Runnable() {
                    public void run() {
                        fo.refresh();
                    }
                });
            }
        }
    };                
}
