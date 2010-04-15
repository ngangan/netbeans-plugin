/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.preview;

import java.awt.BorderLayout;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import javafx.scene.Scene;
import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import org.openide.awt.UndoRedo;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.javafx.fxd.composer.misc.ActionLookup;
import org.netbeans.modules.javafx.fxd.composer.misc.ActionLookupUtils;
import org.netbeans.modules.javafx.fxd.composer.model.FXDElement;
import org.netbeans.modules.javafx.fxd.composer.model.FXDFileModel;
import org.openide.util.Lookup;

import org.netbeans.modules.javafx.fxd.composer.navigator.SelectionCookie;
import org.netbeans.modules.javafx.fxd.dataloader.FXDZDataObject;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.openide.filesystems.FileObject;
import org.openide.util.Mutex;
import org.openide.util.RequestProcessor;
import org.openide.util.lookup.Lookups;
import org.openide.util.lookup.ProxyLookup;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */
public final class PreviewTopComponent extends TopComponent implements Runnable {
    protected transient final FXZDataObject        m_dObj;
    private   transient final PreviewImagePanel    m_imgPanel;
    private   transient final PreviewToolbar       m_toolBar;
    private   transient final PreviewStatusBar     m_statusBar;
    private   transient       Lookup               m_lookup = null;
    
    public PreviewTopComponent( final FXZDataObject dObj) {
        m_dObj = dObj;   

        setLayout( new BorderLayout());
        m_statusBar = new PreviewStatusBar();

        m_imgPanel = new PreviewImagePanel(dObj);       
        add( new JScrollPane(m_imgPanel), BorderLayout.CENTER);
        
        add( m_statusBar, BorderLayout.SOUTH);
        
        m_toolBar = new PreviewToolbar(dObj, ActionLookupUtils.merge( new ActionLookup[] {
            m_imgPanel,
            dObj.getController().getActionController()
        }));        
        m_dObj.getController().setPreviewComponent(this);
        setFocusable(true);
    }
                

    public JComponent getToolbarRepresentation() {
        return m_toolBar;
    }

    PreviewStatusBar getStatusBar() {
        return m_statusBar;
    }

    @Override
    public synchronized Lookup getLookup() {
        if ( m_lookup == null) {
            m_lookup = createLookup();
        }
        return m_lookup;
    }
    
//    public JSGPanel getJSGPane() {
//        return m_imgPanel.getJSGPanel();
//    }

    public JComponent getScenePanel() {
        return m_imgPanel.getScenePanel();
    }

    public Scene getScene() {
        return m_imgPanel.getScene();
    }

    /*
    private Lookup createLookup() {
        return Lookups.fixed( new Object[] {            
            new FilterNode(m_dObj.getNodeDelegate(), null, new ProxyLookup(new Lookup[] {
                //new SVGElementNode(elementLookup).getLookup(),
                m_dObj.getLookup(),
                m_dObj.getNodeDelegate().getLookup()
            })),
            //ActionMap map = getActionMap(),            
            new FXDPreviewCookie()
        });
    }
 */
   private Lookup createLookup() {
        return new ProxyLookup(new org.openide.util.Lookup[] {
            m_dObj.getLookup(),
            m_dObj.getNodeDelegate().getLookup(),
            Lookups.singleton( new FXDPreviewCookie())
        });
    }

    public void refresh() {
        m_toolBar.refresh();
        m_imgPanel.refresh();
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
        refresh();
    }

    @Override
    public void componentHidden() {
    }

    @Override
    public void componentActivated() {
    }

    @Override
    public void componentDeactivated() {
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
   
    private final class FXDPreviewCookie implements SelectionCookie {
        public void updateSelection(final FXDZDataObject doj, DocumentElement de, boolean doubleClick) {
            if ( doj instanceof FXZDataObject) {
                FXZDataObject fxzDO = (FXZDataObject) doj;
                FXDElement elem = new FXDElement( fxzDO, FXDFileModel.getElementId(de));
                if ( elem.isVisible()) {
                    fxzDO.getController().getSelectionModel().setSelection(elem, true);
                }
            } else {
                System.err.println("Invalid data object: " + doj);
            }
        }
    }    
    
    private final FocusListener m_focusListener = new FocusAdapter() { 
        public @Override void focusGained(FocusEvent evt) {
            // Refresh file object when component made active
            if (m_dObj != null) {
                final FileObject fo = m_dObj.getPrimaryFile();
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
