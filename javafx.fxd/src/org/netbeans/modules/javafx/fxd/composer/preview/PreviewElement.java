/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.preview;

import com.sun.scenario.scenegraph.JSGPanel;
import java.awt.BorderLayout;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import org.openide.awt.UndoRedo;
import org.netbeans.core.spi.multiview.CloseOperationState;
import org.netbeans.core.spi.multiview.MultiViewElement;
import org.netbeans.core.spi.multiview.MultiViewElementCallback;
import org.netbeans.modules.javafx.fxd.composer.misc.ActionLookup;
import org.netbeans.modules.javafx.fxd.composer.misc.ActionLookupUtils;
import org.netbeans.modules.javafx.fxd.composer.model.FXDElement;
import org.openide.util.Lookup;

import org.netbeans.modules.javafx.fxd.composer.navigator.SelectionCookie;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZEditorSupport;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.nodes.FilterNode;
import org.openide.util.Mutex;
import org.openide.util.RequestProcessor;
import org.openide.util.lookup.Lookups;
import org.openide.util.lookup.ProxyLookup;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */
public final class PreviewElement extends TopComponent implements MultiViewElement, Runnable {
    private static final long serialVersionUID = 1L;
    
    protected final FXZEditorSupport     m_edSup;
    private   final PreviewImagePanel    m_imgPanel;
    private   final PreviewToolbar       m_toolBar;
    private   final PreviewStatusBar     m_statusBar;
    //protected       SGParent        m_root = null;
    private Lookup m_lookup = null;
    
    public PreviewElement( final FXZEditorSupport edSup) {
        m_edSup = edSup;   
        FXZDataObject dObj = (FXZDataObject) edSup.getDataObject();

        setLayout( new BorderLayout());
        m_statusBar = new PreviewStatusBar();

        m_imgPanel = new PreviewImagePanel(dObj);       
        add( new JScrollPane(m_imgPanel), BorderLayout.CENTER);
        
        add( m_statusBar, BorderLayout.SOUTH);
        
        m_toolBar = new PreviewToolbar(dObj, ActionLookupUtils.merge( new ActionLookup[] {
            m_imgPanel,
            dObj.getController().getActionController()
        }));        
        dObj.getController().setPreviewComponent(this);
        setFocusable(true);
    }
                
    public JComponent getVisualRepresentation() {
        return this;
    }

    public JComponent getToolbarRepresentation() {
        return m_toolBar;
    }

    PreviewStatusBar getStatusBar() {
        return m_statusBar;
    }
    
    @Override
    public Action[] getActions() {
        return new Action[0];
        //throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public synchronized Lookup getLookup() {
        if ( m_lookup == null) {
            m_lookup = createLookup();
        }
        return m_lookup;
    }
    
    public JSGPanel getJSGPane() {
        return m_imgPanel.getJSGPanel();
    }
    
    
    private Lookup createLookup() {
        DataObject dObj = m_edSup.getDataObject();
        return Lookups.fixed( new Object[] {            
            new FilterNode(dObj.getNodeDelegate(), null, new ProxyLookup(new Lookup[] {
                //new SVGElementNode(elementLookup).getLookup(),
                dObj.getLookup(),
                dObj.getNodeDelegate().getLookup()
            })),
            //ActionMap map = getActionMap(),            
            new FXDPreviewCookie()
        });
    }

    public void refresh() {
        m_toolBar.refresh();
        m_imgPanel.refresh();
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

    private MultiViewElementCallback m_callback;
    
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
        return CloseOperationState.STATE_OK;
        //throw new UnsupportedOperationException("Not supported yet.");
    }
        
    private final class FXDPreviewCookie implements SelectionCookie {
        public void updateSelection(final FXZDataObject doj, FXDElement elem, boolean doubleClick) {
            if ( elem.isVisible()) {
                doj.getController().getSelectionModel().setSelection(elem, true);
            }
        }
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
