/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package org.netbeans.modules.javafx.fxd.composer.source;

import java.io.Serializable;
import javax.swing.Action;
import javax.swing.JComponent;
import org.netbeans.core.spi.multiview.CloseOperationState;
import org.netbeans.core.spi.multiview.MultiViewElement;
import org.netbeans.core.spi.multiview.MultiViewElementCallback;
import org.netbeans.core.spi.multiview.MultiViewFactory;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.openide.awt.UndoRedo;
import org.openide.util.Lookup;

/**
 *
 * @author Pavel Benes
 */
public final class SourceElement implements MultiViewElement, Serializable {
    private static final long serialVersionUID = 2L;
    
    private final     FXZDataObject      m_dObj;
    private transient SourceTopComponent m_sourceTC = null; 
    
    SourceElement(final FXZDataObject dObj)  {
        m_dObj = dObj;
    }

    private synchronized SourceTopComponent getSourceTC() {
        if (m_sourceTC == null) {
            m_sourceTC = new SourceTopComponent(m_dObj);
        }
        return m_sourceTC;
    }     
    
    public JComponent getVisualRepresentation() {
        return getSourceTC();
    }

    public JComponent getToolbarRepresentation() {
        return getSourceTC().getToolbar();
    }

    public Action[] getActions() {
        return getSourceTC().getActions();
    }

    public Lookup getLookup() {
        return getSourceTC().getLookup();
    }

    public void componentOpened() {
        getSourceTC().componentOpened();
    }

    public void componentClosed() {
        getSourceTC().componentClosed();
        synchronized(this) {
            m_sourceTC = null;
        }
    }

    public void componentShowing() {
        getSourceTC().componentShowing();
    }

    public void componentHidden() {
        getSourceTC().componentHidden();
    }

    public void componentActivated() {
        getSourceTC().componentActivated();
    }

    public void componentDeactivated() {
        getSourceTC().componentDeactivated();
    }

    public UndoRedo getUndoRedo() {
        return getSourceTC().getUndoRedo();
    }

    public void setMultiViewCallback(MultiViewElementCallback callback) {
        m_dObj.setMultiviewElementCallback(callback);
        getSourceTC().updateName();
    }

    public CloseOperationState canCloseElement() {
        return MultiViewFactory.createUnsafeCloseState(
            "ID_FXZ_CLOSING", // NOI18N
            MultiViewFactory.NOOP_CLOSE_ACTION,
            MultiViewFactory.NOOP_CLOSE_ACTION);
    }
}
