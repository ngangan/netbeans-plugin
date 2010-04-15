/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package org.netbeans.modules.javafx.fxd.composer.preview;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import javax.swing.Action;
import javax.swing.JComponent;
import org.netbeans.core.spi.multiview.CloseOperationState;
import org.netbeans.core.spi.multiview.MultiViewElement;
import org.netbeans.core.spi.multiview.MultiViewElementCallback;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZEditorSupport;
import org.openide.awt.UndoRedo;
import org.openide.util.Lookup;

/**
 *
 * @author Pavel Benes
 */
final class PreviewElement implements MultiViewElement, Serializable {
    private static final long serialVersionUID = 2L;

    private final     FXZDataObject       m_dObj;
    private           String              m_entryCached;
    private transient PreviewTopComponent m_previewTC = null;
    
    PreviewElement( FXZDataObject dObj) {
        m_dObj = dObj;
    }
    
    private synchronized PreviewTopComponent getPreviewTC() {
        if ( m_previewTC == null) {
            m_previewTC = new PreviewTopComponent(m_dObj);
        }
        return m_previewTC;
    }

    public JComponent getVisualRepresentation() {
        return getPreviewTC();
    }
    
    public JComponent getToolbarRepresentation() {
        return getPreviewTC().getToolbarRepresentation();
    }

    public Action[] getActions() {
        Action[] acts = getPreviewTC().getActions();
        return FXZEditorSupport.removeCloneAction(acts);
    }

    public Lookup getLookup() {
        return getPreviewTC().getLookup();
    }

    public void componentOpened() {        
        getPreviewTC().componentOpened();
    }

    public void componentClosed() {
        getPreviewTC().componentClosed();
        synchronized(this) {
            m_previewTC = null;
        }
    }

    public void componentShowing() {
        getPreviewTC().componentShowing();
    }

    public void componentHidden() {
        getPreviewTC().componentHidden();
    }

    public void componentActivated() {
        getPreviewTC().componentActivated();
    }

    public void componentDeactivated() {
        getPreviewTC().componentDeactivated();
    }

    public UndoRedo getUndoRedo() {
        return getPreviewTC().getUndoRedo();
    }

    public void setMultiViewCallback(MultiViewElementCallback callback) {
        m_dObj.setMultiviewElementCallback(callback);
        getPreviewTC().updateName();
    }

    public CloseOperationState canCloseElement() {
        return CloseOperationState.STATE_OK;
    }

    private void writeObject(ObjectOutputStream out) throws IOException {
        m_entryCached = m_dObj.getEntryName();
        out.defaultWriteObject();
    }

    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
        in.defaultReadObject();
        m_dObj.setCachedEntry(m_entryCached);
    }
}
