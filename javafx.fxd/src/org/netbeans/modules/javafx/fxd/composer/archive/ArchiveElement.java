/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.archive;

import java.io.Serializable;
import javax.swing.Action;
import javax.swing.JComponent;
import org.netbeans.core.spi.multiview.CloseOperationState;
import org.netbeans.core.spi.multiview.MultiViewElement;
import org.netbeans.core.spi.multiview.MultiViewElementCallback;
import org.netbeans.core.spi.multiview.MultiViewFactory;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZEditorSupport;
import org.openide.awt.UndoRedo;
import org.openide.util.Lookup;

/**
 *
 * @author Pavel Benes
 */
final class ArchiveElement implements MultiViewElement, Serializable {
    private static final long serialVersionUID = 2L;
    
    private final     FXZDataObject       m_dObj;
    private transient ArchiveTopComponent m_archiveTC = null;

    ArchiveElement(FXZDataObject dObj) {
        m_dObj = dObj;
    }
    
    private synchronized ArchiveTopComponent getArchiveTC() {
        if ( m_archiveTC == null) {
            m_archiveTC = new ArchiveTopComponent( m_dObj);
        }
        return m_archiveTC;
    }

    public JComponent getVisualRepresentation() {
        return getArchiveTC();
    }
    
    public JComponent getToolbarRepresentation() {
        return getArchiveTC().getToolbar();
    }

    public Action[] getActions() {
        Action[] acts = getArchiveTC().getActions();
        return FXZEditorSupport.removeCloneAction(acts);
    }

    public Lookup getLookup() {
        return getArchiveTC().getLookup();
    }

    public void componentOpened() {
        getArchiveTC().componentOpened();
    }

    public void componentClosed() {
        getArchiveTC().componentClosed();
        synchronized(this) {
            m_archiveTC = null;
        }
    }

    public void componentShowing() {
        getArchiveTC().componentShowing();
    }

    public void componentHidden() {
        getArchiveTC().componentHidden();
    }

    public void componentActivated() {
        getArchiveTC().componentActivated();
    }

    public void componentDeactivated() {
        getArchiveTC().componentDeactivated();
    }

    public UndoRedo getUndoRedo() {
        return getArchiveTC().getUndoRedo();
    }

    public void setMultiViewCallback(MultiViewElementCallback callback) {
        m_dObj.setMultiviewElementCallback(callback);
        getArchiveTC().updateName();
    }

    public CloseOperationState canCloseElement() {
        return MultiViewFactory.createUnsafeCloseState(
            "ID_FXZ_CLOSING", // NOI18N
            MultiViewFactory.NOOP_CLOSE_ACTION,
            MultiViewFactory.NOOP_CLOSE_ACTION);
    }    
}
