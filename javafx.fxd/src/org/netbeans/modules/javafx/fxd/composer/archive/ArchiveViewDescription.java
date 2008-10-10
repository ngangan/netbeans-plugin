/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.archive;

import java.awt.EventQueue;
import java.io.Serializable;
import org.netbeans.core.spi.multiview.MultiViewDescription;
import org.netbeans.core.spi.multiview.MultiViewElement;
import org.netbeans.modules.javafx.fxd.dataloader.FXDDataObject;
import org.netbeans.modules.javafx.fxd.dataloader.FXDEditorSupport;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */
public class ArchiveViewDescription implements MultiViewDescription, Serializable {
    private static final long serialVersionUID = 1L;
        
    private final FXDEditorSupport m_support;
    private       ArchiveElement   m_elem;

    public ArchiveViewDescription(FXDEditorSupport ed) {
        m_support = ed;
    }

    public synchronized MultiViewElement createElement() {
        assert EventQueue.isDispatchThread();
        assert m_elem == null;
        if (m_elem == null) {
            m_elem = new ArchiveElement(m_support);
        }
        return m_elem;
    }

    public java.awt.Image getIcon() {
        return null;
    }

    public String preferredID() {
        return "archive"; //NOI18N
    }

    public int getPersistenceType() {
        return TopComponent.PERSISTENCE_ONLY_OPENED;
    }

    public String getDisplayName() {
        return NbBundle.getMessage(FXDDataObject.class, "LBL_MULTIVIEW_ARCHIVE_TITLE");
    }

    public HelpCtx getHelpCtx() {
        return new HelpCtx(HelpCtx.class.getName() + ".DEFAULT_HELP"); // NOI18N
    }        
}

