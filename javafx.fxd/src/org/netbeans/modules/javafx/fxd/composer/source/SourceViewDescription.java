/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.source;

import java.awt.EventQueue;
import java.io.Serializable;
import org.netbeans.core.spi.multiview.MultiViewDescription;
import org.netbeans.core.spi.multiview.MultiViewElement;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataNode;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZEditorSupport;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */
public final class SourceViewDescription implements MultiViewDescription, Serializable {
    private static final long serialVersionUID = 1L;
        
    private FXDSourceEditor        m_editor = null;
    private final FXZEditorSupport m_support;

    public SourceViewDescription(FXZEditorSupport ed) {
        m_support = ed;
    }

    public MultiViewElement createElement() {
        assert EventQueue.isDispatchThread();
        if (m_editor == null) {
            m_editor = new FXDSourceEditor(m_support);
        }
        return m_editor;
    }

    public java.awt.Image getIcon() {
        return FXZDataNode.FILE_IMAGE;
    }

    public String preferredID() {
        return "source"; //NOI18N
    }

    public int getPersistenceType() {
        return TopComponent.PERSISTENCE_ONLY_OPENED;
    }

    public String getDisplayName() {
        return NbBundle.getMessage(SourceViewDescription.class, "LBL_MULVIEW_SOURCE_TITLE");  //NOI18N
    }

    public HelpCtx getHelpCtx() {
        return new HelpCtx(HelpCtx.class.getName() + ".DEFAULT_HELP"); // NOI18N
    }        
}
