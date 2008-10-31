/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.preview;

import java.awt.EventQueue;
import java.awt.Image;
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
public class PreviewViewDescription implements MultiViewDescription, Serializable {
    private static final long serialVersionUID = 1L;

    private transient final FXZEditorSupport m_edSup;

    public PreviewViewDescription(final FXZEditorSupport edSup) {
        m_edSup = edSup;
    }
    
    public synchronized MultiViewElement createElement() {
        assert EventQueue.isDispatchThread();
        return new PreviewElement(m_edSup);
    }

    public Image getIcon() {
        return FXZDataNode.FILE_IMAGE;
    }

    public String preferredID() {
        return "preview"; //NOI18N
    }

    public HelpCtx getHelpCtx() {
        return new HelpCtx(HelpCtx.class.getName() + ".DEFAULT_HELP"); // NOI18N
    }        

    public int getPersistenceType() {
        return TopComponent.PERSISTENCE_ONLY_OPENED;
    }

    public String getDisplayName() {
        return NbBundle.getMessage(PreviewViewDescription.class, "LBL_MULTIVIEW_PREVIEW_TITLE"); //NOI18N
    }
}
