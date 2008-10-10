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
import org.netbeans.modules.javafx.fxd.dataloader.FXDDataObject;
import org.netbeans.modules.javafx.fxd.dataloader.FXDEditorSupport;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */
public class PreviewViewDescription implements MultiViewDescription, Serializable {
    private static final long serialVersionUID = 1L;

    private transient final FXDEditorSupport m_edSup;
    //private transient PreviewElement      m_preview = null;


    public PreviewViewDescription(final FXDEditorSupport edSup) {
        m_edSup = edSup;
    }
    
    public synchronized MultiViewElement createElement() {
        assert EventQueue.isDispatchThread();
        //assert m_preview == null;
        //m_preview = new PreviewElement(m_edSup);
        //return m_preview;
        return new PreviewElement(m_edSup);
    }

    public Image getIcon() {
        return null;
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
        return NbBundle.getMessage(FXDDataObject.class, "LBL_MULTIVIEW_PREVIEW_TITLE"); //NOI18N
    }
}
