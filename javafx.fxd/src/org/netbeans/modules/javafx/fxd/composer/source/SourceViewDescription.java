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
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */
public final class SourceViewDescription implements MultiViewDescription, Serializable {
    private static final long serialVersionUID = 2L;
        
    private final FXZDataObject m_dObj;

    public SourceViewDescription(FXZDataObject dObj) {
        m_dObj = dObj;
    }

    public MultiViewElement createElement() {
        assert EventQueue.isDispatchThread();
        return new SourceElement(m_dObj);
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
