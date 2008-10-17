/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.model;

import java.io.FileNotFoundException;
import java.io.IOException;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;

/**
 *
 * @author Pavel Benes
 */
public class FXDComposerModel {
    //private transient final FXDDataObject         m_dObj;    
    private transient final FXZArchive            m_fxzArchive;
    
    /** persistent properties */
    private boolean  m_isHighlightOn = true;   
    private boolean  m_showTooltip   = true;   
    private float    m_zoomRatio     = 1.0f;
            
    public FXDComposerModel( FXZDataObject dObj) throws FileNotFoundException, IOException {
        assert dObj != null;
        //m_dObj = dObj;
        m_fxzArchive = new FXZArchive(dObj);
    }
    
        
    public FXZArchive getFXDContainer() {
        return m_fxzArchive;
    }
    
    /*
    public FXDDataObject getDataObject() {
        return m_dObj;
    }*/
        
    public boolean getIsHighlightOn() {
        return m_isHighlightOn;
    }

    boolean setIsHighlightOn(boolean isHighlightOn) {
        if ( isHighlightOn != m_isHighlightOn) {
            m_isHighlightOn = isHighlightOn;
            return true;
        } else {
            return false;
        }
    }

    public boolean getIsTooltipOn() {
        return m_showTooltip;
    }

    boolean setIsTooltipOn(boolean showTooltip) {
        if ( m_showTooltip != showTooltip) {
            m_showTooltip = showTooltip;
            return true;
        } else {
            return false;
        }
    }
    
    public float getZoomRatio() {
        return m_zoomRatio;
    }
    
    boolean setZoomRatio(float zoomRatio) {
        if ( Math.abs(zoomRatio - m_zoomRatio) > 0.001) {
            m_zoomRatio = zoomRatio;
            return true;
        } else {
            return false;
        }
    }
}
