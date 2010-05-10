/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 */
package org.netbeans.modules.javafx.fxd.composer.model;

import java.io.FileNotFoundException;
import java.io.IOException;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;

import com.sun.javafx.tools.fxd.loader.Profile;
import com.sun.javafx.tools.fxd.container.FXDContainer;
import org.netbeans.modules.javafx.fxd.composer.misc.FXDComposerUtils;

/**
 *
 * @author Pavel Benes
 */
public class FXDComposerModel {
    //private transient final FXDDataObject         m_dObj;    
    private transient FXZArchive  m_fxzArchive;
    private transient Exception   m_fxzArchiveException = null;
    
    /** persistent properties */
    private String        m_selectedEntry  = null;
    private boolean       m_isHighlightOn  = true;
    private boolean       m_showTooltip    = true;
    private float         m_zoomRatio      = 1.0f;
    private Profile m_previewProfile = Profile.desktop;
            
    public FXDComposerModel( FXZDataObject dObj, String cachedEntry) throws FileNotFoundException, IOException {
        assert dObj != null;
        m_selectedEntry = cachedEntry != null ? cachedEntry : FXDContainer.MAIN_CONTENT;
        dObj.updateEditorCookie();
        try {
            m_fxzArchive = new FXZArchive(dObj);
        } catch( Exception e) {
            System.err.println(e.getLocalizedMessage());
            e.printStackTrace();
            m_fxzArchive = null;
            m_fxzArchiveException = e;
        }
    }
                
    public FXZArchive getFXDContainer() {
        return m_fxzArchive;
    }
         
    public Exception getFXDContainerLoadError() {
        return m_fxzArchiveException;
    }
    
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

    public Profile getPreviewProfile() {
        return m_previewProfile;
    }

    boolean setPreviewProfile(Profile profile) {
        if ( profile != m_previewProfile) {
            m_previewProfile = profile;
            return true;
        } else {
            return false;
        }
    }

    public String getSelectedEntry() {
        return m_selectedEntry;
    }

    boolean setSelectedEntry( String selectedEntry) {
        if ( !FXDComposerUtils.safeEquals( selectedEntry, m_selectedEntry)) {
            m_selectedEntry = selectedEntry;
            return true;
        } else {
            return false;
        }
    }
}
