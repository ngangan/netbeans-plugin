/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
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
