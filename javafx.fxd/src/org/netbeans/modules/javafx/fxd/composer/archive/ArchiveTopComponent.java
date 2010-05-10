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

package org.netbeans.modules.javafx.fxd.composer.archive;

import java.awt.BorderLayout;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import javax.swing.JComponent;
import org.netbeans.modules.javafx.fxd.composer.model.FXZArchive;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.openide.awt.UndoRedo;
import org.openide.filesystems.FileObject;
import org.openide.util.Lookup;
import org.openide.util.Mutex;
import org.openide.util.RequestProcessor;
import org.openide.util.lookup.ProxyLookup;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */
final class ArchiveTopComponent extends TopComponent implements Runnable {
    protected transient final FXZDataObject  m_dObj;
    protected transient       ArchivePanel   m_panel;
    protected transient       ArchiveToolbar m_toolbar;
    protected transient       Lookup         m_lookup;
    
    public ArchiveTopComponent( final FXZDataObject dObj) {
        m_dObj = dObj;   
        FXZArchive fxzArchive = dObj.getDataModel().getFXDContainer();
        if ( fxzArchive != null) {
            m_panel = new ArchivePanel(fxzArchive);
            setLayout( new BorderLayout());
            add( m_panel, BorderLayout.CENTER);
        } else {
            m_panel = null;
        }
        setFocusable(true);
    }
                
    public JComponent getToolbar() {
        if ( m_toolbar == null) {
            m_toolbar = new ArchiveToolbar(m_panel);
        }
        return m_toolbar;
    }

    @Override
    public synchronized Lookup getLookup() {
        if ( m_lookup == null) {
            m_lookup = new ProxyLookup(new org.openide.util.Lookup[] {
                m_dObj.getLookup(),
                m_dObj.getNodeDelegate().getLookup()
            });
        }

        return m_lookup;
    }

    @Override
    public void componentOpened() {
        m_dObj.init();
        addFocusListener(m_focusListener);
    }

    @Override
    public void componentClosed() {
        removeFocusListener(m_focusListener);
        m_dObj.reset();
    }
    
    @Override
    public void componentShowing() {
        //refresh();
    }

    @Override
    public void componentHidden() {
    }

    @Override
    public void componentActivated() {
//        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void componentDeactivated() {
       // throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public UndoRedo getUndoRedo() {
        return null;
        //throw new UnsupportedOperationException("Not supported yet.");
    }        
    
    protected void updateName() {
        Mutex.EVENT.readAccess(this);
    }    
    
    public void run() {
        m_dObj.updateTCName();
    }    
    
    private final FocusListener m_focusListener = new FocusAdapter() { 
        public @Override void focusGained(FocusEvent evt) {
            // Refresh file object when component made active
            final FileObject fo = m_dObj.getPrimaryFile();
            if (fo != null) {
                RequestProcessor.getDefault().post(new Runnable() {
                    public void run() {
                        fo.refresh();
                    }
                });
            }
        }
    };                
}
