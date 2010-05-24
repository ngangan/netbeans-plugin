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
package org.netbeans.modules.javafx.fxd.composer.source;

import java.awt.Dimension;
import java.awt.Rectangle;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Map;
import java.util.HashMap;

import javax.swing.*;

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
public final class SourceElement implements MultiViewElement, Serializable {
    private static final long serialVersionUID = 2L;
        
    private final     FXZDataObject                   m_dObj;
    private           String                          m_entryCached;
    private transient SourceEditorWrapper             m_editorWrapper;
    private transient ToolbarWrapper                  m_toolbarWrapper;
    private transient Map<String, SourceTopComponent> m_sourceTCs = null;
    private transient SourceTopComponent              m_currentSourceTC = null;

    private static final class ToolbarWrapper extends JPanel {
        private JToolBar  m_delegate = null;
        private Dimension m_dim      = null;
        
        public ToolbarWrapper() {
            setLayout(null);
            setBorder(null);
        }
        public void wrap(JToolBar delegate) {
            
            if ( delegate != m_delegate) {
                removeAll();
                m_delegate = delegate;
                m_delegate.setBorder(null);
                add( m_delegate);
                m_delegate.setLocation(0, 0);
                if ( m_dim != null) {
                    m_delegate.setSize(m_dim);
                }
                m_delegate.setVisible(true);
                m_delegate.invalidate();
                validate();
                repaint();            
            }
        }
                
        @Override
        public void setSize( Dimension dim) {
            m_dim = dim;
            m_delegate.setSize(dim.width, dim.height);
            super.setSize(dim);
        }

        @Override
        public void setBounds( Rectangle r) {
            m_dim = new Dimension( r.width, r.height);
            m_delegate.setSize(r.width, r.height);
            super.setBounds(r);
        }

        @Override
        public void setBounds( int x, int y, int w, int h) {
            m_dim = new Dimension( w, h);
            m_delegate.setSize(w, h);
            super.setBounds(x, y, w, h);
        }

        @Override
        public Dimension getPreferredSize() {
            return m_delegate.getPreferredSize();
        }
    }
    
    SourceElement(final FXZDataObject dObj)  {
        m_dObj = dObj;
    }
    
    private SourceTopComponent getSourceTC() {
        String entryName = m_dObj.getEntryName();
        if (m_sourceTCs == null){
            m_sourceTCs = new HashMap<String, SourceTopComponent>();
        }
        SourceTopComponent stc = m_sourceTCs.get( entryName);
        
        boolean wasCreated = false;
        if (stc == null) {
            stc = new SourceTopComponent(m_dObj);
            //System.out.println("Creating STC for " + entryName + ":" + stc);
            m_sourceTCs.put( entryName, stc);
            wasCreated = true;
        }
        
        if ( m_currentSourceTC != stc) {
            if ( m_currentSourceTC != null) {
                m_currentSourceTC.componentDeactivated();
                m_currentSourceTC.componentHidden();
            }
            m_currentSourceTC = stc;
            if ( wasCreated) {
                m_currentSourceTC.componentOpened();
            }
            //m_currentSourceTC.componentShowing();
        } 
        return stc;
    }     
    
    public JComponent getVisualRepresentation() {
        if ( m_editorWrapper == null) {
            m_editorWrapper = new SourceEditorWrapper();
            m_editorWrapper.wrap( getSourceTC());
        }
        return m_editorWrapper;
    }

    public void refresh() {
        SwingUtilities.invokeLater( new Runnable() {
            public void run() {
                SourceTopComponent stc = getSourceTC();
                stc.componentActivated();
                if ( m_editorWrapper != null) {
                    m_editorWrapper.wrap(stc);
                }
                if ( m_toolbarWrapper != null) {
                    m_toolbarWrapper.wrap(stc.getToolbar());
                }
                stc.refresh();
            }
        });
    }
    
    public synchronized JComponent getToolbarRepresentation() {
        if ( m_toolbarWrapper == null) {
            m_toolbarWrapper = new ToolbarWrapper();
            m_toolbarWrapper.wrap( getSourceTC().getToolbar());
        }
        return m_toolbarWrapper;
    }

    public Action[] getActions() {
        Action[] acts = getSourceTC().getActions();
        return FXZEditorSupport.removeCloneAction(acts);
    }

    public Lookup getLookup() {
        return getSourceTC().getLookup();
    }
    
    public void componentOpened() {
        getSourceTC().componentOpened();
    }

    public void componentClosed() {
        if ( m_currentSourceTC != null) {
            m_currentSourceTC.componentClosed();
        }
        synchronized(this) {
            if (m_sourceTCs != null) {
                m_sourceTCs.clear();
            }
            m_currentSourceTC = null;
            m_editorWrapper = null;
            m_toolbarWrapper = null;
        }
    }

    public void componentShowing() {
        m_dObj.getController().setSourceElement(this);
        getSourceTC().componentShowing();
        refresh();
    }

    public void componentHidden() {
        m_dObj.getController().setSourceElement(null);
        if ( m_currentSourceTC != null) {
            m_currentSourceTC.componentHidden();
        }
    }

    public void componentActivated() {
        getSourceTC().componentActivated();
    }

    public void componentDeactivated() {
        if ( m_currentSourceTC != null) {
            m_currentSourceTC.componentDeactivated();
        }
    }

    public UndoRedo getUndoRedo() {
        //do not create SourceTopComponent just because of undo/redo
        return m_dObj.getEditorSupport().getUndoRedoManager();
    }

    public void setMultiViewCallback(MultiViewElementCallback callback) {
        m_dObj.setMultiviewElementCallback(callback);
        getSourceTC().updateName();
    }

    public CloseOperationState canCloseElement() {
        return MultiViewFactory.createUnsafeCloseState(
            "ID_FXZ_CLOSING", // NOI18N
            MultiViewFactory.NOOP_CLOSE_ACTION,
            MultiViewFactory.NOOP_CLOSE_ACTION);
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
