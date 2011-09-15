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

package org.netbeans.modules.visage.fxd.composer.source;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.util.logging.Logger;
import javax.swing.ActionMap;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import org.netbeans.editor.EditorUI;
import org.netbeans.editor.StatusBar;
import org.netbeans.editor.Utilities;
import org.openide.text.CloneableEditorSupport;
import org.openide.util.Lookup;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;
import org.openide.windows.CloneableTopComponent;

/**
 *
 * @author Pavel Benes
 */
public class SourceEditorWrapper extends JPanel implements CloneableEditorSupport.Pane {
    public static final String CELL_ERROR = "error"; // NOI18N
    
    private SourceTopComponent m_delegate = null;

    private final static class EditorLookupWrapper extends JPanel implements Lookup.Provider {
        private final SourceTopComponent m_stc;
        private final JComponent         m_editor;
        private final Lookup             m_lookup;
        
        public EditorLookupWrapper( SourceTopComponent stc) {
            assert stc != null;
            m_stc = stc;
            //force the creation of the editor pane
            m_stc.componentShowing();
            JEditorPane pane = m_stc.getEditorPane();
            if (pane != null) {
                addErrorStatusBarCell(pane);
                Container c = pane;
                Container parent;
                while ((parent = c.getParent()) != m_stc) {
                    c = parent;
                }
                m_editor = (JComponent) c;
            } else {
                Logger.getLogger(SourceEditorWrapper.class.getName()).warning(
                        "UNEXPECTED null returned by CloneableEditor.getEditorPane()");
                JEditorPane p = new JEditorPane();
                p.setEnabled(false);
                m_editor = (JComponent) p;
            }

            m_stc.remove(m_editor);
            assert m_editor != null;
            setLayout(null);
            setBorder(null);
            add(m_editor);
            
            InstanceContent ic = new InstanceContent ();
            m_lookup = new AbstractLookup (ic);
            ic.add ( stc.getEditorSupport());
        }
        
        @Override
        public Dimension getPreferredSize() {
            return m_editor.getPreferredSize();
        }

        protected void showContent() {
            setLocation(0, 0);
            setVisible(true);
            findEditorPane(m_editor).requestFocusInWindow();
        }

        protected void hideContent() {
            setVisible(false);
            setSize(0, 0);
        }
        
        public Lookup getLookup() {
            return m_lookup;
        }
        
        @Override
        public void setSize( int w, int h) {
            m_editor.setSize(w, h);
            super.setSize( w, h);
        }  
        
        private static JEditorPane findEditorPane(Component component) {
            JEditorPane result = null;
            
            if ( component instanceof JEditorPane) {
                result = (JEditorPane) component;
            } else if ( component instanceof Container) {
                for ( Component c : ((Container)component).getComponents()) {
                    if ( (result=findEditorPane(c)) != null) {
                        break;
                    }
                }
            } 
            return result;
        }
    }
    
    public SourceEditorWrapper() {
        setLayout( null);
    }
    
    private EditorLookupWrapper findWrapper( SourceTopComponent tc, boolean hideOthers) {
        Component [] comps = getComponents();
        
        EditorLookupWrapper wrapper = null;
        for ( Component c : comps) {
            EditorLookupWrapper w = (EditorLookupWrapper) c;
            if ( w.m_stc == tc) {
                wrapper = w;
            } else if ( hideOthers) {
                w.hideContent();
            }
        }
        return wrapper;
    }
    
    public void wrap( SourceTopComponent tc) {
        m_delegate = tc;
        EditorLookupWrapper wrapper = findWrapper(tc, true);        
        if ( wrapper == null) {
            add( wrapper = new EditorLookupWrapper(tc));
        }
        copyActionMap(tc, this);

        wrapper.showContent();
        wrapper.invalidate();
        validate();
        repaint();
    }

    private void copyActionMap(JComponent source, JComponent target) {
        ActionMap from = source.getActionMap();
        ActionMap to = target.getActionMap();
        for (Object key : from.keys()) {
            to.put(key, from.get(key));
        }
    }

    @Override
    public Dimension getPreferredSize() {
        return m_delegate == null ? null : m_delegate.getPreferredSize();
    }
    
    public JEditorPane getEditorPane() {
        return m_delegate.getEditorPane();
    }

    public CloneableTopComponent getComponent() {
        return m_delegate.getComponent();
    }

    public void updateName() {
        m_delegate.updateName();
    }

    public void ensureVisible() {
        setVisible(true);
        m_delegate.ensureVisible();
    }
    
    @Override
    public void setPreferredSize( Dimension dim) {
        super.setPreferredSize(dim);
    }
    
    @Override
    public void setSize( Dimension dim) {
        // an attempt to fix http://visage-jira.kenai.com/browse/RT-4600
        EditorLookupWrapper elw = findWrapper(m_delegate, false);
        if ( elw != null) {
            elw.setSize(dim.width, dim.height);
        }
        super.setSize(dim);
    }

    @Override
    public void setBounds( Rectangle r) {
        // an attempt to fix http://visage-jira.kenai.com/browse/RT-4600
        EditorLookupWrapper elw = findWrapper(m_delegate, false);
        if ( elw != null) {
            elw.setSize(r.width, r.height);
        }
        super.setBounds(r);
    }

    @Override
    public void setBounds( int x, int y, int w, int h) {
        // an attempt to fix http://visage-jira.kenai.com/browse/RT-4600
        EditorLookupWrapper elw = findWrapper(m_delegate, false);
        if ( elw != null) {
            elw.setSize(w, h);
        }
        super.setBounds(x, y, w, h);
    }    
    
    public static void addErrorStatusBarCell( JEditorPane pane) {
        EditorUI eui = Utilities.getEditorUI(pane);
        StatusBar sBar = eui == null ? null : eui.getStatusBar();
        if (sBar != null && sBar.getCellByName(CELL_ERROR) == null) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < 60; i++) {
                sb.append( 'A');
            }
            sBar.addCell( CELL_ERROR, new String [] { sb.toString()});
        }
    }    
}
