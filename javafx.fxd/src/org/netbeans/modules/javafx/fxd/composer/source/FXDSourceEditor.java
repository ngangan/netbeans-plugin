/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.source;

import java.awt.Component;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.SwingUtilities;
import javax.swing.text.Document;
import org.netbeans.core.spi.multiview.CloseOperationState;
import org.netbeans.core.spi.multiview.MultiViewElement;
import org.netbeans.core.spi.multiview.MultiViewElementCallback;
import org.netbeans.modules.javafx.fxd.composer.model.FXDElement;
import org.netbeans.modules.javafx.fxd.composer.navigator.SelectionCookie;
import org.netbeans.modules.javafx.fxd.dataloader.FXDDataObject;
import org.netbeans.modules.javafx.fxd.dataloader.FXDEditorSupport;
import org.openide.cookies.EditCookie;
import org.openide.cookies.EditorCookie;
import org.openide.cookies.OpenCookie;
import org.openide.text.CloneableEditor;
import org.openide.text.NbDocument;
import org.openide.util.Lookup;
import org.openide.util.Mutex;
import org.openide.util.lookup.Lookups;
import org.openide.util.lookup.ProxyLookup;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */
public class FXDSourceEditor extends CloneableEditor implements MultiViewElement, Runnable {
    private JComponent m_toolbar = null;
    private static MultiViewElementCallback m_callback;
    
    public FXDSourceEditor( FXDEditorSupport ed) {
        super(ed);
    }

    public JComponent getVisualRepresentation() {
        //System.err.println("Asking for visual");
        return this;
    }

    public JComponent getToolbarRepresentation() {
        if (m_toolbar == null) {
            final JEditorPane editorPane = getEditorPane();
            if (editorPane!= null) {
                final Document doc = editorPane.getDocument();
                if (doc instanceof NbDocument.CustomToolbar) {
                    m_toolbar = ((NbDocument.CustomToolbar) doc).createToolbar(editorPane);
                }
            }
            if (m_toolbar == null) {
                // attempt to create own toolbar??
                m_toolbar = new javax.swing.JPanel();
            }
        }
        return m_toolbar;
    }
    
    public void setMultiViewCallback(MultiViewElementCallback callback) {
        m_callback = callback;
        updateName();
    }
    
    @Override
    public void updateName() {
        Mutex.EVENT.readAccess(this);
    }

    public void run() {
        MultiViewElementCallback c = m_callback;
        if ( c == null) {
            return;
        }
        TopComponent tc = c.getTopComponent();
        if ( tc == null) {
            return;            
        }
        super.updateName();
        tc.setName( this.getName());
        tc.setDisplayName( this.getDisplayName());
        tc.setHtmlDisplayName( this.getHtmlDisplayName());
    }

    @Override
    public Lookup getLookup() {
        return new ProxyLookup(new org.openide.util.Lookup[] {                
            ((FXDDataObject) ((FXDEditorSupport) cloneableEditorSupport()).getDataObject()).getNodeDelegate().getLookup(),
            Lookups.singleton( new SelectionCookie() {
                public void updateSelection(FXDDataObject doj, FXDElement elem, boolean doubleClick) {
                    selectElement(doj, elem.getStartOffset(), doubleClick);
                }
            })
        });
    }    

    @Override    
    public void componentOpened() {
        super.componentOpened();
    }

    @Override
    public void componentClosed() {
        super.componentClosed();
    }

    @Override
    public void componentShowing() {
        super.componentShowing();
    }

    @Override
    public void componentHidden() {
        super.componentHidden();
    }

    @Override
    public void componentActivated() {
        super.componentActivated();
    }

    @Override
    public void componentDeactivated() {
        super.componentDeactivated();
    }

    public CloseOperationState canCloseElement() {
        return CloseOperationState.STATE_OK;
    }
    
    public static void selectElement( final FXDDataObject dDoj, int startOffset, final boolean requestFocus) {
        if ( startOffset != -1) {
            selectPosition(dDoj, startOffset, requestFocus);
        }
    }
    
    public static void selectPosition( final FXDDataObject dObj, final int position, final boolean requestFocus) {
        if ( requestFocus) {
            openFileInEditor(dObj);
        }

        SwingUtilities.invokeLater( new Runnable() {
            @SuppressWarnings({"deprecation"})
            public void run() {
                EditorCookie ed = dObj.getCookie(EditorCookie.class);
                try {
                    if (ed != null) {
                        ed.openDocument();
                        JEditorPane [] opened = ed.getOpenedPanes();
                        if ( opened != null && opened.length > 0) {
                            final JEditorPane  pane = opened[0];
                            m_callback.requestVisible();
                            pane.setSelectionStart(position);
                            pane.setSelectionEnd(position);

                            if ( requestFocus) {
                                System.err.println("Requesting focus ...");
                                m_callback.requestActive();
                                TopComponent tc = (TopComponent) SwingUtilities.getAncestorOfClass( TopComponent.class, pane);
                                if (tc != null) {
                                    tc.requestActive();
                                    // the requestActive itself does not work
                                    tc.requestFocus();
                                }
                                
                            }
                        }
                    } else {
                        System.err.println("Editor cookie not found!");
                    }            
                } catch( Exception e) {
                    e.printStackTrace();
                    //SceneManager.error("Select in editor failed.", e); //NOI18N
                }
            }            
        });        
    }    
        
    private static boolean openFileInEditor(FXDDataObject dObj) {
        EditCookie ck = dObj.getCookie(EditCookie.class);
        if (ck != null) {
            System.err.println("Edit cookie found, editing ...");
            ck.edit();
            return true;
        }

        OpenCookie oc = dObj.getCookie(OpenCookie.class);
        if (oc != null) {
            System.err.println("Open cookie found, opening ...");
            oc.open();
            return true;
        }
        

        return false;
    }        
}
