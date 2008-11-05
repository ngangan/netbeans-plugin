/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.source;

import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.SwingUtilities;
import javax.swing.text.Document;
import org.netbeans.core.spi.multiview.CloseOperationState;
import org.netbeans.core.spi.multiview.MultiViewElement;
import org.netbeans.core.spi.multiview.MultiViewElementCallback;
import org.netbeans.core.spi.multiview.MultiViewFactory;
import org.netbeans.editor.EditorUI;
import org.netbeans.editor.StatusBar;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.javafx.fxd.composer.navigator.SelectionCookie;
import org.netbeans.modules.javafx.fxd.dataloader.FXDZDataObject;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZEditorSupport;
import org.openide.cookies.EditCookie;
import org.openide.cookies.EditorCookie;
import org.openide.cookies.OpenCookie;
import org.openide.loaders.DataObject;
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
    public static final String CELL_ERROR = "error";
    
    private JComponent m_toolbar = null;
    private static MultiViewElementCallback m_callback;
    
    public FXDSourceEditor( FXZEditorSupport ed) {
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

    protected FXZDataObject getDataObject() {
        return (FXZDataObject) ((FXZEditorSupport) cloneableEditorSupport()).getDataObject();
    }
    
    @Override
    public Lookup getLookup() {
        return new ProxyLookup(new org.openide.util.Lookup[] {                
            getDataObject().getNodeDelegate().getLookup(),
            Lookups.singleton( new SelectionCookie() {
                public void updateSelection(FXDZDataObject doj, DocumentElement de, boolean doubleClick) {
                    selectElement(doj, de.getStartOffset(), doubleClick);
                }
            })
        });
    }    

    @Override    
    public void componentOpened() { 
        getDataObject().init();            
        super.componentOpened();
        if ( getDataObject().getDataModel().getFXDContainer() == null) {
            getEditorPane().setEnabled(false);
        }
    }

    @Override
    public void componentClosed() {
        super.componentClosed();
    }

    @Override
    public void componentShowing() {
        super.componentShowing();
        addErrorStatusBarCell( getDataObject());
    }

    public static void addErrorStatusBarCell( final DataObject dObj) {
        SwingUtilities.invokeLater( new Runnable() {
            public void run() {
                EditorCookie ec = dObj.getCookie(EditorCookie.class);
                if ( ec != null) {
                    JEditorPane [] panes = ec.getOpenedPanes();
                    if ( panes != null && panes.length > 0 && panes[0] != null) {
                        EditorUI eui = Utilities.getEditorUI(panes[0]);
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
            }            
        });        
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
        return MultiViewFactory.createUnsafeCloseState(
            "ID_FXZ_CLOSING", // NOI18N
            MultiViewFactory.NOOP_CLOSE_ACTION,
            MultiViewFactory.NOOP_CLOSE_ACTION);
    }
    
    public static void selectElement( final FXDZDataObject dDoj, int startOffset, final boolean requestFocus) {
        if ( startOffset != -1) {
            selectPosition(dDoj, startOffset, requestFocus);
        }
    }
    
    public static void selectPosition( final FXDZDataObject dObj, final int position, final boolean requestFocus) {
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
                            //m_callback.requestVisible();
                            if (position >= 0) {
                                pane.setSelectionStart(position);
                                pane.setSelectionEnd(position);
                            }
                            
                            if ( requestFocus) {
                                //System.err.println("Requesting focus ...");
                                //m_callback.requestActive();
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
        
    private static boolean openFileInEditor(FXDZDataObject dObj) {  
        if ( dObj instanceof FXZDataObject) {
            ((FXZDataObject)dObj).selectView( FXZDataObject.TEXT_VIEW_INDEX);
        }
        
        EditCookie ck = dObj.getCookie(EditCookie.class);
        if (ck != null) {
            //System.err.println("Edit cookie found, editing ...");
            ck.edit();
            return true;
        }

        OpenCookie oc = dObj.getCookie(OpenCookie.class);
        if (oc != null) {
            //System.err.println("Open cookie found, opening ...");
            oc.open();
            return true;
        }
        
        return false;
    }        
}
