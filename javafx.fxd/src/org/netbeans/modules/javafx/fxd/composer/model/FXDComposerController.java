/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.model;

import com.sun.scenario.scenegraph.JSGPanel;
import com.sun.scenario.scenegraph.SGNode;
import com.sun.scenario.scenegraph.fx.FXNode;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import javax.swing.SwingUtilities;
import org.openide.util.Utilities;
import org.netbeans.modules.javafx.fxd.composer.model.actions.ActionController;
import org.netbeans.modules.javafx.fxd.composer.model.actions.ComposerAction;
import org.netbeans.modules.javafx.fxd.composer.model.actions.SelectActionFactory;
import org.netbeans.modules.javafx.fxd.composer.preview.PreviewElement;
import org.netbeans.modules.javafx.fxd.composer.preview.PreviewTopComponent;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;

/**
 *
 * @author Pavel Benes
 */
public final class FXDComposerController {
    private final FXZDataObject       m_dObj;
    private final ActionController    m_actionController;
    private final List<String>        m_busyStates   = new ArrayList<String>(4);
    private       boolean             m_busyCursorOn = false;
    private       PreviewTopComponent m_previewTopComponent = null; 
    private volatile short            m_screenChangeTicker = 0;
    
    //private       JSGPanel         m_sgPanel      = null;
    //private       StatusBar        m_statusBar = null;   
                
    public FXDComposerController( FXZDataObject dObj) {
        m_dObj = dObj;
        m_actionController = new ActionController(dObj);
    }
        
    public synchronized void init() {
        //System.err.println("Controller initalized.");
    }
    
    public synchronized void close() {
    }
    
    public ActionController getActionController() {
        return m_actionController;
    }
        
    public SelectActionFactory getSelectionModel() {
        return m_actionController.getSelectionModel();
    }
        
    public void setBusyState( String key, boolean isBusy) {
        synchronized( m_busyStates) {
            if ( m_busyStates.contains(key)) {
                if ( !isBusy) {
                    m_busyStates.remove(key);
                }
            } else {
                if ( isBusy) {
                    m_busyStates.add(key);
                }
            }

            final boolean busyCursorOn = !m_busyStates.isEmpty();
            if (m_busyCursorOn != busyCursorOn) {
                final Component comp = getSGPanel();

                if ( comp != null) {
                    SwingUtilities.invokeLater( new Runnable() {
                        public void run() {
                            Cursor cursor = busyCursorOn ? Utilities.createProgressCursor(comp) : null;
                            comp.setCursor(cursor);
                        }
                    });
                }
            }
            m_busyCursorOn = busyCursorOn;
        }
    }
    
    public boolean isBusy() {
        synchronized( m_busyStates) {
            return !m_busyStates.isEmpty();
        }
    }   
    
    public PreviewTopComponent getPreviewComponent() {
        return m_previewTopComponent;        
    }
    
    public void setPreviewComponent( final PreviewTopComponent previewTopComponent) {
        assert previewTopComponent != null;
        m_previewTopComponent = previewTopComponent;
    }
                   
    public JSGPanel getSGPanel() {
        return m_previewTopComponent.getJSGPane();
    }
       
    public SGNode getNode(final String id) {
        SGNode root = getRootNode();
        if (root != null) {
            return root.lookup(id);
        } else {
            return null;
        }
    }
        
    protected boolean hasPreviewTC() {
        return m_previewTopComponent != null;
    }
    
    protected SGNode getRootNode() {
        JSGPanel panel = getSGPanel();
        if (panel != null) {
            return panel.getScene();
        } else {
            return null;
        }        
    }
    
    private static final List<FXDElement> NO_ELEMENTS = new ArrayList<FXDElement>(0);
    
    public List<FXDElement> getElementsAt( int x, int y) {
        List<FXDElement> elems = NO_ELEMENTS;

        SGNode root = getRootNode();
        if (root != null) {
            List<SGNode> nodes = root.pick(new Point2D.Float(x,y));
            elems = new ArrayList<FXDElement>(nodes.size());
            
            for (SGNode node : nodes) {
                String id;                
                if ( (id=node.getID()) != null) {
                   elems.add( new FXDElement(m_dObj, id));
                }
            }
        } 
        
        //System.err.println(String.format( "Elements at [%d,%d]: %s", x, y, elems.toString()));
        return elems;
    }
    
    public void repaint() {
//        m_imageContainer.setTryPaint();
        m_previewTopComponent.repaint();
    }
    
    public void refresh() {
        m_previewTopComponent.refresh();
        repaint();
    }
    
    public short getScreenChangeTicker() {
        return m_screenChangeTicker;
    }
    
    public void setZoomRatio(float zoomRatio) {
        if ( m_dObj.getDataModel().setZoomRatio(zoomRatio)) {
            m_screenChangeTicker++;
            if ( hasPreviewTC()) {
                FXNode scene = (FXNode) getSGPanel().getScene();
                scene.setScaleX(zoomRatio);
                scene.setScaleY(zoomRatio);
                refresh();
            }
        }
    }

    public boolean setIsTooltipOn( boolean isTooltipOn) {
        boolean wasChanged;
        if ( (wasChanged=m_dObj.getDataModel().setIsTooltipOn(isTooltipOn))) {
            refresh();
        }
        return wasChanged;
    }
    
    public boolean setIsHighlightOn(boolean isHighlightOn) {
        boolean wasChanged;
        if ( (wasChanged=m_dObj.getDataModel().setIsHighlightOn(isHighlightOn))) {
            refresh();
        }
        return wasChanged;
    }
    
    public void paintActions( Graphics g) {
        Stack<ComposerAction> actions = m_actionController.getActiveActions();
        synchronized( actions) {
            for (int i = actions.size()-1; i >= 0; i--) {
                actions.get(i).paint(g);
            }
        }
    }    
}
