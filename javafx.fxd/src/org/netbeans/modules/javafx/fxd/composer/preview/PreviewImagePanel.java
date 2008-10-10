/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.preview;

import com.sun.scenario.scenegraph.JSGPanel;
import com.sun.scenario.scenegraph.fx.FXNode;
import java.awt.AWTEvent;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import javax.swing.Action;
import javax.swing.JLabel;
import javax.swing.JPanel;
import org.netbeans.modules.javafx.fxd.composer.misc.ActionLookup;
import org.netbeans.modules.javafx.fxd.composer.misc.ActionLookupUtils;
import org.netbeans.modules.javafx.fxd.composer.model.actions.AbstractFXDAction;
import org.netbeans.modules.javafx.fxd.dataloader.FXDDataObject;
import javafx.fxd.Loader;

/**
 *
 * @author Pavel Benes
 */
final class PreviewImagePanel extends JPanel implements ActionLookup {
    private static final float       ZOOM_STEP = (float) 1.1;
    
    private final FXDDataObject      m_dObj;
    private final Action []          m_actions;
    private       JSGPanel           m_sgPanel = null;
    //TODO Implement the ZoomToFit action
    //private final ZoomToFitAction m_zoomToFitAction;
    private       int                m_changeTickerCopy = -1;
        
    PreviewImagePanel(final FXDDataObject dObj) {
        m_dObj = dObj;
    
        m_actions = new Action[] {
            new ZoomToFitAction(),
            new ZoomInAction(),
            new ZoomOutAction()
        };
        
        setLayout(new BorderLayout());
    }
    
    public JSGPanel getJSGPanel() {
        return m_sgPanel;
    }
    
    synchronized void refresh() {
        final int tickerCopy = m_dObj.getDataModel().getFXDContainer().getChangeTicker();
        if (  tickerCopy != m_changeTickerCopy) {
            removeAll();
            m_sgPanel  = null;

//            JavaFXPlatform jfxpf = JavaFXPlatform.getDefaultFXPlatform();
//            System.err.println("JavaFX platform: " + jfxpf.getJavaFXFolder());
                    
            try {
                //Object obj = Loader.get().load(m_dObj.getDataModel().getFXDContainer());
                System.err.println("Recreating the FXD image ...");
                /*
                FXDContainer fxdContainer = m_dObj.getDataModel().getFXDContainer();
                ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
                Class class_Loader = classLoader.loadClass("javafx.fxd.Loader");
                Method  method_get  = class_Loader.getDeclaredMethod("get");
                Object  loader = method_get.invoke( null);
                Method  method_load  = class_Loader.getDeclaredMethod("load", FXDContainer.class);
                Object  node = method_load.invoke(loader, fxdContainer);
                
                Method  method_impl_getFXNode  = node.getClass().getDeclaredMethod("impl_getFXNode");
                //Object   group  = m.invoke(fxNode);
                FXNode fxNode = (FXNode) method_impl_getFXNode.invoke(node); 
                 */
                
                javafx.scene.Node$Intf node = Loader.get().load(m_dObj.getDataModel().getFXDContainer());
                //Method   m      = fxNode.getClass().getDeclaredMethod("getSGGroup");
                //Object   group  = m.invoke(fxNode);
                FXNode fxNode = node.impl_getFXNode();  
                
                if (fxNode != null) {
                    m_sgPanel = new JSGPanel() {
                        @Override
                        public void paintComponent(Graphics g) {
                            super.paintComponent(g);
                            m_dObj.getController().paintActions(g);
                        }
                    };
                    m_sgPanel.setBackground(Color.WHITE);
//                    FXNode fNode = new FXNode(sgNode);
                    m_sgPanel.setScene( fxNode);
                    
                    add( new ImageHolder(m_sgPanel), BorderLayout.CENTER);

                    MouseEventCollector mec = new MouseEventCollector();
                    m_sgPanel.addMouseListener(mec);
                    m_sgPanel.addMouseMotionListener(mec);
                    m_sgPanel.addMouseWheelListener(mec);
                    
                    m_changeTickerCopy = tickerCopy;
                } else {
                    add( new JLabel("Cannot show"));
                }
            } catch( Exception e) {
                e.printStackTrace();
                add( new JLabel( e.getLocalizedMessage()), BorderLayout.CENTER);
            }
            
            System.gc();
        }
        
        float zoom = m_dObj.getDataModel().getZoomRatio();
        FXNode fxNode = (FXNode) m_sgPanel.getScene();
        fxNode.setScaleX(zoom);
        fxNode.setScaleY(zoom);
        m_sgPanel.invalidate();
    }
    
    final class ZoomToFitAction extends AbstractFXDAction {
        private static final long serialVersionUID = 1L;

        ZoomToFitAction() {
            super("zoom_fit"); //NOI18N
        }

        public void actionPerformed(ActionEvent e) {
            //TODO Implement
            /*
            ScreenManager smgr = getScreenManager();
            Rectangle imgBounds = smgr.getImageBounds();
            Rectangle panelBounds = smgr.getComponent().getBounds();

            float zoomRatio = Math.min((float) (panelBounds.width - 2 * SVGImagePanel.CROSS_SIZE) / imgBounds.width, (float) (panelBounds.height - 2 * SVGImagePanel.CROSS_SIZE) / imgBounds.height);
            smgr.setZoomRatio(zoomRatio * smgr.getZoomRatio());
            updateZoomCombo();
             */
        }
    }
    
    final class ZoomInAction extends AbstractFXDAction {
        private static final long serialVersionUID = 1L;

        ZoomInAction() {
            super("zoom_in"); //NOI18N
        }

        public void actionPerformed(ActionEvent e) {
            float zoom = m_dObj.getDataModel().getZoomRatio() * ZOOM_STEP;
            m_dObj.getController().setZoomRatio(zoom);
        }
    }

    final class ZoomOutAction extends AbstractFXDAction {
        private static final long serialVersionUID = 1L;

        ZoomOutAction() {
            super("zoom_out"); //NOI18N
        }

        public void actionPerformed(ActionEvent e) {
            float zoom = m_dObj.getDataModel().getZoomRatio() / ZOOM_STEP;
            m_dObj.getController().setZoomRatio(zoom);
        }
    }
    
    private final class MouseEventCollector implements MouseListener, MouseMotionListener, MouseWheelListener {
        public void mouseClicked(MouseEvent e) {
            processEvent(e);
        }

        public void mousePressed(MouseEvent e) {
            processEvent(e);
        }

        public void mouseReleased(MouseEvent e) {
            processEvent(e);
        }

        public void mouseEntered(MouseEvent e) {
            processEvent(e);
        }

        public void mouseExited(MouseEvent e) {
            processEvent(e);
            getStatusBar().setText(PreviewStatusBar.CELL_POSITION, "[-,-]");
        }

        public void mouseDragged(MouseEvent e) {
            processEvent(e);
        }

        public void mouseMoved(MouseEvent e) {
            processEvent(e);
            float zoom = m_dObj.getDataModel().getZoomRatio();
            
            getStatusBar().setText( PreviewStatusBar.CELL_POSITION, String.format("[%d,%d]", Math.round(e.getX()/zoom), Math.round(e.getY()/zoom)));
        }

        public void mouseWheelMoved(MouseWheelEvent e) {
            processEvent(e);
        }
        
        protected void processEvent( AWTEvent event) {
            m_dObj.getController().getActionController().processEvent(event);
        }
    }    
    
    public Action get(final Class clazz) {
        return ActionLookupUtils.get(m_actions, clazz);
    }
    
    protected PreviewStatusBar getStatusBar() {
        return m_dObj.getController().getPreviewComponent().getStatusBar();
    }
}
