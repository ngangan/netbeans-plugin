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
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import com.sun.javafx.tools.fxd.LoaderExtended;
import java.awt.Dimension;
import java.awt.geom.Rectangle2D;
import java.net.URL;
import javax.swing.ImageIcon;
import javax.swing.SwingUtilities;
import org.netbeans.modules.javafx.fxd.composer.model.FXZArchive;
import org.openide.util.NbBundle;


/**
 *
 * @author Pavel Benes
 */
final class PreviewImagePanel extends JPanel implements ActionLookup {
    private static final float       ZOOM_STEP = (float) 1.1;
    
    private final FXZDataObject      m_dObj;
    private final Action []          m_actions;
    private final Color              m_defaultBackground;
    private       JSGPanel           m_sgPanel = null;
    private       int                m_changeTickerCopy = -1;
        
    PreviewImagePanel(final FXZDataObject dObj) {
        m_dObj = dObj;
    
        m_actions = new Action[] {
            new ZoomToFitAction(),
            new ZoomInAction(),
            new ZoomOutAction()
        };
        
        setLayout(new BorderLayout());
        m_defaultBackground = getBackground();                
        setBackground( Color.WHITE);
    }
    
    public JSGPanel getJSGPanel() {
        return m_sgPanel;
    }
    
    protected JLabel createWaitPanel() {
        URL url = PreviewImagePanel.class.getClassLoader().getResource("org/netbeans/modules/javafx/fxd/composer/resources/clock.gif"); //NOI18N
        ImageIcon icon = new ImageIcon( url);
        JLabel label = new JLabel( icon);
        label.setHorizontalTextPosition(JLabel.CENTER);
        label.setVerticalTextPosition( JLabel.BOTTOM);
        return label;        
    }
    
    synchronized void refresh() {
        FXZArchive fxzArchive = m_dObj.getDataModel().getFXDContainer(); 
        if (  fxzArchive != null) {
            final int tickerCopy = fxzArchive.getChangeTicker();
            if ( tickerCopy != m_changeTickerCopy) {
                removeAll();
                setBackground( Color.WHITE);
                final JLabel label = createWaitPanel();
                label.setText( NbBundle.getMessage( PreviewImagePanel.class, "LBL_PARSING")); //NOI18N            

                add( label, BorderLayout.CENTER);
                m_sgPanel  = null;

                Thread th = new Thread() {
                    @Override
                    public void run() {
                        final FXZArchive fxz = m_dObj.getDataModel().getFXDContainer();
                        fxz.getFileModel(false).updateModel();

                        SwingUtilities.invokeLater( new Runnable() {
                            public void run() {
                                label.setText( NbBundle.getMessage( PreviewImagePanel.class, "LBL_RENDERING")); //NOI18N            
                                SwingUtilities.invokeLater( new Runnable() {
                                    public void run() {
                                        try {
                                            javafx.scene.Node$Intf node = LoaderExtended.getLoaderExtended().load(fxz);
                                            //Method   m      = fxNode.getClass().getDeclaredMethod("getSGGroup");
                                            //Object   group  = m.invoke(fxNode);
                                            FXNode fxNode = node.impl_getFXNode();  

                                            if (fxNode != null) {
                                                m_sgPanel = new JSGPanel() {
                                                    @Override
                                                    public void paintComponent(java.awt.Graphics g) {
                                                        super.paintComponent(g);
                                                        m_dObj.getController().paintActions(g);
                                                    }
                                                };
                                                m_sgPanel.setBackground(Color.WHITE);
                            //                    FXNode fNode = new FXNode(sgNode);
                                                m_sgPanel.setScene( fxNode);

                                                removeAll();
                                                add( new ImageHolder(m_sgPanel), BorderLayout.CENTER);

                                                MouseEventCollector mec = new MouseEventCollector();
                                                m_sgPanel.addMouseListener(mec);
                                                m_sgPanel.addMouseMotionListener(mec);
                                                m_sgPanel.addMouseWheelListener(mec);

                                                m_changeTickerCopy = tickerCopy;
                                                updateZoom();
                                            } else {
                                                //TODO 
                                                label.setText("Cannot show");
                                                label.setIcon(null);
                                            }
                                        } catch( Exception e) {
                                            //TODO provide better error report
                                            e.printStackTrace();
                                            label.setText(e.getLocalizedMessage());
                                            label.setIcon(null);
                                        } finally {
                                            System.gc();                                
                                        }
                                    }                            
                                });                            
                            }
                        });                    
                    }
                };
                th.setName("ModelUpdate-Thread");  //NOI18N
                th.start();            
            } else {
                updateZoom();
            }
        } else {
            removeAll();
            Exception error = m_dObj.getDataModel().getFXDContainerLoadError();
            setBackground( m_defaultBackground);
            
            JLabel label = new JLabel( 
                NbBundle.getMessage( PreviewImagePanel.class, "MSG_CANNOT_LOAD", error.getLocalizedMessage()), //NOI18N                                
                JLabel.CENTER);
            add( label, BorderLayout.CENTER);
        }
    }
    
    private void updateZoom() {
        assert m_sgPanel != null;
        float zoom = m_dObj.getDataModel().getZoomRatio();
        FXNode fxNode = (FXNode) m_sgPanel.getScene();
        fxNode.setTranslateX( 0);
        fxNode.setTranslateY( 0);
        fxNode.setScaleX(zoom);
        fxNode.setScaleY(zoom);
        Rectangle2D bounds = fxNode.getTransformedBounds();
        
        fxNode.setTranslateX( -bounds.getX());
        fxNode.setTranslateY( -bounds.getY());
        
        m_sgPanel.invalidate();
    }
    
    final class ZoomToFitAction extends AbstractFXDAction {
        private static final long serialVersionUID = 1L;

        ZoomToFitAction() {
            super("zoom_fit"); //NOI18N
        }

        public void actionPerformed(ActionEvent e) {
            FXNode fxNode = (FXNode) m_sgPanel.getScene();
            Rectangle2D bounds = fxNode.getBoundsInLocal();
            
            Dimension panelSize = getParent().getSize();
            
                        double xRatio = (panelSize.getWidth() - 2 * ImageHolder.CROSS_SIZE) / bounds.getWidth();
            double yRatio = (panelSize.getHeight() - 2 * ImageHolder.CROSS_SIZE) / bounds.getHeight();
            
            m_dObj.getController().setZoomRatio((float) Math.min( xRatio, yRatio));
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
            getStatusBar().setText(PreviewStatusBar.CELL_POSITION, "[-,-]");  //NOI18N
        }

        public void mouseDragged(MouseEvent e) {
            processEvent(e);
        }

        public void mouseMoved(MouseEvent e) {
            processEvent(e);
            float zoom = m_dObj.getDataModel().getZoomRatio();
            
            getStatusBar().setText( PreviewStatusBar.CELL_POSITION, String.format("[%d,%d]", Math.round(e.getX()/zoom), Math.round(e.getY()/zoom))); //NOI18N
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
