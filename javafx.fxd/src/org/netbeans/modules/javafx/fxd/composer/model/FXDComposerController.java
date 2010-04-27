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

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import javax.swing.SwingUtilities;

import org.openide.util.Utilities;
import org.netbeans.modules.javafx.fxd.composer.model.actions.ActionController;
import org.netbeans.modules.javafx.fxd.composer.model.actions.ComposerAction;
import org.netbeans.modules.javafx.fxd.composer.model.actions.SelectActionFactory;
import org.netbeans.modules.javafx.fxd.composer.preview.PreviewTopComponent;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;


import com.sun.javafx.tools.fxd.loader.Profile;
import java.util.Collection;
import javafx.fxd.FXDNode;
import javafx.scene.Node;
import javafx.scene.Scene;
import javax.swing.JComponent;
import org.netbeans.modules.javafx.fxd.composer.source.SourceElement;

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
                final Component comp = getScenePanel();

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
    
    private SourceElement m_sourceElement;
    
    public void setSourceElement( final SourceElement sourceElement) {
        m_sourceElement = sourceElement;
    }
    
    public void setPreviewComponent( final PreviewTopComponent previewTopComponent) {
        assert previewTopComponent != null;
        m_previewTopComponent = previewTopComponent;
    }
                   
    public Scene getScene() {
        return m_previewTopComponent.getScene();
    }

    public JComponent getScenePanel() {
        return m_previewTopComponent.getScenePanel();
    }

    public Node getNode(final String id) {
        Scene scene = getScene();
        return scene != null ? scene.lookup(id) : null;
    }
        
    protected boolean hasPreviewTC() {
        return m_previewTopComponent != null;
    }

    /*
    public FXDElement getElementAt( int x, int y) {
        FXDElement elem = null;

        SGNode root = getRootNode();
        if (root != null) {
            List<SGNode> selected = new ArrayList<SGNode>();
            String idPrefix = FXDFileModel.createIdPrefix(m_dObj.getEntryName());

            Affine2D parentTX = new Affine2D();
            getScenePanel().getSceneGroup().getTransformMatrix(parentTX);

            Affine2D currTx = new Affine2D();
            root.getTransformMatrix(currTx);

            parentTX.concatenate(currTx);
            pick( (SGGroup) root, selected, x, y, idPrefix, null, parentTX);

            String id;
            int selNum;
//            System.out.println("*******************************************");
//            for ( SGNode node : selected) {
//                System.out.println("\t '" + node.getID() + "' " + node);
//            }
            if ( (selNum=selected.size()) > 0 &&
                 (id=selected.get(selNum-1).getID()) != null &&
                 id.length() > 0) {
                elem = new FXDElement(m_dObj, id);
            }
        }

        return elem;
    }
     */

    /*
    private static void pick( SGGroup parent, List<SGNode> selected, float x, float y, 
            String idPrefix, SGNode significantParent, Affine2D accumTx)
    {
        for ( PGNode pgChild : parent.getContent()) {
            SGNode child = (SGNode) pgChild;
            String id = child.getID();
            SGNode significantNode = (id != null && id.startsWith(idPrefix)) ? child : significantParent;
//            if ( child instanceof FXNode) {
//                child = ((FXNode) child).getLeaf();
//                // workaround since the leaf ID is not set
//                if ( child.getID() == null) {
//                    child.setID(id);
//                }
//            }
            if( child instanceof SGGroup) {
                Affine2D parentTX = new Affine2D(accumTx);
                Affine2D temp = new Affine2D();
                child.getTransformMatrix(temp);
                parentTX.concatenate(temp);
                pick( (SGGroup) child, selected, x, y, idPrefix, significantNode, parentTX);
            } else {
                //if ( child.getTransformedBounds().contains(point) && significantChild != null) {
                Bounds2D bounds = new Bounds2D();
                child.getCompleteBounds(bounds, accumTx);
                if ( bounds.contains( x, y) && significantNode != null) {
//                if ( child.contains(x, y) && significantNode != null) {
                    selected.add(significantNode);
                }
            }
        }
    }
     */
    
    public FXDElement getElementAt(int x, int y) {
        FXDElement element = null;
        float zoom = m_dObj.getDataModel().getZoomRatio();
        Collection nodes = getScene().impl_pick(x / zoom, y / zoom);
        /* select the first FXDNode. If no FXDNodes, then the first Node */
        Node node = null;
        for (Object o : nodes) {
            if (o instanceof FXDNode){
                node = (Node)o;
                break;
            }
            if (node == null && o instanceof Node){
                node = (Node)o;
            }
        }
        if (node != null) {
            String id = node.get$id();
            if (id != null && id.length() > 0) {
                element = new FXDElement(m_dObj, id);
            }
        }
        return element;
    }

    public void repaint() {
//        m_imageContainer.setTryPaint();
        if ( m_previewTopComponent != null) {
            m_previewTopComponent.repaint();
        }
    }
    
    public void refresh() {
        if ( m_previewTopComponent != null) {
            m_previewTopComponent.refresh();
        }
        
        if ( m_sourceElement != null) {
            m_sourceElement.refresh();
        }
        repaint();
    }
    
    public short getScreenChangeTicker() {
        return m_screenChangeTicker;
    }
    
    public void setZoomRatio(float zoomRatio) {
        if ( m_dObj.getDataModel().setZoomRatio(zoomRatio)) {
            m_screenChangeTicker++;
            if ( hasPreviewTC()) {
//                FXNode scene = (FXNode) getSGPanel().getScene();
//                scene.setScaleX(zoomRatio);
//                scene.setScaleY(zoomRatio);
                refresh();
            }
        }
    }

    public void setPreviewProfile(Profile profile) {
        if ( m_dObj.getDataModel().setPreviewProfile(profile)) {
            m_screenChangeTicker++;
            if ( hasPreviewTC()) {
                refresh();
            }
        }
    }

    public void setSelectedEntry( String entryName) {
        if ( m_dObj.getDataModel().setSelectedEntry(entryName)) {
            m_dObj.updateEditorCookie();
            m_screenChangeTicker++;
            refresh();
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
