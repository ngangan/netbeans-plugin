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

package org.netbeans.modules.visage.fxd.composer.navigator;

import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.Iterator;
import java.util.List;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.editor.structure.api.DocumentModel;
import org.netbeans.modules.visage.fxd.composer.model.FXDElement;
import org.netbeans.modules.visage.fxd.composer.model.FXDFileModel;

/**
 *
 * @author Pavel Benes
 */
final class FXDNavigatorTree extends JTree {
    public static byte VISIBILITY_DIRECT     = 1;
    public static byte VISIBILITY_UNDIRECT   = 2;
    public static byte VISIBILITY_NO         = 3;
    
    static boolean showAttributes     = true;
    static boolean showIdOnly         = false;
    
    private final DefaultTreeModel  m_treeModel;    
    private       boolean           m_firstPaint;
    //private       FXDElement        m_selected = null;

    public FXDNavigatorTree(final DocumentModel docModel) {
        super();
        
        m_firstPaint = true;
        setShowsRootHandles(true);
        setRootVisible(true);
        putClientProperty("JTree.lineStyle", "Angled");  //NOI18N       
        
        DocumentElement rootElement = docModel.getRootElement();
        FXDNavigatorNode rootTna = new FXDNavigatorNode(rootElement, this, null, VISIBILITY_DIRECT);

        m_treeModel = new DefaultTreeModel(rootTna);
        setModel(m_treeModel);   
        
        setCellRenderer(new FXDNavigatorTreeCellRenderer());
    }
    
    public DefaultTreeModel getTreeModel() {
        return m_treeModel;
    }
      
    /*
    String getSelectedId() {
        return m_selectedId;
    }*/
    
    public void filterChanged() {
        FXDNavigatorNode root = (FXDNavigatorNode) treeModel.getRoot();
        root.refresh();
        
        //make sure that root's children are visible
        if (root.getChildCount() > 0) {
            TreePath path = new TreePath( root);
            if ( isCollapsed(path)) {
                expandPath(path);
            }            
        }
        repaint();
    }
    
    /** Overriden to calculate correct row height before first paint */
    @Override
    public void paint(Graphics g) {
        if (m_firstPaint) {
            int height = g.getFontMetrics(getFont()).getHeight();
            setRowHeight(height + 2);
            m_firstPaint = false;
        }
        super.paint(g);
    }
    
    public static boolean isTreeElement(DocumentElement de) {
        return !FXDFileModel.isError(de);
    }
        
    public void selectNode( FXDElement elem) {
        //m_selected = elem;
        
        if ( elem == null) {
            getSelectionModel().clearSelection();
            repaint();
        } else {
            DocumentElement de = elem.getDocumentElement();

            if (de != null) {
                FXDNavigatorNode rootNode = (FXDNavigatorNode) treeModel.getRoot();
                FXDNavigatorNode node = rootNode.findNode(de);
                if ( node == null) {
                    List<DocumentElement> parents = FXDFileModel.getParentChain(de);
                    int parentIndex = parents.size() - 1;
                    assert parentIndex >= 0 : "The element must have at least one parent"; //NOI18N
                    node = rootNode.findNode( parents.get(parentIndex));
                    assert node != null : "Tree node not found"; //NOI18N

                    while( parentIndex > 0) {
                        FXDNavigatorNode childNode = node.getChildByElemenent(parents.get(--parentIndex));
                        if ( childNode != null) {
                            node = childNode;
                        } else {
                            break;
                        }
                    }
                }

                TreePath treePath = node.getNodePath();
                makeVisible(treePath); 
                setSelectionPath(treePath);
                Rectangle rect = getPathBounds(treePath);
                scrollRectToVisible( new Rectangle( 0, rect.y, rect.x + rect.width, rect.height));
                repaint();
            } 
        }
    }
    
    byte checkVisibility(DocumentElement docElem, boolean deepCheck) {
        boolean isVisible = isVisible(docElem);
        
        if ( !isVisible && deepCheck) {
            // check if node should be visible because of its visible children
            for ( Iterator i = docElem.getChildren().iterator(); i.hasNext(); ) {  
                DocumentElement chde = (DocumentElement)i.next();    
                  
                if( isTreeElement(docElem) ) {
                    if ( checkVisibility(chde, true) != VISIBILITY_NO) {
                        return VISIBILITY_UNDIRECT;
                    }
                }                
            }
        }
        return isVisible ? VISIBILITY_DIRECT : VISIBILITY_NO;
    }    
    
    private static boolean isVisible(DocumentElement docElem) {
        if (showIdOnly) {
            if ( !FXDFileModel.isSignificant(docElem)) {
                return false;
            }
        }

        return true;
    }
}
