/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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

package org.netbeans.modules.javafx.fxd.composer.navigator;

import java.awt.Component;
import java.awt.Image;
import java.util.HashMap;
import java.util.Map;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.javafx.fxd.composer.model.FXDFileModel;
import org.openide.awt.HtmlRenderer;
import org.openide.util.ImageUtilities;


/** TreeCellRenderer implementatin for the XML Navigator.
 *
 * @author Pavel Benes
 * @version 1.0
 */
final class FXDNavigatorTreeCellRenderer extends DefaultTreeCellRenderer { 
    private static final String IMAGE_BASE     = "org/netbeans/modules/javafx/fxd/composer/resources/";  //NOI18N
    private static final String ELEMENT        = IMAGE_BASE + "node_element.png";  //NOI18N
    private static final String NODE_ATTR      = IMAGE_BASE + "node_attr.png";  //NOI18N
    private static final String NODE_GENERAL   = IMAGE_BASE + "node_node.png";  //NOI18N
    private static final String ERROR          = "org/netbeans/modules/xml/text/navigator/resources/badge_error.png"; //NOI18N    
    private static final Image  ERROR_IMAGE    = ImageUtilities.loadImage(ERROR, true);   
    
    private static final Map<String, Icon[]> NODE_ICONS;
    
    static {
        NODE_ICONS = new HashMap<String, Icon[]>();
        NODE_ICONS.put("Group", createIcons(IMAGE_BASE + "node_group.png"));//NOI18N
        NODE_ICONS.put("Path", createIcons(IMAGE_BASE + "node_path.png"));//NOI18N
        NODE_ICONS.put("SVGPath", createIcons(IMAGE_BASE + "node_svgPath.png"));//NOI18N
        NODE_ICONS.put("Rectangle", createIcons(IMAGE_BASE + "node_rectangle.png"));//NOI18N
        NODE_ICONS.put("Text", createIcons(IMAGE_BASE + "node_text.png"));//NOI18N
        NODE_ICONS.put("ImageView", createIcons(IMAGE_BASE + "node_image.png"));//NOI18N
        NODE_ICONS.put("Line", createIcons(IMAGE_BASE + "node_line.png"));//NOI18N
        NODE_ICONS.put("Polygon", createIcons(IMAGE_BASE + "node_polygon.png"));//NOI18N
        NODE_ICONS.put("Polyline", createIcons(IMAGE_BASE + "node_polyline.png"));//NOI18N
        NODE_ICONS.put("Circle", createIcons(IMAGE_BASE + "node_circle.png"));//NOI18N
        NODE_ICONS.put("Ellipse", createIcons(IMAGE_BASE + "node_ellipse.png"));//NOI18N
        NODE_ICONS.put("Arc", createIcons(IMAGE_BASE + "node_arc.png"));//NOI18N
        NODE_ICONS.put("LinearGradient", createIcons(IMAGE_BASE + "node_linearGradient.png"));//NOI18N
        NODE_ICONS.put("RadialGradient", createIcons(IMAGE_BASE + "node_radialGradient.png"));//NOI18N
    }
        
    private final Icon[] NODE_GRAY_ICON    = new Icon[]{getImageIcon(ELEMENT, false), getImageIcon(ELEMENT, true)};
    private final Icon[] NODE_ICON         = createIcons(ELEMENT);
    private final Icon[] ATTR_ICON         = createIcons(NODE_ATTR);
    private final Icon[] NODE_GENERAL_ICON = createIcons(NODE_GENERAL);

    
    private final HtmlRenderer.Renderer renderer;
    
    public FXDNavigatorTreeCellRenderer() {
        super();
        renderer = HtmlRenderer.createRenderer();
        renderer.setHtml(true);
    }
    
    @Override
    public Component getTreeCellRendererComponent(JTree tree, Object value,
        boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
        FXDNavigatorNode tna      = (FXDNavigatorNode)value;
        DocumentElement  de       = tna.getDocumentElement();        
        String           htmlText = tna.getText(true);
        Component        comp     = renderer.getTreeCellRendererComponent(tree, htmlText, sel, expanded, leaf, row, hasFocus);
        
        comp.setEnabled(tree.isEnabled());
        ((JLabel)comp).setToolTipText(tna.getToolTipText().trim().length() > 0 ? tna.getToolTipText() : null);
        
        boolean containsError = tna.getChildrenErrorCount() > 0;
        
        if ( tna.getNodeVisibility() == FXDNavigatorTree.VISIBILITY_UNDIRECT) {
            setIcon( NODE_GRAY_ICON, containsError);
        } else {
            //normal icons
            if( FXDNavigatorTree.isTreeElement(de)) {
                Icon [] icons = null;
                String type = de.getType();
                
                if ( FXDFileModel.FXD_NODE.equals(type)) {
                    icons = NODE_ICONS.get( de.getName());
                } else if ( FXDFileModel.FXD_ATTRIBUTE.equals(type)) {
                    icons = ATTR_ICON;
                } else if ( FXDFileModel.FXD_ATTRIBUTE_ARRAY.equals(type)) {
                    icons = ATTR_ICON;
                } else if ( FXDFileModel.FXD_ARRAY_ELEM.equals(type)) {
                    icons = NODE_GENERAL_ICON;
                } else if ( FXDFileModel.DOCUMENT_ROOT_ELEMENT_TYPE.equals(type)) {
                    icons = NODE_ICON;
                } else {
                    System.err.println("Unknown element type: " + type);  //NOI18N
                }
                if ( icons == null) {
                    icons = NODE_ICON;
                }
                setIcon( icons, containsError);
            }          
        }
        
        return comp;
    }
    
    private void setIcon(Icon[] icons, boolean containsError) {
        renderer.setIcon(icons[containsError ? 1 : 0]);
    }
    
    private static ImageIcon getImageIcon(String name, boolean error){
        ImageIcon icon = null;
        try {
            icon = new ImageIcon(ImageUtilities.loadImage(name));
        } catch( Exception e) {
            System.err.println("Load of " + name + "  failed.");   //NOI18N
            e.printStackTrace();
        }
        
        if(error)
            return new ImageIcon(ImageUtilities.mergeImages( icon.getImage(), ERROR_IMAGE, 15, 7 ));
        else
            return icon;
    }   
    
    private static Icon [] createIcons( String name) {
        return new Icon[] { getImageIcon(name, false), getImageIcon(name, true)};
    }    
}