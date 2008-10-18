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
package org.netbeans.modules.javafx.fxd.composer.model;

import com.sun.scenario.scenegraph.SGNode;
import java.awt.geom.Rectangle2D;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;

/**
 *
 * @author Pavel Benes
 */
public final class FXDElement {
    private final FXZDataObject       m_dObj;
    private final String              m_id;
    private       FXDElementOutline   m_outline   = null;
    private       boolean             m_isDeleted = false;
/*
    private       float               m_translateDx = 0;
    private       float               m_translateDy = 0;
    private       float               m_skewX       = 0;
    private       float               m_skewY       = 0;
    private       float               m_scaleX      = 1;
    private       float               m_scaleY      = 1;
    private       float               m_rotate      = 0;
    
    private       float               m_tempTranslateDx = 0;
    private       float               m_tempTranslateDy = 0;
    private       float               m_tempSkewX       = 0;
    private       float               m_tempSkewY       = 0;
    private       float               m_tempScaleX      = 1;
    private       float               m_tempScaleY      = 1;
    private       float               m_tempRotate      = 0;
*/
    public FXDElement(final FXZDataObject dObj, final String id) {
        assert dObj != null;
        assert id != null;
        
        m_dObj = dObj;
        m_id   = id;
//        SceneManager.log(Level.FINE, "SVGObject created: " + m_elem); //NOI18N        
        System.err.println("CREATED: " + toString());
    }
            
    public boolean isVisible() {
        //TODO use JSONObject to determine this
        return getController().getNode(m_id) != null;
    }
    
    public String getName() {
        return getDocumentElement().getName();
    }
    
    public void visitAttributes( FXDFileModel.ElementAttrVisitor visitor) {
        FXDFileModel.visitAttributes( getDocumentElement(), visitor, true);
    }
    
    //TODO should be private
    public DocumentElement getDocumentElement() {
        return m_dObj.getDataModel().getFXDContainer().getFileModel().getElementById(m_id);
    }
    
    public int getStartOffset() {
        return getDocumentElement().getStartOffset();
    }
    
    public FXDElement getParent() {
        DocumentElement de = getDocumentElement();
        if ( de != null) {
            de = de.getParentElement();
            System.err.println("Parent: " + de); //NOI18N
            if ( de != null) {
                String id = FXDFileModel.getIdAttribute(de);
                return new FXDElement(m_dObj, id);
            }
        }
        return null;
    }
    
    public boolean hasParent() {
        DocumentElement de = getDocumentElement();
        if ( de != null) {
            de = de.getParentElement();
            if ( de != null && !FXDFileModel.DOCUMENT_ROOT_ELEMENT_TYPE.equals(de.getType())) {
                return true;
            }
        }
        return false;
    }
    
    Rectangle2D getBounds() {
        SGNode node = getController().getNode(m_id);
        assert node != null;
        Rectangle2D b = node.getBounds();
        double zoom = m_dObj.getDataModel().getZoomRatio();
        
        return new Rectangle2D.Double( b.getX() * zoom, b.getY() * zoom, 
                                       b.getWidth() * zoom, b.getHeight() * zoom);
    }

    public boolean isDeleted() {
        return m_isDeleted;
    }
    
    protected void repaint(int x, int y, int w, int h) {
        m_dObj.getController().getSGPanel().repaint(x, y, w, h);
    }
    
    public void repaint(double overlap) {
        if ( !m_isDeleted) {
            Rectangle2D rect = getBounds();
            //TODO the attribute elements should not be repainted
            if ( rect != null) {
                repaint( (int) Math.round( rect.getX() - overlap),
                         (int) Math.round(rect.getY() - overlap),
                         (int) Math.round(rect.getWidth() + 2 * overlap),
                         (int) Math.round(rect.getHeight() + 2 * overlap));
            }
        }
    }
    
    public synchronized FXDElementOutline getOutline() {
        if ( m_outline == null) {
            if (isVisible()) {
                m_outline = new FXDElementOutline(this);
            }
        }
        return m_outline;
    }
    
    @Override
    public boolean equals(Object o) {
        if ( o instanceof FXDElement) {
            return ((FXDElement)o).m_id.equals(m_id);
        } else {
            return false;
        }        
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 89 * hash + (m_id != null ? m_id.hashCode() : 0);
        return hash;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append( "FXDElement( id="); //NOI18N
        sb.append( m_id);
        sb.append( " isDeleted="); //NOI18N
        sb.append( m_isDeleted);
        sb.append(")"); //NOI18N

        return sb.toString();
    }
    
    protected final FXDComposerController getController() {
        return m_dObj.getController();
    }
    
    public static boolean areSame(FXDElement [] arr1,FXDElement [] arr2) {
        if (arr1 == arr2) {
            return true;
        } else if (arr1 == null || arr2 == null) {
            return false;
        } else if (arr1.length != arr2.length) {
            return false;
        } else {
            for (int i = 0; i < arr1.length; i++) {
                if ( arr1[i] != arr2[i]) {
                    return false;
                }
            }
            return true;
        }
    }
    
    public static void repaint( final FXDElement [] elems, final int overlap) {
        for (FXDElement elem : elems) {
            elem.repaint(overlap);
        }
    }
}
