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

package org.netbeans.modules.javafx.fxd.composer.preview;


import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.LayoutManager;
import java.awt.Point;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.openide.util.NbBundle;

/**
 *
 * @author Pavel Benes
 * @author ads
 */
final class ImageHolder extends JPanel {
    public static final int CROSS_SIZE = 10;

    private final JComponent    m_imagePanel;
    private final AtomicBoolean canPaint ;
    private final JComponent    myErrorComponent;
    private  final FXZDataObject m_dObj;
    
    public ImageHolder(JComponent imagePanel, FXZDataObject dObj) {
        this.m_imagePanel = imagePanel;
        this.m_dObj = dObj;
        setLayout( new CenteredLayoutManager());
        add(this.m_imagePanel);
        setDoubleBuffered(true);
        canPaint = new AtomicBoolean( true );
        myErrorComponent = new JLabel( 
                NbBundle.getMessage( ImageHolder.class, "ERR_UnableRender"));  // NOI18N
        myErrorComponent.validate();
        setBackground( Color.WHITE);
    }

    public void setTryPaint(){
        if ( !canPaint.getAndSet( true ) ) {
            System.gc();
            remove( myErrorComponent );
            setLayout( new CenteredLayoutManager() );
            add( m_imagePanel );
        }
    }
    
    @Override
    protected void paintChildren(Graphics g) {
        //TODO Determine what to do with the canPaint stuff
    //    if (canPaint.get()) {
            try {
                doPaintChildren(g);
            } catch (Throwable e) {
                System.err.println("Out of Memory");
                e.printStackTrace();
                canPaint.set( false );
                remove( m_imagePanel );
                setLayout( new LabelLayout() );
                add(  myErrorComponent );
            }
 //       }
      //  else {
     //       super.paintChildren(g);
     //   }
    }

    private void doPaintChildren( Graphics g ) {
        super.paintChildren(g);
        int xOff = m_imagePanel.getX();
        int yOff = m_imagePanel.getY();

        g.setColor(Color.BLACK);
        int w = m_imagePanel.getWidth(),
            h = m_imagePanel.getHeight();

        drawCross( g, xOff - 1, yOff -1);
        drawCross( g, xOff - 1, yOff + h + 1);
        drawCross( g, xOff + w + 1, yOff -1);
        drawCross( g, xOff + w + 1, yOff + h + 1);

        /*
        Rectangle2D clip = new Rectangle2D.Float( xOff, yOff, w, h);
        Rectangle visible = getVisibleRect();
        if (visible != null) {
            clip = visible.createIntersection(clip);
        }
        g.setClip( clip);
         */
    }

    @Override
    public void paintComponent(java.awt.Graphics g) {
        super.paintComponent(g);
        m_dObj.getController().paintActions(g);
    }

    private static void drawCross(Graphics g, int x, int y) {
        g.drawLine( x - CROSS_SIZE, y, x + CROSS_SIZE, y);
        g.drawLine( x, y - CROSS_SIZE, x, y + CROSS_SIZE);                
    }     
    
    private class CenteredLayoutManager implements LayoutManager {        
        public void addLayoutComponent(String name, Component comp) {
            assert m_imagePanel.equals(comp);
        }

        public Dimension preferredLayoutSize(Container parent) {
            return m_imagePanel.getPreferredSize();
        }

        public Dimension minimumLayoutSize(Container parent) {
            return m_imagePanel.getPreferredSize();
        }


        public void layoutContainer(Container parent) {
            Dimension d = m_imagePanel.getPreferredSize();
            //System.out.println("Preferred size: " + d);

            float zoom = m_dObj.getDataModel().getZoomRatio();
            int w = (int)(d.getWidth() * zoom);
            int h = (int)(d.getHeight() * zoom);
            
            m_imagePanel.setSize(w, h);
            m_imagePanel.setLocation( (parent.getWidth() - m_imagePanel.getWidth()) / 2,
                    (parent.getHeight() - m_imagePanel.getHeight()) / 2);
        }

        public void removeLayoutComponent(Component comp) {
            assert m_imagePanel.equals(comp);
        }            
    };
    
    private final class LabelLayout extends FlowLayout {
        private static final long serialVersionUID = 2L;

        @Override
        public void layoutContainer(Container parent) {
            super.layoutContainer( parent );
            Point point = myErrorComponent.getLocation();
            myErrorComponent.setLocation( point.x ,
                    (int)(parent.getHeight() - 
                            myErrorComponent.getPreferredSize().getHeight()) / 2);
        }
    }
}
