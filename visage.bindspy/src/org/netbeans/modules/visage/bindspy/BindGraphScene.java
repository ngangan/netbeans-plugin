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


package org.netbeans.modules.visage.bindspy;

import java.io.File;
import javax.swing.JEditorPane;
import org.netbeans.api.visual.action.ActionFactory;
import org.netbeans.api.visual.action.EditProvider;
import org.netbeans.api.visual.action.WidgetAction;
import org.netbeans.api.visual.anchor.Anchor;
import org.netbeans.api.visual.anchor.AnchorFactory;
import org.netbeans.api.visual.anchor.AnchorShape;
import org.netbeans.api.visual.border.BorderFactory;
import org.netbeans.api.visual.graph.GraphScene;
import org.netbeans.api.visual.layout.LayoutFactory;
import org.netbeans.api.visual.widget.ConnectionWidget;
import org.netbeans.api.visual.widget.LayerWidget;
import org.netbeans.api.visual.widget.Widget;
import org.netbeans.api.visual.widget.general.IconNodeWidget;
import org.netbeans.modules.visage.bindspy.BindsModel.BindConnection;
import org.netbeans.modules.visage.bindspy.BindsModel.BindVariable;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;

/**
 *
 * @author Michal Skvor <michal.skvor at sun.com>
 */
public class BindGraphScene extends GraphScene<BindVariable, BindConnection> {

    private Widget mainLayer;
    private Widget connectionLayer;

    private WidgetAction moveAction;

    public BindGraphScene() {
        mainLayer = new LayerWidget( this );
        addChild( mainLayer );
        connectionLayer = new LayerWidget( this );
        addChild( connectionLayer );

        moveAction = ActionFactory.createMoveAction();
//        ActionFactory.createRectangularSelectAction(this, intLayer);
    }

    @Override
    protected Widget attachNodeWidget( BindVariable node ) {
        IconNodeWidget widget = new IconNodeWidget( this );
        widget.setLabel( node.getVariableName());
        widget.setBorder( BorderFactory.createLineBorder());

        WidgetAction.Chain actions = widget.getActions();
        actions.addAction( createObjectHoverAction());
        actions.addAction( createSelectAction());
        actions.addAction( ActionFactory.createEditAction( new EditProvider() {

            public void edit( Widget widget ) {
                BindVariable var = (BindVariable)BindGraphScene.this.findObject( widget );

                FileObject fo = FileUtil.toFileObject( new File( var.getURL()));
                try {
                    DataObject dataObject = DataObject.find( fo );
                    if( dataObject != null ) {
                        EditorCookie ec = dataObject.getLookup().lookup( EditorCookie.class );
                        JEditorPane[] panes = ec.getOpenedPanes();
                        panes[0].setCaretPosition((int) var.getStartPosition());
                    }
                } catch( DataObjectNotFoundException e ) {
                    e.printStackTrace();
                }
            }
        } ));
        actions.addAction( moveAction );

        mainLayer.addChild( widget );
        return widget;
    }

    @Override
    protected Widget attachEdgeWidget( BindConnection connection ) {
        ConnectionWidget widget = new ConnectionWidget( this );
        if( connection.getBindDirection() == BindConnection.BIDIRECTIONAL ) {
            widget.setSourceAnchorShape( AnchorShape.TRIANGLE_FILLED );
        }
        widget.setTargetAnchorShape( AnchorShape.TRIANGLE_FILLED );

//        WidgetAction.Chain actions = widget.getActions ();
//        actions.addAction (createObjectHoverAction ());
//        actions.addAction (createSelectAction ());

        connectionLayer.addChild( widget );
        return widget;
    }

    @Override
    protected void attachEdgeSourceAnchor( BindConnection edge, BindVariable oldSourceNode, BindVariable sourceNode ) {
        ConnectionWidget edgeWidget = (ConnectionWidget)findWidget( edge );
        Widget sourceNodeWidget = findWidget( sourceNode );
        Anchor sourceAnchor = AnchorFactory.createRectangularAnchor( sourceNodeWidget );
        edgeWidget.setSourceAnchor( sourceAnchor );
    }

    @Override
    protected void attachEdgeTargetAnchor( BindConnection edge, BindVariable oldTargetNode, BindVariable targetNode ) {
        ConnectionWidget edgeWidget = (ConnectionWidget)findWidget( edge );
        Widget targetNodeWidget = findWidget( targetNode );
        Anchor targetAnchor = AnchorFactory.createRectangularAnchor( targetNodeWidget );
        edgeWidget.setTargetAnchor (targetAnchor);
    }


}
