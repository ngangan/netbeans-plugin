/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.bindspy;

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
import org.netbeans.modules.javafx.bindspy.BindsModel.BindConnection;
import org.netbeans.modules.javafx.bindspy.BindsModel.BindVariable;
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
