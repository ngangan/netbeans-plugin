/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package mathematics.graph.simple;

import mathematics.graph.GVertex;
import javafx.scene.shape.Circle;

import javafx.scene.Group;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import mathematics.graph.GView;


/**
 * @author andromeda
 */
public class GSimpleVertex extends GVertex {

    public var view:GView = GView.VIEW;
//    public var shape:function():Node;


    public var n: Integer;
    public var centerX: Number;
    public var centerY: Number;
    public var radius: Number = 15;

    //public var text:String = bind "{n}";


    //override var shape = function() {
    public var shape = function() {
        var cx = 0.0;
        var cy = 0.0;

        Group {
            translateX: bind centerX
            translateY: bind centerY
            content: [
                Circle {
                    //centerX: bind centerX
                    //centerY: bind centerY
                    radius: bind radius
                    fill: bind view.fill
                    stroke: bind view.stroke
                }
                Text {
                    x: -5 y: 5
                    content: bind "{n}"
                    fill: bind view.textFill
                    font: Font {size: 16 }
                }
            ]

            onMousePressed: function (e): Void {
                cx = e.sceneX;
                cy = e.sceneY;
            }

            onMouseDragged: function (e): Void {
                //centerX += e.dragX;
                //centerY += e.dragY;
                centerX += e.sceneX - cx;
                centerY += e.sceneY - cy;
                cx = e.sceneX;
                cy = e.sceneY;
                //centerX = cx + e.dragX;
                //centerY = cy + e.dragY;
            }
        }
    }

    override function toString() {
        "Vertex ( {n}: {centerX}, {centerY})"
    }

}
