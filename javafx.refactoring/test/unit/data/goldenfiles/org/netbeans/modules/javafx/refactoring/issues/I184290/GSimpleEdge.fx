/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package mathematics.graph.simple;

import javafx.scene.shape.Line;
import mathematics.graph.GEdge;
import javafx.scene.Group;
import javafx.scene.text.Text;
import javafx.scene.text.Font;
import javafx.scene.paint.Color;

/**
 * @author andromeda
 */
public class GSimpleEdge extends GEdge {


//    public var view:GView = GView.VIEW;
//    public var shape:function():Node;

    //override var view = GView{};
    public var view = GView{};
    public var text: String;

    //override var shape = function() {
    public var shape = function() {
        //println("[simple edge] {vertex1}, {vertex2}");
        //println(this);

        var v1 = vertex1 as GSimpleVertex;
        var v2 = vertex2 as GSimpleVertex;

        Group{

        content: [
             Line{
                startX: bind v1.centerX
                startY: bind v1.centerY
                endX: bind v2.centerX
                endY: bind v2.centerY
                //stroke: Color.ORANGE
                stroke: bind view.stroke
            }
            Text{
                x: bind (v1.centerX + v2.centerX) / 2
                y: bind (v1.centerY + v2.centerY) / 2
                font: Font{ size: 26 }
                content: bind text
                fill: Color.BLUE
            }

            ]
        }

    }

    override function toString() {
        "Edge {vertex1}, {vertex2}"
    }

}
