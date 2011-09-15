package declarations;

import visage.scene.paint.Color;
import visage.scene.shape.Polyline;
import visage.scene.shape.Polyline;

/** completion after last var should not show Polyline items when ; is missing */

var color : Integer = 0;
var colorSEQ = [Color.BLACK, Color.BLUE, Color.ROYALBLUE, Color.DARKRED];

var leftEyebrow = Polyline {
    points : [ 65,50, 75,45, 80,50 ]
    strokeWidth: 2.0
    stroke: bind colorSEQ[color]
    fill: bind colorSEQ[color]
}


