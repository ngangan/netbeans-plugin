package forms;

import javafx.gui.*;

var d = 40;
var p1 = d;
var p2 = p1 + d;
var p3 = p2 + d;
var p4 = p3 + d;
        
Frame {
    content : Canvas {
        width : 200
        height : 200
        background : Color.BLACK
        content : [
        Line {
            startX : p3
            startY : p3
            endX : p2
            endY : p3
            stroke : Color.LIGHTGREY
        },
        Line {
            startX : p2
            startY : p3
            endX : p2
            endY : p2
            stroke : Color.LIGHTGREY
        },
        Line {
            startX : p2
            startY : p2
            endX : p3
            endY : p2        
            stroke : Color.LIGHTGREY
        },
        Line {
            startX : p3
            startY : p2
            endX : p3
            endY : p3        
            stroke : Color.LIGHTGREY
        },
        // Points
        Line {
            startX : p1
            startY : p1
            endX : p1
            endY : p1
            stroke : Color.WHITE
        },
        Line {
            startX : p1
            startY : p3
            endX : p1
            endY : p3
            stroke : Color.WHITE
        },
        Line {
            startX : p2
            startY : p4
            endX : p2
            endY : p4
            stroke : Color.WHITE
        },
        Line {
            startX : p3
            startY : p1
            endX : p3
            endY : p1
            stroke : Color.WHITE
        },
        Line {
            startX : p4
            startY : p2
            endX : p4
            endY : p2
            stroke : Color.WHITE
        },
        Line {
            startX : p4
            startY : p4
            endX : p4
            endY : p4
            stroke : Color.WHITE
        }
        ]
    };
    
    visible : true
    title : "Points And Lines"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }
}