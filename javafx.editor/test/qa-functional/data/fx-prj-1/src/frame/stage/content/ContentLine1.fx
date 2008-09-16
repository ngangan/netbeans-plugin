
package frame.stage.content;

import javafx.application.Frame;
import javafx.scene.Scene;
import javafx.scene.geometry.Line;
import javafx.scene.paint.Color;

Frame {
    title: "MyApplication"
    width: 200
    height: 200
    closeAction: function() { 
        java.lang.System.exit( 0 ); 
    }
    visible: true

    scene: Scene {
        content: [
            Line {
                
                startX: 10, startY: 10
                endX: 230, endY: 100
                strokeWidth: 1
                stroke: Color.BLACK
            }
        ]
    }
}
