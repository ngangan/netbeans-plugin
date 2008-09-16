
package frame.stage.content;

import javafx.application.Frame;
import javafx.scene.Scene;
import javafx.scene.geometry.Polygon;
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
            Polygon {
                
                points : [ 0,0, 100,0, 100,100 ]
                fill: Color.YELLOW
            }
        ]
    }
}
