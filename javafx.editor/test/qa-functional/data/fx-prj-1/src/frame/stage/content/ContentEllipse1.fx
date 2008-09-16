
package frame.stage.content;

import javafx.application.Frame;
import javafx.scene.Scene;
import javafx.scene.geometry.Ellipse;
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
            Ellipse {
                
                centerX: 100, centerY: 100
                radiusX: 40, radiusY: 15
                fill: Color.GREEN
            }
        ]
    }
}
