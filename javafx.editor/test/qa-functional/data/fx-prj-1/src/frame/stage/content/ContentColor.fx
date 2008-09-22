
package frame.stage.content;

import javafx.stage.Frame;
import javafx.scene.Scene;
import javafx.scene.paint.Color;
import javafx.scene.shape.Arc;
import javafx.scene.shape.ArcType;
import javafx.scene.shape.Circle;

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
            Circle {
                centerX: 100, centerY: 100
                radius: 40
                
            }
        ]
    }
}
