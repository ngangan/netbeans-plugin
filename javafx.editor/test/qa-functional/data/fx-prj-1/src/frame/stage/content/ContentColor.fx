
package frame.stage.content;

import javafx.application.Frame;
import javafx.scene.Scene;
import javafx.scene.paint.Color;
import javafx.scene.geometry.Arc;
import javafx.scene.geometry.ArcType;
import javafx.scene.geometry.Circle;

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
