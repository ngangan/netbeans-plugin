
package frame.stage;

import javafx.stage.Frame;
import javafx.scene.Scene;
import javafx.scene.shape.Line;
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
            
        ]
    }
}
