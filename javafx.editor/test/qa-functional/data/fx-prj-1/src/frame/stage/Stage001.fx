
package frame.stage;

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
            
        ]
    }
}
