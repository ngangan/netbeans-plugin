
package frame.stage.content;

import javafx.application.Frame;
import javafx.application.Stage;
import javafx.scene.geometry.Rectangle;
import javafx.scene.paint.Color;

Frame {
    title: "MyApplication"
    width: 200
    height: 200
    closeAction: function() { 
        java.lang.System.exit( 0 ); 
    }
    visible: true

    stage: Stage {
        content: [
            Rectangle {
                
                x: 10, y: 10
                width: 140, height: 90
                fill: Color.BLACK
            }
        ]
    }
}
