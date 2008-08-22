
package frame.stage.content;

import javafx.application.Frame;
import javafx.application.Stage;
import javafx.scene.geometry.Polyline;
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
            Polyline {
                
                points : [ 0,0, 100,0, 100,100 ]
                strokeWidth: 2.0
                stroke: Color.RED
            }
        ]
    }
}
