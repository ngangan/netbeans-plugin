
package frame.stage.content;

import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.shape.Polyline;
import javafx.scene.paint.Color;

Stage {
    title: "MyApplication"
    width: 200
    height: 200
    visible: true

    scene: Scene {
        content: [
            Polyline {
                
                points : [ 0,0, 100,0, 100,100 ]
                strokeWidth: 2.0
                stroke: Color.RED
            }
        ]
    }
}
