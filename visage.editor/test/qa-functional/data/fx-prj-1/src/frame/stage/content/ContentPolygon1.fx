
package frame.stage.content;

import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.shape.Polygon;
import javafx.scene.paint.Color;

Stage {
    title: "MyApplication"
    width: 200
    height: 200
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
