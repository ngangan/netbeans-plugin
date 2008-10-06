
package frame.stage.content;

import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.shape.Ellipse;
import javafx.scene.paint.Color;

Stage {
    title: "MyApplication"
    width: 200
    height: 200
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
