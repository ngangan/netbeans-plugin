
package frame.stage.content;

import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.shape.Circle;
import javafx.scene.paint.Color;

Stage {
    title: "MyApplication"
    width: 200
    height: 200
    visible: true

    scene: Scene {
        content: [
            Circle {
                
                centerX: 100, centerY: 100
                radius: 40
                fill: Color.BLACK
            }
        ]
    }
}
