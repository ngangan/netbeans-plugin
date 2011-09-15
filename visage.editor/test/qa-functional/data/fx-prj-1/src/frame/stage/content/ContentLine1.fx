
package frame.stage.content;

import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.shape.Line;
import javafx.scene.paint.Color;

Stage {
    title: "MyApplication"
    width: 200
    height: 200
    visible: true

    scene: Scene {
        content: [
            Line {
                
                startX: 10, startY: 10
                endX: 230, endY: 100
                strokeWidth: 1
                stroke: Color.BLACK
            }
        ]
    }
}
