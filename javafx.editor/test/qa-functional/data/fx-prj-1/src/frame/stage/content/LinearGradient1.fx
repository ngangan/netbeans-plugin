package frame.stage.content;

import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.shape.Rectangle;
import javafx.scene.paint.Color;
import javafx.scene.paint.LinearGradient;

Stage {
    title: "MyApplication"
    width: 200
    height: 200
    visible: true

    scene: Scene {
        content: [Rectangle {
                x: 10, y: 10
                width: 140, height: 90
                fill: LinearGradient {
                    
                }
            }]
    }
}
