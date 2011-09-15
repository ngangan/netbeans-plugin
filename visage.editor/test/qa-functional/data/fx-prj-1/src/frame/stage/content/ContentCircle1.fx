
package frame.stage.content;

import visage.stage.Stage;
import visage.scene.Scene;
import visage.scene.shape.Circle;
import visage.scene.paint.Color;

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
