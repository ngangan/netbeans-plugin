
package frame.stage.content;

import visage.stage.Stage;
import visage.scene.Scene;
import visage.scene.shape.Rectangle;
import visage.scene.paint.Color;

Stage {
    title: "MyApplication"
    width: 200
    height: 200
    visible: true

    scene: Scene {
        content: [
            Rectangle {
                
                x: 10, y: 10
                width: 140, height: 90
                fill: Color.BLACK
            }
        ]
    }
}
