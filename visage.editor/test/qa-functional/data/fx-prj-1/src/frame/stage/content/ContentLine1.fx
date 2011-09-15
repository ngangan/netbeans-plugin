
package frame.stage.content;

import visage.stage.Stage;
import visage.scene.Scene;
import visage.scene.shape.Line;
import visage.scene.paint.Color;

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
