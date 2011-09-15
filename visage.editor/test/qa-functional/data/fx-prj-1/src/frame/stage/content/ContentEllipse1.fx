
package frame.stage.content;

import visage.stage.Stage;
import visage.scene.Scene;
import visage.scene.shape.Ellipse;
import visage.scene.paint.Color;

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
