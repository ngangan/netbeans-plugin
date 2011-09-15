
package frame.stage.content;

import visage.stage.Stage;
import visage.scene.Scene;
import visage.scene.shape.Polyline;
import visage.scene.paint.Color;

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
