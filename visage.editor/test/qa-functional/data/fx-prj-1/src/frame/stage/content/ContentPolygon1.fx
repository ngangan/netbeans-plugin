
package frame.stage.content;

import visage.stage.Stage;
import visage.scene.Scene;
import visage.scene.shape.Polygon;
import visage.scene.paint.Color;

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
