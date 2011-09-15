
package frame.stage.content;

import visage.stage.Stage;
import visage.scene.Scene;
import visage.scene.text.Text;
import visage.scene.text.Font;

Stage {
    title: "MyApplication"
    width: 200
    height: 200
    visible: true

    scene: Scene {
        content: [
            Text {
                
                font: Font { 
                    
                    size: 24 
                }
                x: 10, y: 30
                content: "HelloWorld"
            }
        ]
    }
}
