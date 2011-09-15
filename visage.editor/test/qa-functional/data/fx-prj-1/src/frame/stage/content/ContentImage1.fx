
package frame.stage.content;

import visage.stage.Stage;
import visage.scene.Scene;
import visage.scene.image.ImageView;
import visage.scene.image.Image;

Stage {
    title: "MyApplication"
    width: 200
    height: 200
    visible: true

    scene: Scene {
        content: [
            ImageView {
                
                image: Image {
                    
                    url: "{__DIR__}/myPicture.png"
                }
            }
        ]
    }
}
