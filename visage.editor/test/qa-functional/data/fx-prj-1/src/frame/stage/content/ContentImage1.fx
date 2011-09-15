
package frame.stage.content;

import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.image.ImageView;
import javafx.scene.image.Image;

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
