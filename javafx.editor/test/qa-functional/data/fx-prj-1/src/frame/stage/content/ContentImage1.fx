
package frame.stage.content;

import javafx.application.Frame;
import javafx.scene.Scene;
import javafx.scene.image.ImageView;
import javafx.scene.image.Image;

Frame {
    title: "MyApplication"
    width: 200
    height: 200
    closeAction: function() { 
        java.lang.System.exit( 0 ); 
    }
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
