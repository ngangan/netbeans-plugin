package applications;

import visage.scene.Scene;
import visage.scene.text.Font;
import visage.scene.text.Text;
import visage.stage.Stage;

/** FontStyle should not return $ classes */
Stage{
    title: "Application title"
    width: 250
    height: 80
    scene: Scene{
        content: Text {
            x: 10, y: 30
            content: "Application content"
            font: Font {
                size: 24
                
            }
        }
    }
}





