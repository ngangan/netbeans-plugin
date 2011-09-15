package applications;

import javafx.scene.Scene;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.stage.Stage;

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





