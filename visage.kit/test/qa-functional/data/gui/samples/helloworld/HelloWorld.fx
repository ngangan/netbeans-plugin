package helloworld;

import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.text.Text;



Stage {
    title: "Hello World JavaFX!"
    width: 300
    height: 200
    onClose: function() { java.lang.System.exit( 0 ); }

    scene: Scene{
        content: [
            Text {
                x: 10, y: 30
                content: "Hello World!"
            }
        ]
    }
    visible: true
}
