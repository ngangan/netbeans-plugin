package helloworld;

import visage.stage.Stage;
import visage.scene.Scene;
import visage.scene.text.Text;



Stage {
    title: "Hello World Visage!"
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
