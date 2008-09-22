
package frame.stage.content;

import javafx.stage.Frame;
import javafx.scene.Scene;
import javafx.scene.text.Text;
import javafx.scene.text.Font;

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
