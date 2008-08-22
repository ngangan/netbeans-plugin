
package frame.stage.content;

import javafx.application.Frame;
import javafx.application.Stage;
import javafx.scene.text.Text;
import javafx.scene.text.Font;
import javafx.scene.FontStyle;

Frame {
    title: "MyApplication"
    width: 200
    height: 200
    closeAction: function() { 
        java.lang.System.exit( 0 ); 
    }
    visible: true

    stage: Stage {
        content: [
            Text {
                
                font: Font { 
                    
                    size: 24 
                    style: FontStyle.PLAIN
                }
                x: 10, y: 30
                content: "HelloWorld"
            }
        ]
    }
}
