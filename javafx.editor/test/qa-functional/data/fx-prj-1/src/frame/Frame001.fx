
package frame;

import javafx.application.Frame;
import javafx.application.Stage;

Frame {
    
    title: "MyApplication"
    width: 200
    height: 200
    
    closeAction: function() { 
        java.lang.System.exit( 0 ); 
    }
    visible: true

    stage: Stage {
        content: []
    }
}
