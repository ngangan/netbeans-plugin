package auto1;

import javafx.animation.*;

var rotation = 0;
var anim = Timeline {
    
    repeatCount: Timeline.INDEFINITE   
    keyFrames : [   
        
        KeyFrame {   
            
            time : 0s   
            values: rotation => 0 tween Interpolator.LINEAR   
        },   
        KeyFrame {   
            time : 2s   
            values: rotation => (360/12) tween Interpolator.LINEAR   
        },   
    ]   
}   

anim.play();