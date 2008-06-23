/*
 * Copyright (c) 2007, Sun Microsystems, Inc.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 * 
 *  * Redistributions of source code must retain the above copyright notice, 
 *    this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright 
 *    notice, this list of conditions and the following disclaimer in 
 *    the documentation and/or other materials provided with the distribution.
 *  * Neither the name of Sun Microsystems, Inc. nor the names of its 
 *    contributors may be used to endorse or promote products derived 
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED 
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
 */

 package transform;

import javafx.gui.*;
import javafx.animation.*;

import java.lang.Math;
import java.util.Random;

var angle : Number = 0.0;
var jitter : Number = 0.0;

var random : Random = new Random();

var ticker : Timeline = Timeline {
    repeatCount: Timeline.INDEFINITE
    keyFrames : 
        KeyFrame {
            time : 20ms
            action : function(): Void {
                angle += jitter;
            }
    }
};

var jitterTimeline : Timeline = Timeline {
    repeatCount: Timeline.INDEFINITE
    keyFrames :
        KeyFrame {
            time : 1s
            action : function(): Void {
                jitter = random.nextDouble() * 12 - 6;
            }
    }
};
    
Frame {    
    content : Canvas {
        background : Color.GRAY
        content : Rectangle {
            transform : [ 
                javafx.gui.Rotate { angle : bind angle, x : 100, y : 100 }, 
                javafx.gui.Translate { x : 43, y : 43 } 
            ]
            width : 114, height : 114
            fill : Color.WHITE
        }
    }
    
    visible : true
    title : "Rotate"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }
}

ticker.start();
jitterTimeline.start();
