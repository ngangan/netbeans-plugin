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

package math;

import javafx.application.Frame;
import javafx.application.Stage;
import javafx.scene.geometry.Circle;
import javafx.scene.paint.Color;
import javafx.scene.transform.Translate;
import javafx.scene.transform.Rotate;
import javafx.animation.Timeline;
import javafx.animation.KeyFrame;
import javafx.animation.Interpolator;

import java.lang.Math;

/**
 * @author Michal Skvor
 */

var diameter : Number = bind 45 * Math.sin( angle ) + 210;
var angle : Number = 0.0;

var circles : Circle[];

var timeline : Timeline = Timeline {
    repeatCount: Timeline.INDEFINITE     
    keyFrames : [
        KeyFrame {
            time : 0s;
            values : {
                angle => 0.0
            }
        },
        KeyFrame {
            time : 10s;
            values : {
                angle => Math.PI * 2 tween Interpolator.LINEAR
            }
        }
    ]
};

for( i in [0..4] ) {
    insert Circle {
        transform : [ Rotate{ angle : angle + 45, x : 130, y : 65 } ]
            fill : Color.BLACK
            radius : bind diameter / 2
    } into circles;
    angle += 360 / 5;
}

Frame {
    stage : Stage {
        fill : Color.LIGHTGREY
        content : [
            Circle {
                centerX : 130
                centerY : 65
                radius : 8
                fill : Color.WHITE
            }, circles ]        
    };

    
    visible : true
    title : "Sine"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }        
}

timeline.start();
