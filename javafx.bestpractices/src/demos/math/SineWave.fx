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

import javafx.ext.swing.SwingFrame;
import javafx.ext.swing.Canvas;
import javafx.scene.geometry.Circle;
import javafx.scene.paint.Color;
import javafx.animation.Timeline;
import javafx.animation.KeyFrame;

import java.lang.Math;
    
/**
 * @author Michal Skvor
 */

var dots : Circle[];
var theta : Number = 0.0;
var amplitude : Number = 75;
var xspacing : Number = 8;
var period : Number = 500;
var dx : Number = ( Math.PI * 2 / period ) * xspacing;
var time : Number = 0.0;

var timeline : Timeline = Timeline {
    repeatCount: Timeline.INDEFINITE        
    keyFrames : 
        KeyFrame {
            time : 16.6ms
            action: function() {
                time += 0.02;
            }                
    }
};

var x = theta;
for( i in [0..25] ) {      
    var xx = x;
    insert Circle {
        centerX : i * 8
        centerY : bind 100 + Math.sin( xx + time ) * amplitude
        radius : 8
        fill : Color.WHITE
        opacity : 0.3
    } into dots;
    x += dx;
}

SwingFrame {
    content : Canvas {
        background : Color.BLACK
        content : dots
    }
    
    visible : true
    title : "Sine Wave"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }        
}

timeline.start();
