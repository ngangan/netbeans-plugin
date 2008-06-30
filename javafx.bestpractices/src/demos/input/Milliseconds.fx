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

package input;

import javafx.scene.paint.Color;
import javafx.scene.geometry.Rectangle;
import javafx.ext.swing.Frame;
import javafx.ext.swing.Canvas;
import javafx.animation.Timeline;
import javafx.animation.KeyFrame;

var miliseconds : Integer;
var colors : Color[];

var timeline : Timeline = Timeline {
    repeatCount: Timeline.INDEFINITE
    keyFrames : 
        KeyFrame {
            time : 16ms
            action : function() : Void {
                var milliseconds : Number = ( java.lang.System.currentTimeMillis()) as Number;                
                for( i in [0..9] ) {
                    var ii : Integer = ( i + 1 ) * 100;
                    var color : Number = milliseconds % ii / ii;
                    colors[i] = Color { 
                        red : color, green : color, blue : color };
                }
            }
        }
};

var barrs : Rectangle[];
for( i in [0..9] ) {
    var ii : Integer = i;
    insert Color {} into colors;
    insert Rectangle {
        x : i * 20, y : 0, width : 20, height : 200
        fill : bind colors[ii]
    } into barrs;
}

Frame {
    content : Canvas {
        content : bind barrs;
    };
    
    visible : true
    title : "Milliseconds"
    width : 208
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }
}

timeline.start();
