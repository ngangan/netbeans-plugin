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

package particles;

import javafx.scene.Node;
import javafx.scene.CustomNode;
import javafx.scene.Group;
import javafx.scene.geometry.Circle;
import javafx.scene.paint.Color;
import javafx.ext.swing.Frame;
import javafx.ext.swing.Canvas;
import javafx.animation.Timeline;
import javafx.animation.KeyFrame;

import java.lang.Math;
import java.lang.System;

var parts : Particle[];

var timeline : Timeline = Timeline {
    repeatCount: Timeline.INDEFINITE
    keyFrames : 
        KeyFrame {
            time : 16.6ms
            action: function() {
                update();
            }                
    }
};

function update() : Void {
    insert Particle {
       x : 100
       y : 100
       vx : 1 - 2 * Math.random()
       vy : -2 * Math.random()
       accx : 0
       accy : 0.05
       timer : 100
    } into parts;
    var i = sizeof parts - 1;
    while( i >= 0 ) {
       parts[i.intValue()].update();
       if (parts[i.intValue()].isdead()) {
           delete parts[i.intValue()];
       }
       i--;
    }
}    
    
Frame {
    content : Canvas {
        background : Color.BLACK
        content : bind parts 
    }
    
    visible : true
    title : "Simple Particle System"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }
}

timeline.start();

public class Particle extends CustomNode {
    attribute x : Number;
    attribute y : Number;
    attribute vx : Number;
    attribute vy : Number;
    attribute accx : Number;
    attribute accy : Number;
    attribute timer : Number;
    
    function create(): Node {
       return Circle {
           centerX: bind x
           centerY: bind y
           radius: 5
           fill: Color.WHITE
           opacity: bind timer / 100
       };
    }
 
    function update(): Void {
       timer -= 1;
       x += vx;
       y += vy;
       vx += accx;
       vy += accy;
    }
    
    function isdead(): Boolean {
       return timer <= 0;
    }    
}
