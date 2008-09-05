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

import javafx.input.MouseEvent;
import javafx.scene.Node;
import javafx.scene.CustomNode;
import javafx.scene.geometry.Rectangle;
import javafx.scene.geometry.Circle;
import javafx.scene.paint.Color;
import javafx.application.Frame;
import javafx.application.Stage;
import javafx.animation.Timeline;
import javafx.animation.KeyFrame;

import java.lang.Math;

/**
 * @author Michal Skvor
 */

var easing : Ball = Ball {};

Frame {
    stage : Stage {
        content : [
            Rectangle {
                width : 200, height : 200
                fill : Color{ red: 0.2, green : 0.2, blue : 0.2 }

                onMouseMoved : function( e : MouseEvent ): Void {
                    easing.targetX = e.x;
                    easing.targetY = e.y;
                }
            },
            easing
        ]

    };

    visible : true
    title : "Easing"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }
}

class Ball extends CustomNode {
    var x : Number;
    var y : Number;
    public var targetX : Number = 100;
    public var targetY : Number = 100;

    var easing : Number = 0.05;

    init {
        timer.start();
    }

    var timer : Timeline = Timeline {
        repeatCount: Timeline.INDEFINITE
        keyFrames :
            KeyFrame {
                time : 20ms
                action : function() {
                    var dx = targetX - x;
                    if( Math.abs( dx ) > 1 ) {
                        x += dx * easing;
                    }

                    var dy = targetY - y;
                    if( Math.abs( dy ) > 1 ) {
                        y += dy * easing;
                }
            }
        }
    };

    public override function create(): Node {
        return Circle {
            centerX : bind x
            centerY : bind y
            radius : 33 / 2
            fill : Color.WHITE
        };
    }
}