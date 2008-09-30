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

import javafx.scene.input.MouseEvent;
import javafx.scene.Node;
import javafx.scene.CustomNode;
import javafx.scene.Group;
import javafx.scene.shape.Rectangle;
import javafx.scene.shape.Circle;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.animation.Timeline;
import javafx.animation.KeyFrame;

import java.lang.System;

/**
 * @author Michal Skvor
 */

Stage {
    var input : StoringInput = StoringInput {};
    scene : Scene {
        content : bind [
            Rectangle {
                width : 200, height : 200
                fill : Color.GRAY

                onMouseMoved : function( e : MouseEvent ): Void {
                    input.mouseX = e.sceneX;
                    input.mouseY = e.sceneY;
                }
            },
            input
        ]
    }

    visible : true
    title : "Storing Input"
    width : 200
    height : 232
}

class StoringInput extends CustomNode {

    var circles : Circle[];
    var mouseX : Number;
    var mouseY : Number;

    var length : Integer = 60;

    var timer : Timeline = Timeline {
        repeatCount: Timeline.INDEFINITE
        keyFrames :
            KeyFrame {
                time : 16ms
                action : function() {
                    update();
                }
            }
    }

    public function update() : Void {
        for( i in [0..length - 1] ) {
            circles[i].centerX = circles[i+1].centerX;
            circles[i].centerY = circles[i+1].centerY;
            circles[i].radius = circles[i+1].radius;
        }
        circles[length] = Circle {
            centerX : mouseX, centerY : mouseY, radius : 30, fill : Color.WHITE, opacity : 0.3
        };
        for( i in [0..length] ) {
            circles[i].radius = i / 4;
        }
    }

    public override function create(): Node {
        return Group {
            content : bind circles
        };
    }

    init {
        for( i in [0..length] ) {
            insert Circle { fill : Color.WHITE } into circles;
        }
        timer.play();
    }
}