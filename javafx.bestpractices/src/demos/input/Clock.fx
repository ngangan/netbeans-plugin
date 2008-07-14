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

import javafx.scene.Node;
import javafx.scene.CustomNode;
import javafx.scene.Group;
import javafx.scene.geometry.Circle;
import javafx.scene.geometry.Line;
import javafx.application.Frame;
import javafx.application.Stage;
import javafx.scene.paint.Color;
import javafx.scene.transform.Translate;
import javafx.scene.transform.Rotate;
import javafx.animation.Timeline;
import javafx.animation.KeyFrame;

import java.util.Calendar;
import java.lang.System;

/**
 * @author Michal Skvor
 */

var clockWork : ClockWork = ClockWork {};

Frame {
    stage : Stage {
        content : clockWork
        }
    
    visible : true
    title : "Clock"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }    
    }

clockWork.timer.start();

public class ClockWork extends CustomNode {
    
    attribute seconds : Number;
    attribute minutes : Number;
    attribute hours : Number;
    
    public attribute timer : Timeline = Timeline {
        repeatCount : Timeline.INDEFINITE
        keyFrames : 
            KeyFrame {
                time : 1s
                action : function() {
                    var calendar : Calendar = Calendar.getInstance();
                    seconds = calendar.get( Calendar.SECOND );
                    minutes = calendar.get( Calendar.MINUTE );
                    hours = calendar.get( Calendar.HOUR_OF_DAY );        
                }
            }
    };
    
    public function create(): Node {
        return Group {
            content : [
                Circle {
                    transform : [ Translate { y : 100 }]
                    centerX : 100
                    radius : 80
                    fill : Color.GRAY
                },
                Line {
                    transform : [ Rotate { angle : bind seconds * 6, x : 100, y : 100 }]
                    startX : 100
                    startY : 30
                    endX : 100
                    endY : 100
                    stroke : Color.WHITE
                },
                Line {
                    transform : [ Rotate { angle : bind minutes * 6, x : 100, y : 100 }]
                    startX : 100
                    startY : 40
                    endX : 100
                    endY : 100
                    stroke : Color.WHITE
                    strokeWidth : 2
                },
                Line {
                    transform : [ Rotate { angle : bind hours * 30, x : 100, y : 100 }]
                    startX : 100
                    startY : 50
                    endX : 100
                    endY : 100
                    stroke : Color.WHITE
                    strokeWidth : 4
                }            
                ]
        };
    }
}