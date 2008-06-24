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

 package forms;

import javafx.scene.geometry.Line;
import javafx.scene.paint.Color;
import javafx.scene.swing.Frame;
import javafx.scene.swing.Canvas;

var d = 40;
var p1 = d;
var p2 = p1 + d;
var p3 = p2 + d;
var p4 = p3 + d;
        
Frame {
    content : Canvas {
        width : 200
        height : 200
        background : Color.BLACK
        content : [
        Line {
            startX : p3
            startY : p3
            endX : p2
            endY : p3
            stroke : Color.LIGHTGREY
        },
        Line {
            startX : p2
            startY : p3
            endX : p2
            endY : p2
            stroke : Color.LIGHTGREY
        },
        Line {
            startX : p2
            startY : p2
            endX : p3
            endY : p2        
            stroke : Color.LIGHTGREY
        },
        Line {
            startX : p3
            startY : p2
            endX : p3
            endY : p3        
            stroke : Color.LIGHTGREY
        },
        // Points
        Line {
            startX : p1
            startY : p1
            endX : p1
            endY : p1
            stroke : Color.WHITE
        },
        Line {
            startX : p1
            startY : p3
            endX : p1
            endY : p3
            stroke : Color.WHITE
        },
        Line {
            startX : p2
            startY : p4
            endX : p2
            endY : p4
            stroke : Color.WHITE
        },
        Line {
            startX : p3
            startY : p1
            endX : p3
            endY : p1
            stroke : Color.WHITE
        },
        Line {
            startX : p4
            startY : p2
            endX : p4
            endY : p2
            stroke : Color.WHITE
        },
        Line {
            startX : p4
            startY : p4
            endX : p4
            endY : p4
            stroke : Color.WHITE
        }
        ]
    };
    
    visible : true
    title : "Points And Lines"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }
}