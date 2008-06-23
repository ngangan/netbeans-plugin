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

import javafx.gui.*;

import java.lang.Math;

var eyes : Eye[] = [
    Eye { x : 50, y : 16, size : 40 },
    Eye { x : 64, y : 85, size : 20 },  
    Eye { x : 90, y : 200, size : 60 },
    Eye { x : 150, y : 44, size : 20 }, 
    Eye { x : 175, y : 120,  size : 40 }
];
    
Frame {
    content : Canvas {
        content : [
            Rectangle {
                width : 200, height : 200
                fill : Color.LIGHTGREY
                
                onMouseMoved : function( e : MouseEvent ) {
                    for( eye in eyes ) {
                        eye.mouse = e;
                    }
                }
            },
            eyes
        ]
    }
    
    visible : true
    title : "Arctangent"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }
}

class Eye extends CustomNode {
    
    public attribute x : Number;
    public attribute y : Number;
    
    public attribute size : Number;
    
    public attribute mouse : MouseEvent;
    attribute angle : Number = bind Math.toDegrees( Math.atan2( y - mouse.getY(), x - mouse.getX())) + 180;
    
    public function create(): Node {
        return Group {
            transform : Translate { x : bind x, y : bind y }
            content : [
                Circle {
                    radius : bind size
                    fill : Color.WHITE
                },
                Circle {
                    transform : Rotate { angle : bind angle }
                    centerX : bind size / 2, centerY : 0, radius : bind size / 2
                    fill : Color.rgb( 153, 153, 153 )
                }
            ]
        };
    }
}
