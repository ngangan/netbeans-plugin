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

import javafx.gui.*;

var width : Number = 200;
var height : Number = 200;
var mouseX : Number = 0;
var mouseY : Number = 0;
    
Frame {
    content : Canvas {
        content : [
            Rectangle {
                width : 200, height : 200
                fill : Color { red : 0.2, green : 0.2, blue :0.2 }
                
                onMouseMoved : function( e : MouseEvent ): Void {
                    mouseX = e.getX();
                    mouseY = e.getY();
                }
            },
            Rectangle {
                x : bind mouseX - mouseY / 4 - 5
                y : bind height / 2 - mouseY / 4 - 5
                width : bind mouseY / 2 + 10
                height : bind mouseY / 2 + 10
                fill : Color.WHITE
                opacity : 0.8
            },
            Rectangle {
                x : bind width - mouseX - (( height - mouseY ) / 4 ) - 5
                y : bind height / 2 - (( height - mouseY ) / 4 ) - 5
                width : bind (( height - mouseY ) / 2 ) + 10
                height : bind (( height - mouseY ) / 2 ) + 10
                fill : Color.WHITE
                opacity : 0.8
            }
        ]
    }
    
    visible : true
    title : "Mouse 2D"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }
}