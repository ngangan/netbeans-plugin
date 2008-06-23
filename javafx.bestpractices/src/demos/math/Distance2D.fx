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
import java.lang.System;

var distance = 0;
var circles : Circle[];

var width : Number = 100;
var height : Number = 100;

var mouseX : Number = 0;
var mouseY : Number = 0;

var max_distance : Number = dist( 0, 0, width, height );

function dist( x1 : Number, y1 : Number, x2 : Number, y2 : Number ): Number {
    return Math.sqrt(( x1 - x2 ) * ( x1 - x2 ) + ( y1 - y2 ) * ( y1 - y2 ));
}

for( x in [0..10] ) {
    for( y in [0..10] ) {
        var cx = x * 20 + 10;
        var cy = y * 20 + 10;
        insert Circle {
            centerX : cx
            centerY : cy
            radius : bind dist( mouseX, mouseY, cx, cy ) / max_distance * 20
            fill : Color.WHITE
        } into circles;
    }
}

Frame {
    content : Canvas {    
        width : 200
        height : 200
        content : [ 
            Rectangle {
                width : 200, height : 200
                fill : Color.GRAY
                
                onMouseMoved : function( e : MouseEvent ): Void {
                    mouseX = e.getX();
                    mouseY = e.getY();
                }            
            },
            circles
        ]
    };        
    
    visible : true
    title : "Distance 2D"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }    
}
