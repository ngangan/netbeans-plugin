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

import javafx.gui.*;

Frame {
    content : Canvas {    
        background : Color.BLACK
        content : [
        Polygon {
            points : [ 10, 10, 10, 200, 45, 200 ]
            fill : Color.LIGHTGREY
        },
        Rectangle {
            x : 45
            y : 34
            width : 35
            height : 35
            fill : Color.LIGHTGREY
        },
        Polygon {
            points : [ 105, 10, 120, 10, 120, 200, 80, 200 ]
            fill : Color.LIGHTGREY
        },
        Circle {
            centerX : 140
            centerY : 80
            radius : 20
            fill : Color.LIGHTGREY
        },
        Polygon {
            points : [ 160, 10, 195, 200, 160, 200 ]
            fill : Color.LIGHTGREY
        }
        ]
    };
    
    visible : true
    title : "Shape Primitives"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }
}