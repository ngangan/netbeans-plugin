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
 
package color;

import javafx.scene.Group;
import javafx.scene.CustomNode;
import javafx.scene.Node;
import javafx.scene.paint.Color;
import javafx.scene.geometry.Arc;
import javafx.scene.geometry.ArcType;
import javafx.ext.swing.Frame;
import javafx.ext.swing.Canvas;

public class ColorWheel extends CustomNode {
    attribute segments : Number = 12;
    attribute steps : Number = 6;
    attribute radius : Number = 95;
    attribute valueShift : Number = 0;
    attribute stripes : Arc[];
    
    function create() : Node {
        return Group {
            content : bind stripes
        };
    }
    
    init {
        var r = radius;
        var rStep = radius / steps;
        var colors : Color[];
        for( i in [1..segments + 1] ) {
            insert Color.rgb( 255, 255, 0 ) into colors;
        }
        for( i in [0..steps-1] ) {
            if( valueShift == 0 ) {
                colors[1] = Color.rgb( 255 - ( 255 / steps * i ), 255 - ( 255 / steps * i ), 0 );
                colors[2] = Color.rgb( 255 - ( 255 / steps ) * i, ( 255 / 1.5 ) - (( 255 / 1.5 ) / steps ) * i, 0 ); 
                colors[3] = Color.rgb( 255 - ( 255 / steps ) * i, ( 255 / 2 ) - ( ( 255 / 2 ) / steps ) * i, 0 ); 
                colors[4] = Color.rgb( 255 - ( 255 / steps ) * i, ( 255 / 2.5 ) - (( 255 / 2.5 ) / steps ) * i, 0 ); 
                colors[5] = Color.rgb( 255 - ( 255 / steps ) * i, 0, 0 );
                colors[6] = Color.rgb( 255 - ( 255 / steps ) * i, 0, ( 255 / 2 ) - (( 255 / 2 ) / steps ) * i );
                colors[7] = Color.rgb( 255 - ( 255 / steps ) * i, 0, 255 - ( 255 / steps ) * i ); 
                colors[8] = Color.rgb(( 255 / 2 ) - (( 255 / 2 ) / steps ) * i, 0, 255 - ( 255 / steps ) * i ); 
                colors[9] = Color.rgb( 0, 0, 255 - ( 255 / steps ) * i );
                colors[10] = Color.rgb( 0, 255 - ( 255 / steps ) * i, ( 255 / 2.5 ) - (( 255 / 2.5 ) / steps ) * i ); 
                colors[11] = Color.rgb( 0 , 255 - ( 255 / steps ) * i, 0 ); 
                colors[12] = Color.rgb(( 255 / 2 ) -(( 255 / 2 ) / steps ) * i, 255 - ( 255 / steps ) * i, 0 );
            } else if( valueShift == 1 ) {
                colors[1] = Color.rgb(( 255 / steps ) * i, ( 255 / steps ) * i, 0 ); 
                colors[2] = Color.rgb(( 255 / steps ) * i, (( 255 / 1.5 ) / steps ) * i, 0 ); 
                colors[3] = Color.rgb(( 255 / steps ) * i, (( 255 / 2 ) / steps ) * i, 0 ); 
                colors[4] = Color.rgb(( 255 / steps ) * i, (( 255 / 2.5 ) / steps ) * i, 0 ); 
                colors[5] = Color.rgb(( 255 / steps ) * i, 0, 0 ); 
                colors[6] = Color.rgb(( 255 / steps ) * i, 0, (( 255 / 2 ) / steps ) * i ); 
                colors[7] = Color.rgb(( 255 / steps ) * i, 0, ( 255 / steps ) * i ); 
                colors[8] = Color.rgb((( 255 / 2 ) / steps ) * i, 0, ( 255 / steps ) * i ); 
                colors[9] = Color.rgb( 0, 0, ( 255 / steps ) * i );
                colors[10] = Color.rgb( 0, ( 255 / steps ) * i, (( 255 / 2.5 ) / steps ) * i ); 
                colors[11] = Color.rgb( 0, ( 255 / steps ) * i, 0 ); 
                colors[12] = Color.rgb((( 255 / 2 ) / steps ) * i, ( 255 / steps ) * i, 0 );        
            }
            for( j in [1..segments] ) {
                var c = colors[j.intValue()];                
                insert Arc {
                    centerX : 100, centerY : 100
                    radiusX : r , radiusY : r
                    startAngle : 360 / segments * ( segments - j - 0.5)
                    length : 360 / segments
                    fill : c
                    type : ArcType.ROUND
                } into stripes;
            }
            r -= rStep;
        }
    }    
}


Frame {
    content : Canvas {
        background : Color.GRAY
        content : ColorWheel {}
    }
    
    visible : true
    title : "Color Wheel"
    width : 209
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }
}
