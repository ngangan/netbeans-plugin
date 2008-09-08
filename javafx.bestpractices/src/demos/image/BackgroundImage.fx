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

package image;

import javafx.animation.Timeline;
import javafx.animation.KeyFrame;
import javafx.animation.Interpolator;
import javafx.scene.Group;
import javafx.scene.geometry.Line;
import javafx.scene.paint.Color;
import javafx.application.Frame;
import javafx.application.Stage;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.transform.Translate;

/**
 * @author Michal Skvor
 */

var y : Number;

var timeline = Timeline {
    repeatCount: Timeline.INDEFINITE
    keyFrames : [
        KeyFrame {
            time : 0s
            values :
                y => -20
        },
        KeyFrame {
            time : 3s
            values :
                y => 200 tween Interpolator.LINEAR
        }
    ]
};

Frame {
    stage : Stage {
        content : [
            ImageView {
                image : Image { url : "{__DIR__}/../resources/background.png" }
            },
            Group {
                transforms : Translate { y : bind y }
                content : [
                    Line {
                        startX : 0, startY : 20, endX : 200, endY : 0
                        stroke : Color.RED
                    },
                    Line {
                        startX : 0, startY : 20, endX : 200, endY : 0
                        stroke : Color.RED
                        transforms : Translate { y : 10 }
                    }
                ]
            }
        ]
    }

    visible : true
    title : "Background Image"
    width : 200
    height : 232
    closeAction : function() { java.lang.System.exit( 0 ); }
}

timeline.start();
