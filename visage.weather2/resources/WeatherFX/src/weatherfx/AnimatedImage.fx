/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER 
 * Copyright  2008, 2010 Oracle and/or its affiliates.  All rights reserved. 
 * Use is subject to license terms.
 * 
 * This file is available and licensed under the following license:
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met: 
 * 
 *   * Redistributions of source code must retain the above copyright notice, 
 *     this list of conditions and the following disclaimer. 
 *
 *   * Redistributions in binary form must reproduce the above copyright notice,
 *     this list of conditions and the following disclaimer in the documentation
 *     and/or other materials provided with the distribution.
 *
 *   * Neither the name of Oracle Corporation nor the names of its contributors 
 *     may be used to endorse or promote products derived from this software 
 *     without specific prior written permission. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR 
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
 */

 
package weatherfx;

import visage.scene.image.Image;
import visage.scene.image.ImageView;
import visage.animation.*;
import java.lang.*;

/**
 * Animates a set of images using specified frame delay
 * 
 * @author breh
 */

public function create(baseURL:String, baseName:String, extension:String, imagesCount:Integer):AnimatedImage {
    var animatedImage = AnimatedImage{baseURL:baseURL, baseName:baseName, extension:extension, imagesCount:imagesCount};
    animatedImage.loadImages();
    animatedImage.play();
    return animatedImage;
}

public class AnimatedImage extends ImageView {

    
    var baseURL: String;
    var baseName: String;
    var extension: String;

    var imagesCount: Integer = 0;
    
    var images: Image[];
    
    var currentImageIndex:Integer ;
    
    var movieTimeline = Timeline {
        repeatCount: Double.POSITIVE_INFINITY
        keyFrames: KeyFrame {
            time: 1s/12
            action: function() {                
                currentImageIndex += 1;
                if (currentImageIndex >= imagesCount) {
                    currentImageIndex = 0;
                }
                image = images[currentImageIndex];
            }
        },
    };    
    
    
    public function play():Void {
        if (images == null) {
            loadImages();
        }
        movieTimeline.play();
    }
    
    public function stop():Void {
        movieTimeline.stop();
    }
    
    
    public function loadImages():Void {
        var count = imagesCount - 1;
        if (baseURL != null) {
            images =  for (i in [0..count]) {
                CachedImage.getCachedImage("{baseURL}{baseName}{%03d i}.{extension}");
            };    
        }
    }   
}