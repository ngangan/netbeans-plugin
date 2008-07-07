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

package flickflockr;

import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.CustomNode;
import javafx.scene.Font;
import javafx.scene.FontStyle;
import javafx.scene.Cursor;

import javafx.scene.geometry.Rectangle;
import javafx.scene.geometry.Line;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.transform.*;
import javafx.scene.text.Text;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.animation.*;

import javafx.ext.swing.Canvas;
import javafx.ext.swing.Frame;
import javafx.ext.swing.Label;
import javafx.ext.swing.ComponentView;
import javafx.ext.swing.BorderPanel;
import javafx.ext.swing.TextField;
import javafx.ext.swing.Slider;

import com.aetrion.flickr.Flickr;
import com.aetrion.flickr.FlickrException;
import com.aetrion.flickr.photos.*;
import com.aetrion.flickr.photos.SearchParameters;
import com.aetrion.flickr.tags.*;

import java.util.concurrent.*;
import java.util.*;
import java.lang.Math;
import java.lang.System;


public class TagList extends CustomNode {
    attribute textOpacity: Number;
    attribute color = bind Color.color(.6, .6, .6, textOpacity);
    attribute selectionColor = Color.color(1, 1, 1, 1);
    attribute height: Number;
    attribute width: Number;
    attribute scrollY:Number;
    attribute tags: String[];
    attribute font = Font { size: 14, style: FontStyle.BOLD };
    attribute listGroup: Node;
    attribute maxWidth: Number = 150; //bind listGroup.currentWidth;
    attribute clickAction: function(tag:String):Void;



    public function create():Node {
        Group {
            var margin = 5;
            var spacing = 0;
            clip: Rectangle {
                height: bind height;
                width: bind width;
            }
            content:
            [Rectangle {
                height: bind sizeof tags * 25;
                width: bind width;
                fill: Color.color(0, 0, 0, 0);
                smooth: false
            },
            listGroup = Group { //VBox {
                transform: bind Transform.translate(0, scrollY);
                //spacing: spacing;
                content: bind for (tag in tags)
                    Group {
                        content:
                        Group {
                           translateY: indexof tag * (20+spacing)
                            var r:Rectangle;
                            content:
                            [r = Rectangle {
                                height: 20;
                                width: 150;
                                smooth: false
                                stroke: Color.color(0, 0, 0, 0);
                                fill: Color.color(0, 0, 0, 0);
                                onMouseClicked: function(e) {
                                    clickAction(tag);
                                }
                            },
                            Text {
                                fill: bind if (r.mouseOver) selectionColor else color
                                translateX: 5
                                translateY: 12;
                                //valign: 0.5;
                                content: tag;
                                font: font;
                            }]
                        }
                     }
            }]
        }
    }
}

public class Sprite extends CustomNode {
    
    attribute screenHeight: Number;
    attribute screenWidth: Number;
    attribute screenAlpha:Number = 1.0;
    attribute alpha:Number = 1.0;
    attribute textAlpha:Number = 0.0;
    attribute spriteHeading:Number;
    attribute spriteX:Number = bind if (not selected) boidx else x;
    attribute spriteY:Number =  bind if (not selected) boidy else y;
    attribute boidx: Number = bind boid.loc.x;
    attribute boidy: Number = bind boid.loc.y;
    attribute boidSelected: Boolean = bind boid.selected 
        on replace {
        if (not boid.selected and fullScreen) {
            fullScreenTimeline.stop();
            fader.stop();
            largeImage = null;
            largeImageFade = 0;
            x = boidx;
            y = boidy;
            nonFullScreenTimeline.start();
        }
    }
    attribute x: Number;
    attribute y: Number;
    attribute boidHeading:Number;
   
    attribute scaleTimeline: Timeline;
    attribute unscaleTimeline: Timeline;
    attribute fullScreenTimeline: Timeline;
    attribute nonFullScreenTimeline: Timeline;
    attribute largeImage: Image;
    attribute initialized: Boolean = false;
    init {
        initialized = true;
        
     fullScreenTimeline = Timeline {
        keyFrames:
        [KeyFrame {
                time: 0s;
                values:
                [alpha => 2.0,
                x => boidx,
                y => boidy]
        },
        KeyFrame {
                time: 0.5s;
                values:
                [alpha => screenAlpha tween Interpolator.EASEOUT,
                x => 0.0 tween Interpolator.EASEOUT,
                y => 0.0 tween Interpolator.EASEOUT]
                action: function() {}
        }]
     };
     nonFullScreenTimeline = Timeline {
        keyFrames:
        [KeyFrame {
                time: 0s;
                values:
                [alpha => screenAlpha,
                spriteHeading => 0.0,
                x => 0.0,
                y => 0.0]
                action: function() {}
        },
        KeyFrame {
                time: 0.5s;
                values:
                [alpha => 1.0 tween Interpolator.EASEOUT,
                x => boidx tween Interpolator.EASEOUT,
                y => boidy tween Interpolator.EASEOUT,              
                spriteHeading => boidHeading tween Interpolator.EASEOUT]
                action: function() { 
                        fullScreen = false; 
                        selected = false; 
                        textAlpha = 0; 
                        boid.returnToFlocking(); 
                 }
          }]
        };
    }
    attribute selected: Boolean = false;
    attribute r: Number= bind boid.r;
    attribute heading: Number;
      attribute imageUrl: String on replace { 
        if (imageUrl <> null) {
            image = Image { 
                placeholder: image
                url: imageUrl, backgroundLoading: true 
            }
        }
    }

    attribute fader: Timeline = Timeline {
        keyFrames:
        [KeyFrame {
                time: 1s;
                values: largeImageFade => 1.0 tween Interpolator.LINEAR
        }]
    };

    attribute largeImageFade: Number = 0.0;

    attribute largeImageUrl: String on replace { 
    }
    attribute boid: Boid;
    attribute image: Image;
    attribute nextImage: Image;
    attribute fullScreen: Boolean;

    public attribute clickAction: function(tag:String):Void;
    public attribute selectAction: function(tag:String):Void;

    function doClick(s:String):Void {
        clickAction(s);
        onLeave();
    }

    attribute largeImageProgress = bind largeImage.progress on replace {
        System.out.println("progress = {largeImageProgress}");
        if (largeImage <> null and largeImageProgress == 100) {
            fader.start();
        }
    }
    
    function doSelect():Void {
        if (not fullScreen and alpha == 2.0) {
            nonFullScreenTimeline.stop();
            var sx = screenWidth / (r*2);
            var sy = screenHeight / (r*2);
            fullScreen = true;
            screenAlpha = Math.max(sx, sy);
            //System.out.println("screenAlpha {screenAlpha}");
            fullScreenTimeline.start();
        if (largeImageUrl <> null) {
            fader.stop();
            System.out.println("large image { largeImageUrl }");
            largeImage = Image { 
                url: largeImageUrl, backgroundLoading: true 
                //width: bind screenWidth
                //height: bind screenHeight
            }
            largeImageFade = 0.0;
        }
        } else if (fullScreen) {
            fader.stop();
            largeImage = null;
            largeImageFade = 0;
            fullScreenTimeline.stop();
            x = boidx;
            y = boidy;
            nonFullScreenTimeline.start();
        }
    }

    public function onEnter():Void { 
        unscaleTimeline.stop();
        boidHeading = heading;
        spriteHeading = heading;


        x = spriteX;
        y = spriteY;
        textAlpha = 0;
        selected = true;
        boid.selectYourself();
scaleTimeline = Timeline {
        keyFrames:
        [KeyFrame {
                time: 0s;
                values: [alpha => 1.0, spriteHeading => boidHeading, textAlpha => 0.0]
        },
        KeyFrame {
                time: 0.5s;
                values: 
                [alpha => 2.0 tween Interpolator.EASEOUT,
                spriteHeading => 0.0 tween Interpolator.EASEOUT,
                textAlpha => 0.0]
                action: function() {
                    // hack
                    spriteHeading = 0.0; 
                    alpha = 2.0;
                }
        },
        KeyFrame {
                time: 1s;
                values: textAlpha => 1.0 tween Interpolator.EASEOUT
        }]
     };
     scaleTimeline.start();
    }

    public function onLeave():Void {
        if (not fullScreen and initialized) {
            scaleTimeline.stop();
            textAlpha = 0;
            unscaleTimeline = Timeline {
                keyFrames:
                [KeyFrame {
                        time: 0s;
                        values:
                        [alpha => 2.0,
                         spriteHeading => 0.0];
                    },
                    KeyFrame {
                        time: 0.5s;
                        values:
                        [alpha => 1.0 tween Interpolator.EASEOUT,
                         spriteHeading => boidHeading tween Interpolator.EASEOUT];
                        action: function() { 
                            System.out.println("UNSCALED");
                            fullScreen = false; 
                            selected = false; 
                            textAlpha = 0; 
                            alpha = 1.0;
                            spriteHeading = boidHeading;
                            boid.returnToFlocking(); 
                        }
                    }]
            };       
            unscaleTimeline.start();
        }
    }

    override attribute blocksMouse = true;

    override attribute mouseOver on replace {
        if (mouseOver and not fullScreen) {
            onEnter();
        } else {
	    onLeave();
        }

    }

    public function create():Node {
        var f = function(s:String):Void {
            doClick(s);
        };
        Group {
            transform: bind Transform.translate(spriteX, spriteY);
            content: 
            [Group {
                transform: bind Transform.scale(alpha, alpha)
                content:
                Group {
                 transform: bind Transform.rotate(if (not selected) heading else spriteHeading, r, r)
                content: 
                [Rectangle {
                    height: bind r*2
                    width: bind r*2
                    fill: Color.BLACK
                    smooth: false
                },
                Group {
                 content:
                 ImageView {
                      opacity: bind 1.0 - largeImageFade
                     image: bind image
                     onMouseClicked: function(e) {
                         this.doSelect();
                     }
                 }
               },
               Rectangle {
                    //smooth: false
                    transform: bind Transform.scale(1.0/alpha, 1.0/alpha);
                    stroke: Color.GRAY
                    height: bind r*2*alpha
                    width: bind r*2*alpha
                    fill: Color.color(0, 0, 0, 0);
                    strokeWidth: 1
               }]
               }
              },

              Group {
                  opacity: bind largeImageFade
                  visible: bind largeImage <> null
                  content: 
                  [Rectangle { height: bind screenHeight, width: bind screenWidth,
                              fill: Color.BLACK 
                      visible: bind largeImageFade == 1.0
                  },
                  ImageView { 
                      image: bind largeImage;
                      transform: Transform.scale(2.0, 2.0)
                  }]
             },
             Group {
                       visible: bind alpha == 2.0
                       var margin = bind r*2*alpha;
                       content:
                       bind if (boid.selected and sizeof boid.tags > 0)
                           TagList {
                               textOpacity: bind textAlpha
                               clickAction: f
                               tags: bind boid.tags[0..7]
                               height: 150;
                               width: 150;
                               transform: bind Transform.translate(margin, 0);
                           }
                       else null as Node
                    }]
                }
      }
}

class CrossHair extends CustomNode {
    attribute cx: Number;
    attribute cy: Number;
    attribute fill: Paint = Color.GRAY;
    attribute w: Number;
    attribute h: Number;
    attribute thickness: Number = 1;
    attribute dash: Number[];
    function create():Node {
        return Group {
            content:
            [Line {
                // transform: bind Transform.translate(0, cy);
                translateY: bind cy
                //strokeDashArray: bind dash
                stroke: bind fill
                strokeWidth: bind thickness
                // horizontal
                smooth: false
                startX: 0
                startY: 0
                endX: bind w
                endY: 0
                
            },
            Line {
                // transform: bind Transform.translate(cx, 0)
                translateX: bind cx
                //strokeDashArray: bind dash
                strokeWidth: bind thickness
                stroke: bind fill
                // vertical
                smooth: false
                startX: 0
                startY: 0
                endX: 0
                endY: bind h
            }]
        };
    }
}

class Vector3D {
    attribute x: Number;// on replace { System.out.println("x={x}"); }
    attribute y: Number;// on replace { System.out.println("y={y}"); }
    attribute z: Number;
    attribute heading2D: Number;

    function updateHeading2D():Void {
        heading2D = Math.toDegrees(-Math.atan2(-y, x));
    }

    function mult(n:Number):Void {
        x *= n;
        y *= n;
        z *= n;
    }

    function copy(): Vector3D {
        return Vector3D { x: x, y: y, z: z };
    }

    function add(vec:Vector3D):Void {
        x += vec.x;
        y += vec.y;
        z += vec.z;
    }

    function div(n:Number):Void {
        x /= n;
        y /= n;
        z /= n;
    }
    
    function normalize():Void {
        var  m = magnitude();
        if (m > 0) {
            div(m);
        }
    }

    function limit(max:Number):Void {
        if (magnitude() > max) {
            normalize();
            mult(max);
        }
    }

    //    function heading2D():Number {
    //    }

    function magnitude():Number {
        return Math.sqrt(x*x + y*y + z*z);
    }

    static function distance (v1:Vector3D, v2:Vector3D):Number {
        var dx = v1.x - v2.x;
        var dy = v1.y - v2.y;
        var dz = v1.z - v2.z;
        return Math.sqrt(dx*dx + dy*dy + dz*dz);
    }

    static function sub (v1:Vector3D, v2:Vector3D):Vector3D {
        var dx = v1.x - v2.x;
        var dy = v1.y - v2.y;
        var dz = v1.z - v2.z;
        return Vector3D {x: dx, y: dy, z: dz };
    }

    function setXYZ(x:Number, y:Number, z:Number):Void {
        this.x  = x;
        this.y = y;
        this.z = z;;
    }
        
    public function toString():String {"{x}, {y}, {z}";}
}

class PhotoModel {
      attribute thumbUrl: String;
      attribute photoUrl: String;
      attribute tags: String[];
}

class PhotoModelHolder {
      attribute photoModel: PhotoModel;
}

class Model {
    attribute COUNT: Integer;
    attribute tagMap: Map;
    attribute accessingFlickr:Boolean;
    attribute imageLoading:Boolean;
    attribute updating:Boolean;
    attribute queryString:String = " " on replace { if (queryString <> null) { doSearch(); }; }
    attribute photos: PhotoModelHolder[] = for (i in [1..COUNT]) PhotoModelHolder {};
    attribute page: Number = 1;
    attribute resultPage: Number;
    attribute total: Number;
    attribute currentTag: String;
    attribute searchCount:Integer = 0;
    attribute pool:ExecutorService = Executors.newCachedThreadPool(ThreadFactory {
            public function newThread(r:java.lang.Runnable):java.lang.Thread {
                var t= new java.lang.Thread(r);
                t.setPriority(java.lang.Thread.MIN_PRIORITY);
                return t;
            }
        });
    function next():Void {
        page++;
        queryString = currentTag;
        doSearch();
    }

    function back():Void {
        page--;
        queryString = currentTag;
        doSearch();
    }

    function search(value:String):Void  {
        queryString = value;
        page = 1;
        doSearch();
    }

    function postPhoto(count:Number, photo:Photo, n:Integer, m:Integer):Void {
       // System.out.println("post photo");
       // System.out.println("window={window}");
        if (searchCount == count) {
            var url = photo.getSmallSquareUrl();
            var farm = photo.getFarm();
            var server = photo.getServer();
            var id = photo.getId();
            var secret = photo.getSecret();
            var osecret = photo.getOriginalSecret(); 
            url = "http://farm{farm}.static.flickr.com/{server}/{id}_{secret}_s.jpg";
            var largeUrl:String = photo.getMediumUrl();
            var it = photo.getTags().iterator();
            var tags:String[];
            var tagCount = 0;

            tagMap.clear();
            while (it.hasNext()) {
                var t = it.next() as Tag;
                insert t.getValue().trim() into tags;
                tagCount++;
            }
            var p = PhotoModel {
                thumbUrl: url;
                photoUrl: largeUrl;                
                tags: tags;
            };
            if (n < COUNT) {
                photos[n].photoModel = p;
            }
            if (n == m-1) {
                for (i in [m..<COUNT]) {
                    photos[i].photoModel = null;
                    tagMap.remove(i);
                }
            }
        };
    }

    function doSearch():Void {
        searchCount++;
        if (queryString.trim().length() == 0) {
            //delete photos;
            return;
        }
        //delete photos;
        var value = queryString;
        System.out.println("search {value}");
        var params = new SearchParameters();
        params.setTags([value]);
        var result:PhotoList;
        var tagResult:RelatedTagsList;
        accessingFlickr = true;
        var photoList:Photo[];
        var count = searchCount;
        var t = java.lang.Runnable {
            public function run():Void {
                try {
                    java.lang.Thread.currentThread().setPriority(java.lang.Thread.MIN_PRIORITY);
                    var flickr:Flickr = new Flickr("c8e56e2c73b31a14061f0981baa23a2a");
                    result = flickr.getPhotosInterface().search(params, COUNT, page);
                    //                    System.out.println("got photos");
                    var iter = result.iterator();
                    var ii = 0;
                    var m = result.size();
                    while (iter.hasNext()) {
                        if (searchCount <> count) {
                            return;
                        }
                        var p = iter.next() as Photo;
                        //                        System.out.println("getting tags...");
                        var now:Number = System.currentTimeMillis();
                        var then_ = now + 1000.0;
                        var p1 = flickr.getTagsInterface().getListPhoto(p.getId());
                        p.setTags(p1.getTags());
                        var n = ii;
                        javax.swing.SwingUtilities.invokeLater(java.lang.Runnable {
                            public function run():Void {
                                postPhoto(count, p, n, m);
                            }
                        });
                        var left = then_ - System.currentTimeMillis();
                        ii++;
                    }
                    //tagResult = flickr.getTagsInterface().getRelated(value);
               } catch (e) {
                   e.printStackTrace();
               }
            }
        };
        pool.execute(t);
    }
}


class Boid {
  attribute id: Integer;
  attribute photoModel: PhotoModel;
  attribute tags: String[] = bind photoModel.tags;
  attribute imageUrl: String = bind photoModel.thumbUrl;
  attribute largeImageUrl: String = bind photoModel.photoUrl;
  attribute width: Number;
  attribute height: Number;
  attribute loc: Vector3D;
  attribute vel: Vector3D;
  attribute acc: Vector3D;
  attribute r: Number;
  attribute maxforce:Number;    // Maximum steering force
  attribute maxspeed: Number;    // Maximum speed
  attribute neighborDist: Number = bind r*2;
  attribute desiredseparation:Number;

  attribute saveVel:Vector3D = Vector3D {}

  attribute selected: Boolean = false;

  function selectYourself() {
      selected = true;
      saveVel.mult(0.0);
      saveVel.add(vel);
  }

  function returnToFlocking() {
      selected = false;
      //vel.mult(0.0);
      //vel.add(saveVel);
  }

  function random(lo:Number, hi:Number):Number {
      var res = lo + Math.random()*(hi-lo);
      return res;
  }

  init {
    acc = Vector3D {};
    vel = Vector3D { x: random(-1,1), y: random(-1,1) };
    loc = Vector3D { x: random(0,200), y: random(0,200) };
  }
  
    function run(boids:Boid[], cohere:function(b1:Boid, b2:Boid):Number):Void {
        if (not selected) {
            flock(boids, cohere);
            update();
            borders();
        }
    }
    
    // We accumulate a new acceleration each time based on three rules
    function flock(boids:Boid[], cohere:function(b1:Boid, b2:Boid):Number):Void {

        var sep = separate(boids);   // Separation
        //System.out.println("sep={sep}");
        var ali = align(boids);      // Alignment
        var coh = cohesion(boids, cohere);   // Cohesion
        // Arbitrarily weight these forces
        sep.mult(8.0);
        ali.mult(1.0);
        coh.mult(1.0);
        // Add the force vectors to acceleration
        acc.add(sep);
        acc.add(ali);
        acc.add(coh);
    }
    
    // Method to update location
    function update():Void {
        // Update velocity
        vel.add(acc);
        // Limit speed
        vel.limit(maxspeed);
        vel.updateHeading2D();
        loc.add(vel);
        // Reset accelertion to 0 each cycle
        acc.setXYZ(0,0,0);
    }
    
    function seek(target:Vector3D):Void {
        acc.add(steer(target,false));
    }
    
    function arrive(target:Vector3D):Void {
        acc.add(steer(target,true));
    }
    
    // A method that calculates a steering vector towards a target
    // Takes a second argument, if true, it slows down as it approaches the target
    function steer(target:Vector3D, slowdown:Boolean):Vector3D {
        var steer:Vector3D;  // The steering vector
        var desired = target.sub(target,loc);  // A vector pointing from the location to the target
        var d = desired.magnitude(); // Distance from the target is the magnitude of the vector
        // If the distance is greater than 0, calc steering (otherwise return zero vector)
        if (d > 0) {
            // Normalize desired
            desired.normalize();
            // Two options for desired vector magnitude (1 -- based on distance, 2 -- maxspeed)
            if ((slowdown) and (d > 100.0)) {desired.mult(maxspeed*(d/100.0));} // This damping is somewhat arbitrary
            else {desired.mult(maxspeed);}
            // Steering = Desired minus Velocity
            steer = target.sub(desired,vel);
            steer.limit(maxforce);  // Limit to maximum steering force
        } else {
            steer = Vector3D {};
        }
        return steer;
    }
    
    
    // Wraparound
    function borders():Void {
        if (loc.x < -r*2) loc.x = width+r*2;
        if (loc.y < -r*2) loc.y = height+r*2;
        if (loc.x > width+r*2) loc.x = -r*2;
        if (loc.y > height+r*2) loc.y = -r*2;
    }
    
    
    // Separation
    // Method checks for nearby boids and steers away
    function separate (boids:Boid[]):Vector3D {
        var sum = Vector3D {};
        var count = 0;
        // For every boid in the system, check if it's too close
        for (other in boids) {
            var d = loc.distance(loc,other.loc);
            // If the distance is greater than 0 and less than an arbitrary amount (0 when you are yourself)
            if ((d > 0) and (d < desiredseparation)) {
                // Calculate vector pointing away from neighbor
                var diff = loc.sub(loc,other.loc);
                diff.normalize();
                diff.div(d);        // Weight by distance
                sum.add(diff);
                count++;            // Keep track of how many
            }
        }
        // Average -- divide by how many
        if (count > 0) {
            sum.div(count);
        }
        return sum;
    }
  
    // Alignment
    // For every nearby boid in the system, calculate the average velocity
    function align (boids:Boid[]):Vector3D {

        var sum = Vector3D {};
        var count = 0;
        for (other in boids) {
            var d = loc.distance(loc,other.loc);
            if ((d > 0) and (d < neighborDist)) {
                sum.add(other.vel);
                count++;
            }
        }
        if (count > 0) {
            sum.div(count);
            sum.limit(maxforce);
        }
        return sum;
    }
    
    
    // Cohesion
    // For the average location (i.e. center) of all nearby boids, calculate steering vector towards that location
    function cohesion (boids:Boid[], cohere:function(b1:Boid,b2:Boid):Number):Vector3D {
        
        var neighbordist = this.neighborDist*6;
        var sum = Vector3D {};   // Start with empty vector to accumulate all locations
        var count = 0;
        for (other in boids) {
            var c = cohere(this, other);

            var d = loc.distance(loc,other.loc);
            if ((d > 0) and (d < neighbordist*c)) {
                //System.out.println("steering d={d} n={neighbordist*c}");
                sum.add(other.loc); // Add location
                count++;
            } else {
                //System.out.println("not steering d={d} n={neighbordist*c}");
            }
        }
        if (count > 0) {
            sum.div(count);
            return steer(sum,false);  // Steer towards the location
        }
        return sum;
    }
}

class FlickrFlockr extends CustomNode {   
    attribute fullScreen: Boolean;
    attribute hasSelection:Boolean = false on replace {
        if (fader <> null) {
            fader.start();
        }
    }

    static attribute selectedSprite: Sprite;
    static attribute largeImageVisible: Boolean = true;
    static attribute largeImageAlpha: Number;
    static attribute largeImage: Image;
    
    static function hideHighResolutionImage() : Void {
        FlickrFlockr.largeImageVisible = false;
        FlickrFlockr.largeImageAlpha = 0.0;
    }

    static function showHighResolutionImage(selSprite:Sprite) : Void {
        
        largeImageVisible = true;
        largeImageAlpha = 0.0;
        
        selectedSprite = selSprite;
        largeImage = selectedSprite.largeImage;
    }
    
    attribute maxSpeed: Integer = 2*10;
    attribute maxForce: Integer = 5; //0.05 * 100;
    attribute separation: Integer = 75*10;
    attribute alpha: Number = 0.8;
    attribute photoAlpha: Number = 0.0 on replace {
        //System.out.println("photo alpha = {photoAlpha}");
    };    
    attribute model: Model;
    attribute height: Number;
    attribute width: Number;
    attribute mouseX: Number;
    attribute mouseY: Number;
    attribute boids: Boid[];
    attribute BOID_COUNT = 25 on replace { createBoids(); };    
    attribute radius: Integer = (10*75/2.0).intValue();
    attribute selectedImageUrl:String = null;
    attribute fader: Timeline;
    attribute photoSelector: Timeline;
    attribute controlsVisible: Boolean = true;
    attribute spriteGroupVisible: Boolean = true;
    
    attribute tagMap:Map;
    attribute maxTags:Integer = 12;

    function toggleControlsVisible(): Void {
        controlsVisible = not controlsVisible;
    }
    
    function tagCohesion(boid1:Boid, boid2:Boid):Number {
      var tagCount:Number = (sizeof boid1.tags + sizeof boid2.tags);
      if (tagCount == 0) {
          return 1;
      }
      var map = tagMap.get(boid1.id) as Map;
      if (map == null) {
          map = new HashMap();
          tagMap.put(boid1.id, map);
      }
      var result = map.get(boid2.id) as java.lang.Double;
      if (result <> null) {
          return result.doubleValue();
      }
      var count = 0.0;
      for (t1 in boid1.tags where count < maxTags) {
          for (t2 in boid2.tags where count < maxTags) {
              if (t1 == t2) {
                  count++;
              }
          }
      }
      result =  Math.min(count, maxTags);
      map.put(boid2.id, new java.lang.Double(result));
      return result;
   }

   attribute timeline: Timeline = Timeline {
              keyFrames:
              KeyFrame {
                     time: 1s/60
                     action: function() {
                         run();
                     }
                     canSkip: true
              }
              repeatCount: Timeline.INDEFINITE
    }

   public function run(): Void {
        var sawSelected = false;
        for (b in boids) {
            b.run(boids, function(b1:Boid, b2:Boid):Number {
                    var c:Number = tagCohesion(b1, b2);
                    return if (maxTags == 0) 1 else c / maxTags;
                });
            if (b.selected) {
                sawSelected = true;
            }
        }
        if (selectedImageUrl == null) {
            hasSelection = sawSelected;
        }

    }

    init {
        javax.imageio.ImageIO.setUseCache(false);
        model = Model {
            tagMap: tagMap = new HashMap()
            COUNT: bind BOID_COUNT
        };
        
        spriteGroupVisible = false;
        createBoids();
        maxTags = 12;
        timeline.start();

        // To avoid display of initial frozen screen, 
        // make boids visible after 1 second
        var al = java.awt.event.ActionListener {
            public function actionPerformed(ae:java.awt.event.ActionEvent) {
                spriteGroupVisible = true;
            }
        };
        var timer = new javax.swing.Timer(1000, al);
        timer.setRepeats(false);
        timer.start();
    }

    bound function getPhoto(i:Integer) {
        if (i < sizeof model.photos) 
            model.photos[i].photoModel
        else 
            null
    }

    function createBoids():Void {
        if (model <> null) {
            boids = for (i in [0..<BOID_COUNT]) Boid {
                    id: i
                    r: bind radius/10.0
                    width: bind width;
                    height: bind height;
                    loc: Vector3D { x: width/2, y: height/2 };
                    maxspeed: bind maxSpeed/10.0
                    maxforce: bind maxForce/100.0
                    photoModel:  bind getPhoto(i)
                    desiredseparation: bind separation/10.0;
                };
        }
    }

    function deselectBoid() {
        if (fullScreen) {
            for (b in boids) b.selected = false;
        }
    }

    attribute textFieldWidth: Number = 0;
    attribute textFieldScale: Number = bind (width-40)/Math.max(textFieldWidth, width-40);
    attribute textField: TextField;

    function create():Node {
        var size = 150;
        textField = TextField {
            //border: new EmptyBorder
            text: model.queryString //with inverse  // ** with inverse doesn't work
            background: Color.color(0, 0, 0, 0)
            foreground: Color.WHITE
            font: Font {name: "Helvetica", size: size}
            //caretColor: Color.WHITE
        }
        textField.getJTextField().setPreferredSize(new java.awt.Dimension(5000, size + 20));
        var click = function (tag:String):Void {
            textField.text = tag;
            model.queryString = tag;
        }
        var select = function(url:String):Void {
             selectedImageUrl = url;
        }
        var tf = new javax.swing.JTextField();
        tf.putClientProperty("caretWidth", 4);
        tf.setBorder( new javax.swing.border.EmptyBorder(0,0,0,0) );
        tf.setFont(textField.getJTextField().getFont());

        var caret = textField.getJTextField().getCaret() as javax.swing.text.DefaultCaret;
        textField.action = function () { model.search(textField.text) };
        textField.getJTextField().putClientProperty("caretWidth", 4);
        textField.getJTextField().setBorder( new javax.swing.border.EmptyBorder(0,0,0,0) );
        textField.getJTextField().getDocument().addDocumentListener(javax.swing.event.DocumentListener {
                public function insertUpdate(e:javax.swing.event.DocumentEvent):Void {
                    tf.setText(textField.getJTextField().getText());
                    textFieldWidth = tf.getPreferredSize().width;
                    deselectBoid();
                    model.queryString = textField.getJTextField().getText();
                }
                public function removeUpdate(e:javax.swing.event.DocumentEvent):Void {
                    tf.setText(textField.getJTextField().getText());
                    textFieldWidth = tf.getPreferredSize().width;
                    deselectBoid();
                    model.queryString = textField.getJTextField().getText();
                }
                public function changedUpdate(e:javax.swing.event.DocumentEvent):Void {
                    tf.setText(textField.getJTextField().getText());
                    textFieldWidth = tf.getPreferredSize().width;
                    deselectBoid();
                    model.queryString = textField.getJTextField().getText();
                }
            });
            textField.getJTextField().setCaretColor(java.awt.Color.white);
            javax.swing.SwingUtilities.invokeLater(java.lang.Runnable {
                public function run():Void {
                    textField.getJTextField().requestFocus();
                    controlsVisible = false;
                }
            });

            textField.getJComponent().addKeyListener(java.awt.event.KeyAdapter {
                public function keyReleased(ke:java.awt.event.KeyEvent) {
                    if(ke.getKeyCode() == java.awt.event.KeyEvent.VK_F1) {
                        toggleControlsVisible();
                    }
                }
            });
        var comp:ComponentView;            
        var result = Group {
            clip: Rectangle { height: bind height, width: bind width}
            content:
            [Rectangle {
                height: bind height
                width: bind width
                fill: Color.BLACK
                smooth: false
            },
            Group {
                var sprites = bind for (b in boids) Sprite {
                        fullScreen: bind fullScreen with inverse
                        screenHeight: bind height
                        screenWidth: bind width
                        heading: bind b.vel.heading2D 
                        boid: b
                        imageUrl: bind b.imageUrl
                        clickAction: click
                        largeImageUrl: bind b.largeImageUrl;
                };
                //content: bind [sprites[s | not s.boid.selected], sprites[s | s.boid.selected]]
                content: 
                [Group {
                     content: bind sprites[s | not s.boid.selected]
                },
                Group {
                     content: bind sprites[s | s.boid.selected];
                }]
                visible: bind spriteGroupVisible
            },
            Group {
                blocksMouse: true
                visible: false
                onMouseClicked: function(e) {
                    photoSelector.start();
                    selectedImageUrl = null;
                }
                content: 
                [Rectangle {
                      height: bind height;
                      width: bind width;
                      fill: Color.BLACK
                      opacity: 0.8
                      smooth: false
                },
                Group {
                    opacity: bind photoAlpha
                    //transform: bind Transform.scale(s, s, 0);
                    content: ImageView {
                        image: bind if (selectedImageUrl == null) null else Image {
                            height: bind height, width: bind width
                            url: selectedImageUrl;
                            backgroundLoading: true;
                        }
                    }
                }]
            },
            Group {
                transform: Transform.translate(15, 200)
            content:
            comp = ComponentView {
                transform: bind [Transform.translate(0, 120), Transform.scale(textFieldScale, textFieldScale), Transform.translate(0, -50)]
                opacity: 0.8
                component: textField
            }},
            Rectangle {
                 cursor: Cursor.NONE
                 height: bind height
                 width: bind width
                 fill: Color.color(0, 0, 0, 0)
                onMouseMoved: function(e) {
                    mouseX = e.getX();
                    mouseY = e.getY();
                }
                onMouseDragged: function(e) {
                    mouseX = e.getX();
                    mouseY = e.getY();
                }
                smooth: false
                 
            },
            Group {
                visible: bind largeImageVisible
                content:
                    ImageView {
                        image: bind largeImage
                        onMouseClicked: function(e) {
                            selectedSprite.doSelect();
                        }
                     }
            },
            CrossHair {
                w: bind width * 2
                h: bind height * 2
                cx: bind mouseX
                cy: bind mouseY
            },
            ImageView {
                image: Image {
                    url: "{__DIR__}/close.gif"
                }
                translateX: bind (width - 25)
                translateY: 10
                cursor: Cursor.HAND
                onMouseClicked: function(e) {
                    System.exit(0);
                }
            },
            if (true) [] else
            Group {
                visible: bind controlsVisible        
                content:
            [MySlider {
                factor: 10.0
                translateX: 20
                translateY: bind height - 150;
                name: "Separation"
                value: bind separation with inverse
                min: 0
                max: 5000
                visible: bind controlsVisible
            },
            MySlider {
                factor: 10.0
                translateX: 20
                translateY: bind height - 120;
                name: "Radius"
                value: bind radius with inverse
                min: 0
                max: 5000
                visible: bind controlsVisible
            },
            MySlider {
                translateX: 20
                translateY: bind height - 90;
                name: "Boids"
                value: bind BOID_COUNT with inverse
                min: 0
                max: 100
                visible: bind controlsVisible
            },
            MySlider {
                factor: 10.0
                translateX: 20
                translateY: bind height - 60;
                name: "Speed"
                value: bind maxSpeed with inverse
                min: 0
                max: 100
                visible: bind controlsVisible
            },
            MySlider {
                factor: 100.0
                translateX: 20
                translateY: bind height - 30;
                name: "Force"
                value: bind maxForce with inverse
                min: 0
                max: 100
                visible: bind controlsVisible
            }]
          }]
        };
        comp.requestFocus();
        result;
    }
}

class MySlider extends CustomNode {
    attribute name: String;
    attribute value: Integer;
    attribute min: Integer = 0;
    attribute max: Integer = 100;
    attribute factor: Number = 1.0;
    function create():Node {
        var s  = Slider {
                        //background: Color.color(0,0,0,0);
                        foreground: Color.WHITE;
                        minimum: bind min;
                        maximum: bind max;
                        value: bind value with inverse;
                    }
        s.getJSlider().setBackground(new java.awt.Color(0,0,0,0));

        Group {
            content:
            [ComponentView {
                cursor: Cursor.DEFAULT
                component: 
                 Label {
                        text: bind name
                        foreground: Color.WHITE;
                 }
            },
            ComponentView {
                translateX: 120
                cursor: Cursor.DEFAULT
                component: 
                 Label {
                        text: bind "{value/factor}"
                        foreground: Color.WHITE;
                 }
            },
            ComponentView {
                translateX: 220
                cursor: Cursor.DEFAULT
                component: s
           }]
        }
    }
}

var canvas:Canvas;

Frame {
    title: "Flickr Flockr - JavaFX"
    height: 750, width: 1000
    background: Color.BLACK
    //undecorated: true
    visible: true;
    resizable: true;
    content: BorderPanel {
        center: canvas = Canvas {
              background: Color.BLACK
              content: FlickrFlockr {
                   height: bind canvas.height, width: bind canvas.width,
              };
        }
    }
    
    closeAction: function() { System.exit( 0 ); }
}
