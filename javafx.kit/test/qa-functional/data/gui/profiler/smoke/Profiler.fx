package smokeprofiler;


import javafx.application.Frame;

import javafx.scene.Scene;

import javafx.scene.paint.Color;
import javafx.scene.geometry.Circle;
import javafx.scene.transform.Translate;


import javafx.animation.Timeline;
import javafx.animation.KeyFrame;

import java.lang.Math;

var radius = 50;

var angle = 0.0;
var frequency = 0.5;

var timeline = Timeline {
    repeatCount: Timeline.INDEFINITE
    keyFrames : [
        KeyFrame {
            time : 0.05s
            action: function() {
                angle+= 0.5;
            }
        }
    ]
}

timeline.start();

Frame {
    title: "Cicrcle"
    width: 300
    height: 300
    closeAction: function() {  java.lang.System.exit( 0 ); }
    visible: true

    scene: Scene{
        content: [
            Circle {
              transforms: Translate { x : 150, y : 150 }
              centerX: bind radius * Math.cos(frequency * angle)
              centerY: bind radius * Math.sin(frequency * angle)
              radius: 10
              fill: Color.ORANGE
            }]
    }
}
