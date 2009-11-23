import javafx.scene.*;
import javafx.stage.*;
import javafx.scene.text.*;
import javafx.scene.paint.*;
import javafx.scene.shape.*;
import javafx.scene.input.*;
import javafx.scene.control.*;

Stage {
   title : "MyApp"
   scene: Scene {
   width: 200
   height: 200
       content: [
           Text {
               font: Font { size: 24 }
               x: 10, y: 30
               content: "Hello World!"
           }
           Button {
                   text: "Button"
                   action: function() {
                       println("2 + 3 = {2 + 3}");
                       println("Hello World!")
                   }
           }
           Circle {
                   centerX: 100, centerY: 100
                   radius: 40
                   fill: Color.GREEN
                   onMouseClicked: function( e: MouseEvent ):Void {
                       var x = e.x;
                       var y = e.y;
                       println("x: {x}, y: {y}");
                   }
           }
       ]
   }
}