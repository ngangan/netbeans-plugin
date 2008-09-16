package helloworld;


import javafx.application.Frame;

import javafx.scene.Scene;

import javafx.scene.text.Text;

Frame {
    title: "Hello World JavaFX!"
    width: 300
    height: 200
    closeAction: function() {
        java.lang.System.exit( 0 );
    }
    visible: true

    scene: Scene{
        content: [
            Text {
                x: 10, y: 30
                content: "Hello World!"
            }
        ]
    }
}
