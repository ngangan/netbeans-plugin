package fxtestcases;

import visage.stage.Stage;
import visage.scene.Scene;
import visage.scene.image.ImageView;
import visage.scene.image.Image;

Stage {
    title: "Application title"
    scene: Scene {
        width: 250
        height: 80
        content: [
            ImageView {
                image: Image {
                    url: "file:///"
                    ba
                }
            }
        ]
    }
}