/*
 * ${name}.fx
 *
 * Created on ${date}, ${time}
 */

<#if package?? && package != "">
package ${package};
</#if>

import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.text.Text;
import javafx.scene.text.Font;

/**
 * @author ${user}
 */

Stage {
    title: "Application title"
    width: 250
    height: 80
    scene: Scene {
        content: Text {
            Font {
                size : 24
            }
            x: 10, y: 30
            content: "Application content"
        }
    }
}