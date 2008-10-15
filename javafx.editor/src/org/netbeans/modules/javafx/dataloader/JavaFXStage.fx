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

/**
 * @author ${user}
 */

Stage{
    title: "Application title"
    width: 250
    height: 80
    scene: Scene{
        content: Text {
            x: 10, y: 30
            content: "Application content"
        }
    }
}