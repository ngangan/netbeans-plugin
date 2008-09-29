/*
 * ${name}.fx
 *
 * Created on ${date}, ${time}
 */

<#if package?? && package != "">
package ${package};
</#if>

import javafx.stage.*;
import javafx.scene.*;
import javafx.scene.text.*;

/**
 * @author ${user}
 */

Stage{
    visible: true
    decoration: Decoration{ title: "Application title" }
    width: 250
    height: 80
    scene: Scene{
        content: Text {
            x: 10, y: 30
            content: "Application content"
            font: Font { size: 24 }
        }
    }
}