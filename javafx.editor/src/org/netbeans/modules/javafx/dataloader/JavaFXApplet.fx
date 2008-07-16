/*
 * ${name}.fx
 *
 * Created on ${date}, ${time}
 */

<#if package?? && package != "">
package ${package};
</#if>

import javafx.application.*;
import javafx.scene.*;
import javafx.scene.text.*;

/**
 * @author ${user}
 */

Application{
    stage: Stage{
        content: Text {
            x: 10, y: 30
            content: "Application content"
            font: Font { size: 24  style: FontStyle.PLAIN }
        }
    }
} 