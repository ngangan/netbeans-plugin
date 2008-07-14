/*
 * ${name}.fx
 *
 * Created on ${date}, ${time}
 */

<#if package?? && package != "">
package ${package};
</#if>

import javafx.application.*;
import javafx.ext.swing.*;

/**
 * @author ${user}
 */

Application{
    content: Label{ text: "Application content"}
} 
