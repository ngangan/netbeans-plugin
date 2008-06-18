/*
 * ${name}.fx
 *
 * Created on ${date}, ${time}
 */

<#if package?? && package != "">
package ${package};
</#if>

import javafx.gui.Application;
import javafx.gui.swing.Label;

/**
 * @author ${user}
 */

Application{
    content: Label{ text: "Application content"}
} 
