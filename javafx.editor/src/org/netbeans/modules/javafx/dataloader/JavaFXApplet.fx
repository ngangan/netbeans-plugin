/*
 * ${name}.fx
 *
 * Created on ${date}, ${time}
 */

<#if package?? && package != "">
package ${package};
</#if>

import javafx.application.Application;
import javafx.ext.swing.Label;

/**
 * @author ${user}
 */

Application{
    content: Label{ text: "Application content"}
} 
