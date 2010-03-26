/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package qa.javafx.functional.library.operator;

import java.awt.Component;
import org.netbeans.jemmy.ComponentChooser;

/**
 *
 * @author andromeda
 */

public class ClassNameComponentChooser implements ComponentChooser {

    String name;
    String text;

    public ClassNameComponentChooser(String name) {
        this(name, "");
    }

    public ClassNameComponentChooser(String name, String text) {
        this.name = name;
        this.text = text;
    }

    public boolean checkComponent(Component component) {
        String description = component.toString();
        //System.out.println("[ClassNameComponentChooser] check: " + description);
        //if(description.contains("JList")){
            //System.out.println("[ClassNameComponentChooser] check: " + description);
        //}
        return description.contains(name) && description.contains(text);
    }

    public String getDescription() {
        return "[ClassNameComponentChooser] name: \"" + name + "\" text: \"" + text + "\"";
    }
}
