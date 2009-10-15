/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.breakpoints;

import org.netbeans.api.debugger.Breakpoint;
import org.netbeans.api.debugger.Properties;
import org.netbeans.modules.javafx.debugger.utils.Utils;

/**
 *
 * @author Michal Skvor
 */
public class JavaFXBreakpointsReader implements Properties.Reader {

    public String[] getSupportedClassNames() {
        return new String[] { JavaFXLineBreakpoint.class.getName() };
    }

    public Object read( String className, Properties properties ) {
        JavaFXLineBreakpoint b = null;
        if( className.equals( JavaFXLineBreakpoint.class.getName())) {
            String url = properties.getString( JavaFXLineBreakpoint.PROP_URL, null );
            if( url == null || url.trim().length() == 0 ) {
                return null;
            }
            // Check if file exists
            if( Utils.getFXPath( url ) == null ) return null;

            b = JavaFXLineBreakpoint.create( url,
                properties.getInt( JavaFXLineBreakpoint.PROP_LINE_NUMBER, 1 )
            );
            b.setCondition(properties.getString( JavaFXLineBreakpoint.PROP_CONDITION, "" ));
            b.setPrintText(properties.getString( JavaFXLineBreakpoint.PROP_PRINT_TEXT, "" ));
            b.setGroupName(properties.getString( Breakpoint.PROP_GROUP_NAME, "" ));
            b.setSuspend(properties.getInt( JavaFXLineBreakpoint.PROP_SUSPEND, JavaFXLineBreakpoint.SUSPEND_ALL ));
            if (properties.getBoolean( JavaFXLineBreakpoint.PROP_ENABLED, true )) {
                b.enable ();
            } else {
                b.disable ();
            }
            
        }
        return b;
    }

    public void write( Object object, Properties properties ) {
        if( object instanceof JavaFXLineBreakpoint ) {
            JavaFXLineBreakpoint b = (JavaFXLineBreakpoint)object;
            properties.setString( JavaFXLineBreakpoint.PROP_PRINT_TEXT, b.getPrintText());
            properties.setString( JavaFXLineBreakpoint.PROP_GROUP_NAME, b.getGroupName());
            properties.setInt( JavaFXLineBreakpoint.PROP_SUSPEND, b.getSuspend());
            properties.setBoolean( JavaFXLineBreakpoint.PROP_ENABLED, b.isEnabled());
            properties.setString( JavaFXLineBreakpoint.PROP_URL, b.getURL());
            properties.setInt( JavaFXLineBreakpoint.PROP_LINE_NUMBER, b.getLineNumber());
            properties.setString( JavaFXLineBreakpoint.PROP_CONDITION, b.getCondition());
        }
        return;
    }
}
