/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.breakpoints;

import org.netbeans.modules.javafx.debugger.Context;
import org.openide.util.NbBundle;

import org.netbeans.spi.viewmodel.NodeModel;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.UnknownTypeException;

/**
 *
 * @author Michal Skvor
 */
public class JavaFXBreakpointsNodeModel implements NodeModel {

    public static final String LINE_BREAKPOINT =
        "org/netbeans/modules/debugger/resources/breakpointsView/Breakpoint";

    public static final String LINE_BREAKPOINT_DISABLED =
        "org/netbeans/modules/debugger/resources/breakpointsView/DisabledBreakpoint";

    public String getDisplayName( Object o ) throws UnknownTypeException {
        if( o instanceof JavaFXLineBreakpoint ) {
            JavaFXLineBreakpoint b = (JavaFXLineBreakpoint) o;
            return NbBundle.getMessage( JavaFXBreakpointsNodeModel.class,
                    "CTL_JavaFX_Line_Breakpoint",
                    Context.getFileName( b ),
                    "" + b.getLineNumber()
                );
        }
        throw new UnknownTypeException( o );
    }

    public String getShortDescription( Object o ) throws UnknownTypeException {
        if( o instanceof JavaFXLineBreakpoint ) {
            return NbBundle.getMessage(
                    JavaFXBreakpointsNodeModel.class,
                    "CTL_JavaFX_Line_Breakpoint",
                    Context.getFileName((JavaFXLineBreakpoint)o ),
                    "" + ((JavaFXLineBreakpoint)o ).getLineNumber()
                );
        }
        throw new UnknownTypeException( o );
    }

    public String getIconBase( Object o ) throws UnknownTypeException {
        if( o instanceof JavaFXLineBreakpoint ) {
            JavaFXLineBreakpoint breakpoint = (JavaFXLineBreakpoint)o;
            if( breakpoint.isEnabled()) {
                return LINE_BREAKPOINT;
            } else {
                return LINE_BREAKPOINT_DISABLED;
            }
        }
        throw new UnknownTypeException( o );
    }

    /**
     *
     * @param l the listener to add
     */
    public void addModelListener( ModelListener l ) {
//        listeners.add (l);
    }

    /**
     *
     * @param l the listener to remove
     */
    public void removeModelListener( ModelListener l ) {
//        listeners.remove (l);
    }
}
