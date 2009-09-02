/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger;

import org.netbeans.api.debugger.DebuggerManager;
import org.netbeans.api.debugger.LazyActionsManagerListener;
import org.netbeans.api.debugger.jpda.JPDADebugger;
import org.netbeans.spi.debugger.ContextProvider;

/**
 *
 * @author Michal Skvor
 */
public class JavaFXBreakpointUpdater extends LazyActionsManagerListener {

    private JPDADebugger debugger;

    public JavaFXBreakpointUpdater( ContextProvider lookupProvider ) {
        JPDADebugger debugger = (JPDADebugger) lookupProvider.lookupFirst (
            null, JPDADebugger.class
        );
        this.debugger = debugger;
        Context.createTimeStamp( debugger );
        JavaFXBreakpointAnnotationListener bal =
            (JavaFXBreakpointAnnotationListener)DebuggerManager.getDebuggerManager().
            lookupFirst( null, JavaFXBreakpointAnnotationListener.class );
        bal.updateJavaFXLineBreakpoints();
    }

    protected void destroy() {
        Context.disposeTimeStamp( debugger );
    }

    public String[] getProperties() {
        return new String[0];
    }
}
