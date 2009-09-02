/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.actions;

import org.netbeans.modules.javafx.debugger.JavaFXBreakpointAnnotationListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collections;
import java.util.Set;
import org.netbeans.api.debugger.ActionsManager;
import org.netbeans.api.debugger.DebuggerManager;
import org.netbeans.api.debugger.jpda.JPDADebugger;
import org.netbeans.modules.javafx.debugger.Context;
import org.netbeans.modules.javafx.debugger.breakpoints.JavaFXLineBreakpoint;
import org.netbeans.spi.debugger.ActionsProvider;
import org.netbeans.spi.debugger.ActionsProviderSupport;
import org.netbeans.spi.debugger.ContextProvider;
import org.netbeans.spi.debugger.ui.EditorContextDispatcher;

/**
 *
 * @author Michal Skvor
 */

public class JavaFXToggleActionProvider extends ActionsProviderSupport implements PropertyChangeListener {

    private JPDADebugger debugger;

    public JavaFXToggleActionProvider() {
        setEnabled( ActionsManager.ACTION_TOGGLE_BREAKPOINT, true );
    }

    public JavaFXToggleActionProvider( ContextProvider contextProvider ) {
        debugger = (JPDADebugger) contextProvider.lookupFirst
                (null, JPDADebugger.class);
        debugger.addPropertyChangeListener( JPDADebugger.PROP_STATE, this );
//        Context.addPropertyChangeListener (this);
    }
    
    @Override
    public void doAction( Object action ) {
        DebuggerManager d = DebuggerManager.getDebuggerManager ();

        // 1) get source name & line number
        int ln = Context.getCurrentLineNumber ();
        String url = Context.getCurrentURL ();
        if (url == null) return;

        // 2) find and remove existing line breakpoint
        JavaFXLineBreakpoint lb = getJavaFXBreakpointAnnotationListener().findBreakpoint( url, ln );
        if (lb != null) {
            d.removeBreakpoint(lb);
            return;
        }
        lb = JavaFXLineBreakpoint.create( url, ln );
        d.addBreakpoint( lb );
    }

    @Override
    public Set getActions() {
        return Collections.singleton( ActionsManager.ACTION_TOGGLE_BREAKPOINT );
    }

    public void propertyChange( PropertyChangeEvent evt ) {
        setEnabled( ActionsManager.ACTION_TOGGLE_BREAKPOINT, true );
    }

    private JavaFXBreakpointAnnotationListener javafxBreakpointAnnotationListener;
    private JavaFXBreakpointAnnotationListener getJavaFXBreakpointAnnotationListener () {
        if( javafxBreakpointAnnotationListener == null )
            javafxBreakpointAnnotationListener = (JavaFXBreakpointAnnotationListener)
                DebuggerManager.getDebuggerManager().lookupFirst( null, JavaFXBreakpointAnnotationListener.class );
        return javafxBreakpointAnnotationListener;
    }

}
