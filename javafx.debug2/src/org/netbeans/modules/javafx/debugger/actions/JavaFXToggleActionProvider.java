/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.actions;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.Set;
import org.netbeans.api.debugger.ActionsManager;
import org.netbeans.api.debugger.Breakpoint;
import org.netbeans.api.debugger.DebuggerManager;
import org.netbeans.api.debugger.jpda.JPDADebugger;
import org.netbeans.modules.javafx.debugger.Context;
import org.netbeans.modules.javafx.debugger.breakpoints.JavaFXLineBreakpoint;
import org.netbeans.spi.debugger.ActionsProvider.Registration;
import org.netbeans.spi.debugger.ActionsProviderSupport;
import org.netbeans.spi.debugger.ContextProvider;
import org.netbeans.spi.debugger.ui.EditorContextDispatcher;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.URLMapper;

/**
 *
 * @author Michal Skvor
 */
@Registration(path="")
public class JavaFXToggleActionProvider extends ActionsProviderSupport implements PropertyChangeListener {

    private JPDADebugger debugger;

    public JavaFXToggleActionProvider() {
        propertyChange( null );
        Context.addPropertyChangeListener( this );
    }

    public JavaFXToggleActionProvider( ContextProvider contextProvider ) {
        debugger = contextProvider.lookupFirst(null, JPDADebugger.class);
        debugger.addPropertyChangeListener( JPDADebugger.PROP_STATE, this );
        Context.addPropertyChangeListener( this );
    }
    
    @Override
    public void doAction( Object action ) {
        DebuggerManager d = DebuggerManager.getDebuggerManager ();

        // 1) get source name & line number
        int ln = Context.getCurrentLineNumber ();
        String url = Context.getCurrentURL ();
        if (url == null) return;

        // 2) find and remove existing line breakpoint
        JavaFXLineBreakpoint lb = findBreakpoint( url, ln );
        if (lb != null) {
            d.removeBreakpoint( lb );
            return;
        }
        lb = JavaFXLineBreakpoint.create( url, ln );
        d.addBreakpoint( lb );
    }

    static JavaFXLineBreakpoint findBreakpoint( String url, int lineNumber ) {
        Breakpoint[] breakpoints = DebuggerManager.getDebuggerManager().getBreakpoints();
        for( int i = 0; i < breakpoints.length; i++ ) {
            if( !( breakpoints[i] instanceof JavaFXLineBreakpoint )) {
                continue;
            }
            JavaFXLineBreakpoint lb = (JavaFXLineBreakpoint) breakpoints[i];
            if( !lb.getURL().equals( url )) continue;
            if( lb.getLineNumber() == lineNumber ) {
                return lb;
            }
        }
        return null;
    }

    @Override
    public Set getActions() {
        return Collections.singleton( ActionsManager.ACTION_TOGGLE_BREAKPOINT );
    }

    public void propertyChange( PropertyChangeEvent evt ) {
        String url = EditorContextDispatcher.getDefault().getCurrentURLAsString();
        FileObject fo;
        try {
            fo = URLMapper.findFileObject(new URL(url));
        } catch (MalformedURLException muex) {
            fo = null;
        }
        setEnabled( ActionsManager.ACTION_TOGGLE_BREAKPOINT,
            (EditorContextDispatcher.getDefault().getCurrentLineNumber () >= 0) &&
            (fo != null && "text/x-fx".equals(fo.getMIMEType())));    
    }    
}
