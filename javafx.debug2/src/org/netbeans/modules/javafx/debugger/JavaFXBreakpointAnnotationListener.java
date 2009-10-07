/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger;

import java.beans.PropertyChangeEvent;
import java.util.HashMap;
import java.util.Iterator;
import org.netbeans.api.debugger.Breakpoint;
import org.netbeans.api.debugger.DebuggerEngine;
import org.netbeans.api.debugger.DebuggerManager;
import org.netbeans.api.debugger.DebuggerManagerAdapter;
import org.netbeans.api.debugger.jpda.JPDADebugger;
import org.netbeans.modules.javafx.debugger.Context;
import org.netbeans.modules.javafx.debugger.breakpoints.JavaFXLineBreakpoint;

/**
 *
 * @author Michal Skvor
 */
public class JavaFXBreakpointAnnotationListener extends DebuggerManagerAdapter {

    private HashMap breakpointToAnnotation = new HashMap();
    private boolean listen = true;

    @Override
    public String[] getProperties() {
        return new String[] { DebuggerManager.PROP_BREAKPOINTS };
    }

    /**
     * Listens on breakpoint.
     */
    @Override
    public void propertyChange( PropertyChangeEvent e ) {
        String propertyName = e.getPropertyName ();
        if( propertyName == null ) return;
        if( !listen ) return;
        if(( !propertyName.equals( JavaFXLineBreakpoint.PROP_ENABLED )) &&
           ( !propertyName.equals( JavaFXLineBreakpoint.PROP_LINE_NUMBER ))) return;

        JavaFXLineBreakpoint b = (JavaFXLineBreakpoint)e.getSource();
        annotate( b );
    }

    /**
    * Called when some breakpoint is added.
    *
    * @param b breakpoint
    */
    @Override
    public void breakpointAdded( Breakpoint b ) {
        if( b instanceof JavaFXLineBreakpoint ) {
            ((JavaFXLineBreakpoint)b ).addPropertyChangeListener( this );
            annotate((JavaFXLineBreakpoint)b );
        }
    }

    /**
    * Called when some breakpoint is removed.
    *
    * @param breakpoint
    */
    @Override
    public void breakpointRemoved( Breakpoint b ) {
        if( b instanceof JavaFXLineBreakpoint ) {
            JavaFXLineBreakpoint bb = (JavaFXLineBreakpoint)b;
            bb.removePropertyChangeListener( this );
            bb.removeLineBreakpoint();
            removeAnnotation( bb );
        }
    }

    public JavaFXLineBreakpoint findBreakpoint( String url, int lineNumber ) {
        Iterator i = breakpointToAnnotation.keySet().iterator();
        while( i.hasNext()) {
            JavaFXLineBreakpoint lb = (JavaFXLineBreakpoint)i.next();
            if( !lb.getURL().equals( url )) continue;
            Object annotation = breakpointToAnnotation.get( lb );
            int ln = Context.getLineNumber( annotation, null );
            if( ln == lineNumber ) return lb;
        }
        return null;
    }

    private void annotate( JavaFXLineBreakpoint b ) {
        // remove old annotation
        Object annotation = breakpointToAnnotation.get (b);
        if( annotation != null )
            Context.removeAnnotation( annotation );
        if( b.isHidden()) return;

        // add new one
        annotation = Context.annotate( b );
        if( annotation == null )
            return;

        breakpointToAnnotation.put( b, annotation );

        DebuggerEngine de =
                DebuggerManager.getDebuggerManager().getCurrentEngine();
        Object timeStamp = null;
        if( de != null )
            timeStamp = de.lookupFirst( null, JPDADebugger.class );
        update( b, timeStamp );
    }

    public void updateJavaFXLineBreakpoints () {
        Iterator it = breakpointToAnnotation.keySet().iterator();
        while( it.hasNext()) {
            JavaFXLineBreakpoint lb = (JavaFXLineBreakpoint)it.next();
            update( lb, null );
        }
    }

    private void update( JavaFXLineBreakpoint b, Object timeStamp ) {
        Object annotation = breakpointToAnnotation.get( b );
        if( annotation == null )
            return;
        int ln = Context.getLineNumber( annotation, timeStamp );
        listen = false;
        b.setLineNumber( ln );
        listen = true;
    }

    private void removeAnnotation( JavaFXLineBreakpoint b ) {
        Object annotation = breakpointToAnnotation.remove( b );
        if( annotation != null )
            Context.removeAnnotation( annotation );
    }

}
