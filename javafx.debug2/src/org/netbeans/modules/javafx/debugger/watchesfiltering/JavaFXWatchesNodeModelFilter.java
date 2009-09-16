/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.watchesfiltering;

import org.netbeans.modules.javafx.debugger.variablesfiltering.*;
import java.awt.datatransfer.Transferable;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import org.netbeans.spi.debugger.ContextProvider;
import org.netbeans.spi.viewmodel.ExtendedNodeModel;
import org.netbeans.spi.viewmodel.ExtendedNodeModelFilter;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.NodeModel;
import org.netbeans.spi.viewmodel.UnknownTypeException;
import org.openide.util.RequestProcessor;
import org.openide.util.datatransfer.PasteType;

/**
 *
 * @author Michal Skvor
 */
public class JavaFXWatchesNodeModelFilter implements ExtendedNodeModelFilter {

    private RequestProcessor evaluationRP;
    private final Collection<ModelListener> modelListeners = new HashSet<ModelListener>();

    public JavaFXWatchesNodeModelFilter( ContextProvider lookupProvider ) {
        evaluationRP = lookupProvider.lookupFirst( null, RequestProcessor.class );
    }

    public String getDisplayName( NodeModel original, Object node ) throws UnknownTypeException {
        String dn = "";

        // Skip first $
        dn = original.getDisplayName( node );
        if( dn.startsWith( "$" )) dn = dn.substring( 1 );
        return dn;
    }

    public boolean canRename( ExtendedNodeModel original, Object node ) throws UnknownTypeException {
        return false;
    }

    public boolean canCopy( ExtendedNodeModel original, Object node ) throws UnknownTypeException {
        return false;
    }

    public boolean canCut( ExtendedNodeModel original, Object node ) throws UnknownTypeException {
        return false;
    }

    public Transferable clipboardCopy( ExtendedNodeModel original, Object node ) throws IOException, UnknownTypeException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Transferable clipboardCut( ExtendedNodeModel original, Object node ) throws IOException, UnknownTypeException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public PasteType[] getPasteTypes( ExtendedNodeModel original, Object node, Transferable t ) throws UnknownTypeException {
        return new PasteType[0];
    }

    public void setName( ExtendedNodeModel original, Object node, String name ) throws UnknownTypeException {
    }

    public String getIconBaseWithExtension( ExtendedNodeModel original, Object node ) throws UnknownTypeException {
        String ib = "";
        ib = original.getIconBaseWithExtension( node );
        return ib;
    }

    public String getIconBase( NodeModel original, Object node ) throws UnknownTypeException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public String getShortDescription( NodeModel original, Object node ) throws UnknownTypeException {
        String sd = "";

        return sd;
    }

    public void addModelListener( ModelListener l ) {
        modelListeners.add( l );
    }

    public void removeModelListener( ModelListener l ) {
        modelListeners.remove( l );
    }

}
