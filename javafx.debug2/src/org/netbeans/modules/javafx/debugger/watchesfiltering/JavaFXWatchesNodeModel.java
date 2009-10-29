/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.watchesfiltering;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import org.netbeans.api.debugger.jpda.InvalidExpressionException;
import org.netbeans.spi.debugger.ContextProvider;
import org.netbeans.spi.debugger.DebuggerServiceRegistration;
import org.netbeans.spi.viewmodel.ModelEvent;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.NodeModel;
import org.netbeans.spi.viewmodel.UnknownTypeException;
import org.openide.util.RequestProcessor;

/**
 *
 * @author Michal Skvor
 */
@DebuggerServiceRegistration( path="netbeans-JPDASession/FX/WatchesView", types={ org.netbeans.spi.viewmodel.NodeModel.class } )
public class JavaFXWatchesNodeModel implements NodeModel {

    private static final String ICON_BASE ="org/netbeans/modules/debugger/resources/watchesView/Watch";

    private final Collection<ModelListener> modelListeners = new HashSet<ModelListener>();
    private final Map<JavaFXWatch, String> shortDescriptionMap = new HashMap<JavaFXWatch, String>();
    private RequestProcessor evaluationRP;

    public JavaFXWatchesNodeModel() {}
    
    public JavaFXWatchesNodeModel( ContextProvider lookupProvider ) {
        evaluationRP = lookupProvider.lookupFirst( null, RequestProcessor.class );
    }

    public String getDisplayName( Object node ) throws UnknownTypeException {
        if( !( node instanceof JavaFXWatch )) throw new UnknownTypeException( node );
        JavaFXWatch watch = (JavaFXWatch) node;
        return watch.getExpression();
    }

    public String getIconBase( Object node ) throws UnknownTypeException {
        if(!( node instanceof JavaFXWatch )) throw new UnknownTypeException( node );
        return ICON_BASE;
    }

    public String getShortDescription( Object node ) throws UnknownTypeException {
        if( !( node instanceof JavaFXWatch )) throw new UnknownTypeException( node );
        final JavaFXWatch watch = (JavaFXWatch) node;

        synchronized( shortDescriptionMap ) {
            String shortDescription = shortDescriptionMap.remove( watch );
            if( shortDescription != null ) {
                return shortDescription;
            }
        }
        // Called from AWT - we need to postpone the work...
        evaluationRP.post( new Runnable() {
            public void run() {
                String shortDescription = getShortDescriptionSynch( watch );
                if (shortDescription != null && !"".equals(shortDescription)) {
                    synchronized( shortDescriptionMap ) {
                        shortDescriptionMap.put( watch, shortDescription );
                    }
                    fireModelChange( new ModelEvent.NodeChanged( JavaFXWatchesNodeModel.this,
                        watch, ModelEvent.NodeChanged.SHORT_DESCRIPTION_MASK ));
                }
            }
        });
        return "";
    }

    private static String getShortDescriptionSynch( JavaFXWatch watch ) {
        String t = watch.getType ();
        String e = watch.getExceptionDescription ();
        if (e != null) {
            return watch.getExpression() + " = >" + e + "<";
        }
        if (t == null) {
            return watch.getExpression() + " = " + watch.getValue();
        } else {
            try {
                return watch.getExpression() + " = (" + watch.getType () + ") " + watch.getToStringValue();
            } catch( InvalidExpressionException ex ) {
                return ex.getLocalizedMessage ();
            }
        }
    }

    public void addModelListener( ModelListener l ) {
        synchronized( modelListeners ) {
            modelListeners.add( l );
        }
    }

    public void removeModelListener( ModelListener l ) {
        synchronized( modelListeners ) {
            modelListeners.remove( l );
        }
    }

    protected void fireModelChange( ModelEvent me ) {
        ModelListener[] listeners;
        synchronized( modelListeners ) {
            listeners = modelListeners.toArray( new ModelListener[] {} );
        }
        for( int i = 0; i < listeners.length; i++ ) {
            listeners[i].modelChanged( me );
        }
    }
}
