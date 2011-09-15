/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
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
