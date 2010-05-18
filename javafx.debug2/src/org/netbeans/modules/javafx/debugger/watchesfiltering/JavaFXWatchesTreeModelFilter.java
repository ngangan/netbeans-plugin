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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.netbeans.api.debugger.DebuggerManager;
import org.netbeans.api.debugger.Watch;
import org.netbeans.api.debugger.jpda.ClassVariable;
import org.netbeans.api.debugger.jpda.Field;
import org.netbeans.api.debugger.jpda.JPDADebugger;
import org.netbeans.spi.debugger.ContextProvider;
import org.netbeans.spi.debugger.DebuggerServiceRegistration;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.TreeModel;
import org.netbeans.spi.viewmodel.TreeModelFilter;
import org.netbeans.spi.viewmodel.UnknownTypeException;

/**
 *
 * @author Michal Skvor
 */
@DebuggerServiceRegistration( path="netbeans-JPDASession/FX/WatchesView", types={ org.netbeans.spi.viewmodel.TreeModelFilter.class } )
public class JavaFXWatchesTreeModelFilter implements TreeModelFilter {

    private final JPDADebugger debugger;
    private final Map<Watch, JavaFXWatch> watch2JavaFXWatch = new HashMap<Watch, JavaFXWatch>();
    private DebuggerListener listener;

    public JavaFXWatchesTreeModelFilter() {
        debugger = null;
    }

    public JavaFXWatchesTreeModelFilter( ContextProvider lookupProvider ) {
        debugger = (JPDADebugger)lookupProvider.lookupFirst( null, JPDADebugger.class );
    }

    public Object getRoot( TreeModel original ) {
        return original.getRoot();
    }

    public Object[] getChildren( TreeModel original, Object parent, int from, int to ) throws UnknownTypeException {
        if( parent == original.getRoot()) {
            Watch [] allWatches = DebuggerManager.getDebuggerManager().getWatches();
            Object [] result = original.getChildren( parent, from, to );

            //original model returns array of JPDAWatch-es, thus we must create an Object array
            //to allow merging with JspElWatch-es
            Object[] ch = new Object[result.length];
            System.arraycopy( result, 0, ch, 0, result.length );

            synchronized( watch2JavaFXWatch ) {
                for( int i = from; i < allWatches.length; i++ ) {
                    Watch w = allWatches[i];
                    String expression = w.getExpression();
                    if( isJavaFXexpression( expression )) {
                        JavaFXWatch jw = watch2JavaFXWatch.get( w );
                        if( jw == null ) {
                            jw = new JavaFXWatch( w, debugger );
                            watch2JavaFXWatch.put( w, jw );
                        }
                        ch[i - from] = jw;
                    }
                }
            }

            if( listener == null ) {
                listener = new DebuggerListener( this, debugger );
            }

            return ch;
        } else if( parent instanceof JavaFXWatch ) {
            JavaFXWatch w = (JavaFXWatch)parent;
            // Filter variables
            Object children[] = original.getChildren( w.getVariable(), from, to );
            List vc = new ArrayList();
            for( int i = 0; i < children.length; i++ ) {
                Object obj = children[i];
                if( obj instanceof ClassVariable ) {
                    ClassVariable cv = (ClassVariable)obj;
//                        vc.add( obj );
                } else if( obj instanceof Field ) {
                    Field f = (Field)obj;
                    if( f.getName().startsWith( "$" )) {
                        vc.add( obj );
                    }
                }
            }
            return vc.subList( from, to > vc.size() ? vc.size() : to ).toArray();
        } else {
            return original.getChildren( parent, from, to );
        }
    }

    public int getChildrenCount( TreeModel original, Object node ) throws UnknownTypeException {
        if( node == original.getRoot() && listener == null ) {
            listener = new DebuggerListener( this, debugger );
        } else if( node instanceof JavaFXWatch ) {
            JavaFXWatch w = (JavaFXWatch)node;
            if( w.getValue() != null && w.getVariable() != null )
                return original.getChildrenCount( w.getVariable());
        }
        return original.getChildrenCount( node );
    }

    public boolean isLeaf( TreeModel original, Object node ) throws UnknownTypeException {
        boolean il = true;
        if( node == original.getRoot()) {
            il = false;
        } else if( node instanceof JavaFXWatch ) {
            JavaFXWatch w = (JavaFXWatch)node;
            il = false;
//            if( w.getVariable() != null )
//                il = original.isLeaf( w.getVariable());
        }
        return il;
    }

    private boolean isJavaFXexpression( String expression ) {
        return true;
    }

    public void addModelListener( ModelListener l ) {
    }

    public void removeModelListener( ModelListener l ) {
    }

    void fireTreeChanged() {
        synchronized( watch2JavaFXWatch ) {
            for( JavaFXWatch javafxWatch : watch2JavaFXWatch.values()) {
                javafxWatch.setUnevaluated();
            }
        }
    }

    private static class DebuggerListener implements PropertyChangeListener {

        WeakReference<JavaFXWatchesTreeModelFilter> javafxWatchesFilterRef;
        WeakReference<JPDADebugger> debuggerRef;

        DebuggerListener( JavaFXWatchesTreeModelFilter javafxWatchesFilter, JPDADebugger debugger ) {
            javafxWatchesFilterRef = new WeakReference<JavaFXWatchesTreeModelFilter>( javafxWatchesFilter );
            debuggerRef = new WeakReference<JPDADebugger>( debugger );
            debugger.addPropertyChangeListener( this );
        }

        public void propertyChange( PropertyChangeEvent evt ) {

            if( debuggerRef.get().getState() == JPDADebugger.STATE_DISCONNECTED ) {
                destroy();
                return;
            }
            if( debuggerRef.get().getState() == JPDADebugger.STATE_RUNNING ) {
                return;
            }

            final JavaFXWatchesTreeModelFilter javafxWatchesFilter = getJavaFXWatchesFilter();
            if( javafxWatchesFilter != null ) {
                javafxWatchesFilter.fireTreeChanged();
            }
        }

        private JavaFXWatchesTreeModelFilter getJavaFXWatchesFilter() {
            JavaFXWatchesTreeModelFilter javafxWatchesFilter = javafxWatchesFilterRef.get();
            if( javafxWatchesFilter == null ) {
                destroy();
            }
            return javafxWatchesFilter;
        }

        private void destroy() {
            JPDADebugger debugger = debuggerRef.get();
            if( debugger != null ) {
                debugger.removePropertyChangeListener( this );
            }
        }
    }
}
