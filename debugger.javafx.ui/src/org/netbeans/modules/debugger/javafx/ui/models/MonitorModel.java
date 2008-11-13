/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
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

package org.netbeans.modules.debugger.javafx.ui.models;

import com.sun.jdi.ObjectCollectedException;
import com.sun.jdi.VMDisconnectedException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import javax.swing.Action;
import org.netbeans.api.debugger.javafx.InvalidExpressionException;

import org.netbeans.api.debugger.javafx.JavaFXThread;
import org.netbeans.api.debugger.javafx.JavaFXThreadGroup;
import org.netbeans.api.debugger.javafx.ObjectVariable;
import org.netbeans.spi.debugger.ui.Constants;
import org.netbeans.spi.viewmodel.ModelEvent;
import org.netbeans.spi.viewmodel.NodeActionsProvider;
import org.netbeans.spi.viewmodel.NodeActionsProviderFilter;
import org.netbeans.spi.viewmodel.NodeModel;
import org.netbeans.spi.viewmodel.NodeModelFilter;
import org.netbeans.spi.viewmodel.TableModel;
import org.netbeans.spi.viewmodel.TreeModel;
import org.netbeans.spi.viewmodel.TreeModelFilter;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.UnknownTypeException;
import org.openide.util.NbBundle;
import org.openide.util.RequestProcessor;


/**
 * @author   Jan Jancura
 */
public class MonitorModel implements TreeModelFilter, NodeModelFilter, 
NodeActionsProviderFilter, TableModel, Constants {

    public static final String CONTENDED_MONITOR =
        "org/netbeans/modules/debugger/resources/allInOneView/ContendedMonitor"; // NOI18N
    public static final String OWNED_MONITORS =
        "org/netbeans/modules/debugger/resources/allInOneView/OwnedMonitors"; // NOI18N
    public static final String MONITOR =
        "org/netbeans/modules/debugger/resources/allInOneView/Monitor"; // NOI18N

    private RequestProcessor evaluationRP = new RequestProcessor();
    private final Collection modelListeners = new HashSet();
    
    // TreeView impl............................................................
    
    public Object getRoot (TreeModel model) {
        return model.getRoot ();
    }
    
    public Object[] getChildren (
        TreeModel   model, 
        Object      o, 
        int         from, 
        int         to
    ) throws UnknownTypeException {
        if (o instanceof ThreadWithBordel) {
            try {
                JavaFXThread t = ((ThreadWithBordel) o).originalThread;
                ObjectVariable contended = t.getContendedMonitor ();
                ObjectVariable[] owned = t.getOwnedMonitors ();
                Object cm = null;
                Object om = null;
                if ( (contended != null) &&
                     (from  == 0) && (to > 0)
                ) cm = new ContendedMonitor (contended);
                if ( (owned.length > 0) &&
                     ( ((contended != null) && (from < 2) && (to > 1)) ||
                       ((contended == null) && (from == 0) && (to > 0))
                     )
                ) om = new OwnedMonitors (owned);
                int i = 0;
                if (cm != null) i++;
                if (om != null) i++;
                Object[] os = new Object [i];
                i = 0;
                if (cm != null) os[i++] = cm;
                if (om != null) os[i++] = om;
                return os;
            } catch (ObjectCollectedException e) {
            } catch (VMDisconnectedException e) {
            }
            return new Object [0];
        }
        if (o instanceof JavaFXThreadGroup) {
            JavaFXThreadGroup tg = (JavaFXThreadGroup) o;
            Object[] ch = model.getChildren (o, from, to);
            int i, k = ch.length;
            for (i = 0; i < k; i++) {
                if (!(ch [i] instanceof JavaFXThread)) continue;
                try {
                    JavaFXThread t = (JavaFXThread) ch [i];
                    if (t.getContendedMonitor () == null &&
                        t.getOwnedMonitors ().length == 0
                    ) continue;
                    ThreadWithBordel twb = new ThreadWithBordel ();
                    twb.originalThread = t;
                    ch [i] = twb;
                } catch (ObjectCollectedException e) {
                } catch (VMDisconnectedException e) {
                }
            }
            return ch;
        }
        if (o instanceof OwnedMonitors) {
            OwnedMonitors om = (OwnedMonitors) o;
            Object[] fo = new Object [to - from];
            System.arraycopy (om.variables, from, fo, 0, to - from);
            return fo;
        }
        return model.getChildren (o, from, to);
    }
    
    public int getChildrenCount (
        TreeModel   model, 
        Object      o
    ) throws UnknownTypeException {
        if (o instanceof ThreadWithBordel) {
            // Performance, see issue #59058.
            return Integer.MAX_VALUE;
            /*
            try {
                JavaFXThread t = ((ThreadWithBordel) o).originalThread;
                ObjectVariable contended = t.getContendedMonitor ();
                ObjectVariable[] owned = t.getOwnedMonitors ();
                int i = 0;
                if (contended != null) i++;
                if (owned.length > 0) i++;
                return i;
            } catch (ObjectCollectedException e) {
            } catch (VMDisconnectedException e) {
            }
            return 0;
             */
        }
        if (o instanceof ThreadWithBordel) {
            return model.getChildrenCount (
                ((ThreadWithBordel) o).originalThread
            );
        }
        if (o instanceof OwnedMonitors) {
            return ((OwnedMonitors) o).variables.length;
        }
        return model.getChildrenCount (o);
    }
    
    public boolean isLeaf (TreeModel model, Object o) 
    throws UnknownTypeException {
        if (o instanceof ThreadWithBordel) {
            return false;
        }
        if (o instanceof OwnedMonitors)
            return false;
        if (o instanceof ContendedMonitor)
            return true;
        if (o instanceof ObjectVariable)
            return true;
        return model.isLeaf (o);
    }
    
    
    // NodeModel impl...........................................................
    
    public String getDisplayName (NodeModel model, Object o) throws 
    UnknownTypeException {
        if (o instanceof ContendedMonitor) {
            ObjectVariable v = ((ContendedMonitor) o).variable;
            return java.text.MessageFormat.format(NbBundle.getBundle(MonitorModel.class).getString(
                    "CTL_MonitorModel_Column_ContendedMonitor"), new Object [] { v.getType(), v.getValue() });	//NOI18N
        } else
        if (o instanceof ThreadWithBordel) {
            return model.getDisplayName (
                ((ThreadWithBordel) o).originalThread
            );
        }
        if (o instanceof OwnedMonitors) {
            return NbBundle.getBundle(MonitorModel.class).getString("CTL_MonitorModel_Column_OwnedMonitors");
        } else
        if (o instanceof ObjectVariable) {
            ObjectVariable v = (ObjectVariable) o;
            return java.text.MessageFormat.format(NbBundle.getBundle(MonitorModel.class).getString(
                    "CTL_MonitorModel_Column_Monitor"), new Object [] { v.getType(), v.getValue() });	//NOI18N
        } else
        return model.getDisplayName (o);
    }
    
    private Map shortDescriptionMap = new HashMap();
    
    public String getShortDescription (final NodeModel model, final Object o) throws 
    UnknownTypeException {
        
        synchronized (shortDescriptionMap) {
            Object shortDescription = shortDescriptionMap.remove(o);
            if (shortDescription instanceof String) {
                return (String) shortDescription;
            } else if (shortDescription instanceof UnknownTypeException) {
                throw (UnknownTypeException) shortDescription;
            }
        }
        
        // Called from AWT - we need to postpone the work...
        evaluationRP.post(new Runnable() {
            public void run() {
                Object shortDescription;
                if (o instanceof ContendedMonitor) {
                    ObjectVariable v = ((ContendedMonitor) o).variable;
                    try {
                        shortDescription = "(" + v.getType () + ") " + v.getToStringValue ();	//NOI18N
                    } catch (InvalidExpressionException ex) {
                        shortDescription = ex.getLocalizedMessage ();
                    }
                } else
                if (o instanceof ThreadWithBordel) {
                    try {
                        shortDescription = model.getShortDescription (
                            ((ThreadWithBordel) o).originalThread
                        );
                    } catch (UnknownTypeException utex) {
                        shortDescription = utex;
                    }
                } else
                if (o instanceof OwnedMonitors) {
                    shortDescription = "";
                } else
                if (o instanceof ObjectVariable) {
                    ObjectVariable v = (ObjectVariable) o;
                    try {
                        shortDescription = "(" + v.getType () + ") " + v.getToStringValue ();	//NOI18N
                    } catch (InvalidExpressionException ex) {
                        shortDescription = ex.getLocalizedMessage ();
                    }
                } else {
                    try {
                        shortDescription = model.getShortDescription (o);
                    } catch (UnknownTypeException utex) {
                        shortDescription = utex;
                    }
                }
                
                if (shortDescription != null && !"".equals(shortDescription)) {
                    synchronized (shortDescriptionMap) {
                        shortDescriptionMap.put(o, shortDescription);
                    }
                    fireModelChange(new ModelEvent.NodeChanged(MonitorModel.this,
                        o, ModelEvent.NodeChanged.SHORT_DESCRIPTION_MASK));
                }
            }
        });
        
        return ""; // NOI18N
    }
    
    public String getIconBase (NodeModel model, Object o) throws 
    UnknownTypeException {
        if (o instanceof ContendedMonitor) {
            return CONTENDED_MONITOR;
        } else
        if (o instanceof ThreadWithBordel) {
            return model.getIconBase (
                ((ThreadWithBordel) o).originalThread
            );
        }
        if (o instanceof OwnedMonitors) {
            return OWNED_MONITORS;
        } else
        if (o instanceof ObjectVariable) {
            return MONITOR;
        } else
        return model.getIconBase (o);
    }

    public void addModelListener (ModelListener l) {
        synchronized (modelListeners) {
            modelListeners.add(l);
        }
    }

    public void removeModelListener (ModelListener l) {
        synchronized (modelListeners) {
            modelListeners.remove(l);
        }
    }
    
    private void fireModelChange(ModelEvent me) {
        Object[] listeners;
        synchronized (modelListeners) {
            listeners = modelListeners.toArray();
        }
        for (int i = 0; i < listeners.length; i++) {
            ((ModelListener) listeners[i]).modelChanged(me);
        }
    }
    
    
    // NodeActionsProvider impl.................................................
    
    public Action[] getActions (NodeActionsProvider model, Object o) throws 
    UnknownTypeException {
        if (o instanceof ContendedMonitor) {
            return new Action [0];
        } else
        if (o instanceof OwnedMonitors) {
            return new Action [0];
        } else
        if (o instanceof ThreadWithBordel) {
            return model.getActions (
                ((ThreadWithBordel) o).originalThread
            );
        }
        if (o instanceof ObjectVariable) {
            return new Action [0];
        } else
        return model.getActions (o);
    }
    
    public void performDefaultAction (NodeActionsProvider model, Object o) 
    throws UnknownTypeException {
        if (o instanceof ContendedMonitor) {
            return;
        } else
        if (o instanceof OwnedMonitors) {
            return;
        } else
        if (o instanceof ThreadWithBordel) {
            model.performDefaultAction (
                ((ThreadWithBordel) o).originalThread
            );
            return;
        }
        if (o instanceof ObjectVariable) {
            return;
        } else
        model.performDefaultAction (o);
    }
    
    
    // TableModel ..............................................................
    
    public Object getValueAt (Object node, String columnID) throws 
    UnknownTypeException {
        if (node instanceof OwnedMonitors ||
            node instanceof ContendedMonitor ||
            node instanceof ObjectVariable) {
            
            if (columnID == THREAD_STATE_COLUMN_ID)
                return "";
            if (columnID == THREAD_SUSPENDED_COLUMN_ID)
                return null;
        }
        throw new UnknownTypeException (node);
    }
    
    public boolean isReadOnly (Object node, String columnID) throws 
    UnknownTypeException {
        if (node instanceof OwnedMonitors ||
            node instanceof ContendedMonitor ||
            node instanceof ObjectVariable) {
            
            if (columnID == THREAD_STATE_COLUMN_ID || 
                columnID == THREAD_SUSPENDED_COLUMN_ID) {
                
                return true;
            }
        }
        throw new UnknownTypeException (node);
    }
    
    public void setValueAt (Object node, String columnID, Object value) 
    throws UnknownTypeException {
    }
    
    
    // innerclasses ............................................................
    
    private static class OwnedMonitors {
        ObjectVariable[] variables;
        
        OwnedMonitors (ObjectVariable[] vs) {
            variables = vs;
        }
    }
    
    private static class ContendedMonitor {
        ObjectVariable variable;
        
        ContendedMonitor (ObjectVariable v) {
            variable = v;
        }
    }
    
    static class ThreadWithBordel {
        JavaFXThread originalThread;
    }
}
