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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.WeakHashMap;
import org.netbeans.api.debugger.javafx.JavaFXThread;
import org.netbeans.api.debugger.javafx.JavaFXThreadGroup;
import org.netbeans.spi.debugger.ui.Constants;
import org.netbeans.spi.viewmodel.ModelEvent;
import org.netbeans.spi.viewmodel.TableModel;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.UnknownTypeException;
import org.openide.ErrorManager;
import org.openide.util.NbBundle;
import org.openide.util.RequestProcessor;
import org.openide.util.WeakSet;



/**
 *
 * @author   Jan Jancura
 */
public class ThreadsTableModel implements TableModel, Constants {
    
    private Vector listeners = new Vector ();
    private Map<JavaFXThread, Integer> refreshingThreads;
    private Map<JavaFXThread, ThreadStateChangeListener> threadStateChangeListeners = new WeakHashMap<JavaFXThread, ThreadStateChangeListener>();

    public Object getValueAt (Object row, String columnID) throws 
    UnknownTypeException {
        //if (row instanceof javax.swing.JToolTip) {
        //    will throw UnknownTypeException - the value is used for tooltips
        //}
        if (row instanceof MonitorModel.ThreadWithBordel) 
            row = ((MonitorModel.ThreadWithBordel) row).originalThread;
        if (row instanceof JavaFXThreadGroup) {
            if (THREAD_STATE_COLUMN_ID.equals (columnID)) 
                return "";
            if (THREAD_SUSPENDED_COLUMN_ID.equals (columnID)) {
                JavaFXThreadGroup group = (JavaFXThreadGroup) row;
                JavaFXThread[] threads = group.getThreads ();
                boolean suspended = true;
                for (JavaFXThread t : threads) {
                    if (!t.isSuspended()) {
                        suspended = false;
                        break;
                    }
                }
                return Boolean.valueOf (suspended);
            }
        }
        if (row instanceof JavaFXThread) {
            JavaFXThread t = (JavaFXThread) row;
            if (THREAD_STATE_COLUMN_ID.equals (columnID)) {
                int state = t.getState ();
                synchronized (this) {
                    if (refreshingThreads == null) {
                        refreshingThreads = new WeakHashMap<JavaFXThread, Integer>();
                        new ThreadStateChangeRefresher(this, refreshingThreads);
                    }
                    refreshingThreads.put(t, state);
                }
                String description = getThreadStateDescription(state);
                if (description != null) {
                    return description;
                }
            } else
            if (THREAD_SUSPENDED_COLUMN_ID.equals (columnID)) {
                synchronized (this) {
                    if (!threadStateChangeListeners.containsKey(t)) {
                        threadStateChangeListeners.put(t, new ThreadStateChangeListener(this, t));
                    }
                }
                return Boolean.valueOf (t.isSuspended ());
            }
        }
        throw new UnknownTypeException (row);
    }
    
    private static String getThreadStateDescription(int state) {
        switch (state) {
            case JavaFXThread.STATE_MONITOR:
                return NbBundle.getMessage (
                    ThreadsTableModel.class, 
                    "CTL_Thread_State_OnMonitor"	//NOI18N
                );
            case JavaFXThread.STATE_NOT_STARTED:
                return NbBundle.getMessage (
                    ThreadsTableModel.class, 
                    "CTL_Thread_State_NotStarted"	//NOI18N
                );
            case JavaFXThread.STATE_RUNNING:
                return NbBundle.getMessage (
                    ThreadsTableModel.class, 
                    "CTL_Thread_State_Running"	//NOI18N
                );
            case JavaFXThread.STATE_SLEEPING:
                return NbBundle.getMessage (
                    ThreadsTableModel.class, 
                    "CTL_Thread_State_Sleeping"	//NOI18N
                );
            case JavaFXThread.STATE_UNKNOWN:
                return NbBundle.getMessage (
                    ThreadsTableModel.class, 
                    "CTL_Thread_State_Unknown"	//NOI18N
                );
            case JavaFXThread.STATE_WAIT:
                return NbBundle.getMessage (
                    ThreadsTableModel.class, 
                    "CTL_Thread_State_Waiting"	//NOI18N
                );
            case JavaFXThread.STATE_ZOMBIE:
                return NbBundle.getMessage (
                    ThreadsTableModel.class, 
                    "CTL_Thread_State_Zombie"	//NOI18N
                );
            default: ErrorManager.getDefault().log(ErrorManager.WARNING, "Unknown thread state: "+state);
                    return null;
        }
    }
    
    public boolean isReadOnly (Object row, String columnID) throws 
    UnknownTypeException {
        if (row instanceof MonitorModel.ThreadWithBordel) 
            row = ((MonitorModel.ThreadWithBordel) row).originalThread;
        if (row instanceof JavaFXThreadGroup) {
            if (THREAD_STATE_COLUMN_ID.equals (columnID)) 
                return true;
            if (THREAD_SUSPENDED_COLUMN_ID.equals (columnID)) 
                return false;
        }
        if (row instanceof JavaFXThread) {
            if (THREAD_STATE_COLUMN_ID.equals (columnID))
                return true;
            else
            if (THREAD_SUSPENDED_COLUMN_ID.equals (columnID))
                return false;
        }
        throw new UnknownTypeException (row);
    }
    
    public void setValueAt (Object row, String columnID, Object value) 
    throws UnknownTypeException {
        if (row instanceof MonitorModel.ThreadWithBordel) 
            row = ((MonitorModel.ThreadWithBordel) row).originalThread;
        if (row instanceof JavaFXThreadGroup) {
            if (THREAD_SUSPENDED_COLUMN_ID.equals (columnID)) {
                if (((Boolean) value).booleanValue ())
                    ((JavaFXThreadGroup) row).suspend ();
                else
                    ((JavaFXThreadGroup) row).resume ();
                fireTableValueChanged (row, Constants.THREAD_SUSPENDED_COLUMN_ID);
                return;
            }
        }
        if (row instanceof JavaFXThread) {
            if (THREAD_SUSPENDED_COLUMN_ID.equals (columnID)) {
                if (value.equals (Boolean.TRUE))
                    ((JavaFXThread) row).suspend ();
                else 
                    ((JavaFXThread) row).resume ();
                fireTableValueChanged (row, Constants.THREAD_SUSPENDED_COLUMN_ID);
                return;
            }
        }
        throw new UnknownTypeException (row);
    }
    
    /** 
     * Registers given listener.
     * 
     * @param l the listener to add
     */
    public void addModelListener (ModelListener l) {
        listeners.add (l);
    }

    /** 
     * Unregisters given listener.
     *
     * @param l the listener to remove
     */
    public void removeModelListener (ModelListener l) {
        listeners.remove (l);
    }
    
    private void fireTableValueChanged (Object o, String propertyName) {
        Vector v = (Vector) listeners.clone ();
        int i, k = v.size ();
        for (i = 0; i < k; i++)
            ((ModelListener) v.get (i)).modelChanged (
                new ModelEvent.TableValueChanged (this, o, propertyName)
            );
    }
    
    private void fireNodeChanged (Object node) {
        Vector v = (Vector) listeners.clone ();
        int i, k = v.size ();
        for (i = 0; i < k; i++)
            ((ModelListener) v.get (i)).modelChanged (
                new ModelEvent.NodeChanged(this, node)
            );
    }
    
    private static class ThreadStateChangeListener implements PropertyChangeListener {
        
        private WeakReference<ThreadsTableModel> tmRef;
        private JavaFXThread t;
        
        public ThreadStateChangeListener(ThreadsTableModel tm, JavaFXThread t) {
            tmRef = new WeakReference<ThreadsTableModel>(tm);
            this.t = t;
            ((java.beans.Customizer) t).addPropertyChangeListener(this);
        }

        public void propertyChange(PropertyChangeEvent evt) {
            ThreadsTableModel tm = tmRef.get();
            if (tm == null) {
                ((java.beans.Customizer) t).removePropertyChangeListener(this);
                return ;
            }
            tm.fireNodeChanged(t);
            JavaFXThreadGroup tg = t.getParentThreadGroup();
            while(tg != null) {
                tm.fireTableValueChanged(tg, THREAD_SUSPENDED_COLUMN_ID);
                tg = tg.getParentThreadGroup();
            }
        }
        
    }
    
    private static class ThreadStateChangeRefresher implements Runnable {
        
        private WeakReference<ThreadsTableModel> tmRef;
        private Map<JavaFXThread, Integer> threads;
        private RequestProcessor.Task refreshTask;
        
        public ThreadStateChangeRefresher(ThreadsTableModel tm, Map<JavaFXThread, Integer> threads) {
            tmRef = new WeakReference<ThreadsTableModel>(tm);
            this.threads = threads;
            refreshTask = new RequestProcessor("Threads Refresh", 1).create(this);	//NOI18N
            refreshTask.schedule(1000);
        }

        public void run() {
            ThreadsTableModel tm = tmRef.get();
            if (tm == null) {
                return ;
            }
            long time = System.currentTimeMillis();
            Map<JavaFXThread, Integer> threadStates;
            synchronized (threads) {
                threadStates = new HashMap(threads);
            }
            for (JavaFXThread t : threadStates.keySet()) {
                int state = t.getState();
                if (state != threadStates.get(t).intValue()) {
                    tm.fireTableValueChanged(t, THREAD_STATE_COLUMN_ID);
                }
            }
            time = System.currentTimeMillis() - time;
            refreshTask.schedule(100*((int) time) + 200);
        }
    }
}
