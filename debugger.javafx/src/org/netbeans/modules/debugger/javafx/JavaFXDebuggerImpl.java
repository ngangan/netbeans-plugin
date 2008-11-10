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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
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

package org.netbeans.modules.debugger.javafx;

import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.Bootstrap;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.InvalidStackFrameException;
import com.sun.jdi.LocalVariable;
import com.sun.jdi.Method;
import com.sun.jdi.ObjectCollectedException;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.StackFrame;
import com.sun.jdi.ThreadGroupReference;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.TypeComponent;
import com.sun.jdi.VMDisconnectedException;
import com.sun.jdi.Value;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.request.EventRequest;
import com.sun.jdi.request.EventRequestManager;
import com.sun.jdi.request.InvalidRequestStateException;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyVetoException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.netbeans.api.debugger.DebuggerEngine;
import org.netbeans.api.debugger.DebuggerInfo;
import org.netbeans.api.debugger.DebuggerManager;
import org.netbeans.api.debugger.LazyActionsManagerListener;
import org.netbeans.api.debugger.Properties;

import org.netbeans.spi.debugger.ContextProvider;
import org.netbeans.api.debugger.Session;

import org.netbeans.api.debugger.javafx.AbstractDICookie;
import org.netbeans.api.debugger.javafx.AttachingDICookie;
import org.netbeans.api.debugger.javafx.CallStackFrame;
import org.netbeans.api.debugger.javafx.DebuggerStartException;
import org.netbeans.api.debugger.javafx.InvalidExpressionException;
import org.netbeans.api.debugger.javafx.JavaFXClassType;
import org.netbeans.api.debugger.javafx.JavaFXDebugger;
import org.netbeans.api.debugger.javafx.JavaFXThread;
import org.netbeans.api.debugger.javafx.JavaFXThreadGroup;
import org.netbeans.api.debugger.javafx.ListeningDICookie;
import org.netbeans.api.debugger.javafx.Variable;
import org.netbeans.api.debugger.javafx.JavaFXBreakpoint;
import org.netbeans.modules.debugger.javafx.models.CallStackFrameImpl;
import org.netbeans.modules.debugger.javafx.actions.CompoundSmartSteppingListener;
import org.netbeans.api.debugger.javafx.JavaFXStep;
import org.netbeans.api.debugger.javafx.SmartSteppingFilter;
import org.netbeans.modules.debugger.javafx.breakpoints.BreakpointsEngineListener;
import org.netbeans.api.debugger.javafx.event.JavaFXBreakpointEvent;
import org.netbeans.modules.debugger.javafx.expr.EvaluationContext;
import org.netbeans.modules.debugger.javafx.expr.EvaluationException;
import org.netbeans.modules.debugger.javafx.expr.EvaluationException2;
import org.netbeans.modules.debugger.javafx.expr.Expression;
import org.netbeans.modules.debugger.javafx.expr.Expression2;
import org.netbeans.modules.debugger.javafx.expr.ParseException;
import org.netbeans.modules.debugger.javafx.models.JavaFXClassTypeImpl;
import org.netbeans.modules.debugger.javafx.models.JavaFXThreadImpl;
import org.netbeans.modules.debugger.javafx.models.LocalsTreeModel;
import org.netbeans.modules.debugger.javafx.models.ObjectTranslation;
import org.netbeans.modules.debugger.javafx.models.ThreadsCache;
import org.netbeans.modules.debugger.javafx.util.JavaFXUtils;
import org.netbeans.modules.debugger.javafx.util.Operator;
import org.netbeans.spi.debugger.DebuggerEngineProvider;
import org.netbeans.spi.debugger.DelegatingSessionProvider;

import org.netbeans.spi.viewmodel.TreeModel;
import org.openide.ErrorManager;

/**
* Representation of a debugging session.
*
* @author   Jan Jancura
*/
public class JavaFXDebuggerImpl extends JavaFXDebugger {
    
    private static final Logger logger = Logger.getLogger("org.netbeans.modules.debugger.javafx");	//NOI18N
    
    private static final boolean SINGLE_THREAD_STEPPING = Boolean.getBoolean("netbeans.debugger.singleThreadStepping");	//NOI18N


    // variables ...............................................................

    //private DebuggerEngine              debuggerEngine;
    private VirtualMachine              virtualMachine = null;
    private Exception                   exception;
    private int                         state = 0;
    private Operator                    operator;
    private PropertyChangeSupport       pcs;
    public  PropertyChangeSupport       varChangeSupport = new PropertyChangeSupport(this);
    private JavaFXThreadImpl              currentThread;
    private CallStackFrame              currentCallStackFrame;
    private int                         suspend = (SINGLE_THREAD_STEPPING) ? SUSPEND_EVENT_THREAD : SUSPEND_ALL;
    public final Object                 LOCK = new Object ();
    private final Object                LOCK2 = new Object ();
    private boolean                     starting;
    private AbstractDICookie            attachingCookie;
    private JavaFXEngineProvider          javaFXEngineProvider;
    private Set<String>                 languages;
    private String                      lastStratumn;
    private ContextProvider             lookupProvider;
    private ObjectTranslation           threadsTranslation;
    private ObjectTranslation           localsTranslation;
    private ExpressionPool              expressionPool;
    private ThreadsCache                threadsCache;

    private StackFrame      altCSF = null;  //PATCH 48174

    private boolean                     doContinue = true; // Whether resume() will actually resume
    private Boolean                     singleThreadStepResumeDecision = null;
    private Boolean                     stepInterruptByBptResumeDecision = null;

    // init ....................................................................

    public JavaFXDebuggerImpl (ContextProvider lookupProvider) {
        this.lookupProvider = lookupProvider;
        pcs = new PropertyChangeSupport (this);
        List l = lookupProvider.lookup (null, DebuggerEngineProvider.class);
        int i, k = l.size ();
        for (i = 0; i < k; i++)
            if (l.get (i) instanceof JavaFXEngineProvider)
                javaFXEngineProvider = (JavaFXEngineProvider) l.get (i);
        if (javaFXEngineProvider == null)
            throw new IllegalArgumentException
                ("JavaFXEngineProvider have to be used to start JavaFXDebugger!");	//NOI18N
        languages = new HashSet<String>();
        languages.add ("JavaFX");	//NOI18N
        threadsTranslation = ObjectTranslation.createThreadTranslation(this);
        localsTranslation = ObjectTranslation.createLocalsTranslation(this);
        this.expressionPool = new ExpressionPool();
    }


    // JavaFXDebugger methods ....................................................

    /**
     * Returns current state of JavaFX debugger.
     *
     * @return current state of JavaFX debugger
     * @see #STATE_STARTING
     * @see #STATE_RUNNING
     * @see #STATE_STOPPED
     * @see #STATE_DISCONNECTED
     */
    public int getState () {
        return state;
    }

    /**
     * Gets value of suspend property.
     *
     * @return value of suspend property
     */
    public int getSuspend () {
        return suspend;
    }

    /**
     * Sets value of suspend property.
     *
     * @param s a new value of suspend property
     */
    public void setSuspend (int s) {
        if (s == suspend) return;
        int old = suspend;
        suspend = s;
        firePropertyChange (PROP_SUSPEND, new Integer (old), new Integer (s));
    }

    /**
     * Returns current thread or null.
     *
     * @return current thread or null
     */
    public JavaFXThread getCurrentThread () {
        return currentThread;
    }

    /**
     * Returns current stack frame or null.
     *
     * @return current stack frame or null
     */
    public synchronized CallStackFrame getCurrentCallStackFrame () {
        if (currentCallStackFrame != null) {
            try {
                if (!currentCallStackFrame.getThread().isSuspended()) {
                    currentCallStackFrame = null;
                }
            } catch (InvalidStackFrameException isfex) {
                currentCallStackFrame = null;
            }
        }
        if (currentCallStackFrame == null && currentThread != null) {
            try {
                currentCallStackFrame = currentThread.getCallStack(0, 1)[0];
            } catch (Exception ex) {}
        }
        return currentCallStackFrame;
    }

    /**
     * Evaluates given expression in the current context.
     *
     * @param expression a expression to be evaluated
     *
     * @return current value of given expression
     */
    public Variable evaluate (String expression)
    throws InvalidExpressionException {
        return evaluate(expression,0);
    }

    /**
     * Evaluates given expression in the current context.
     *
     * @param expression a expression to be evaluated
     *
     * @return current value of given expression
     */
    public Variable evaluate (String expression, int pos)
    throws InvalidExpressionException {
        Value v = evaluateIn (expression,pos);
        return getLocalsTreeModel ().getVariable (v);
    }

    /**
     * Waits till the Virtual Machine is started and returns 
     * {@link DebuggerStartException} if any.
     *
     * @throws DebuggerStartException is some problems occurres during debugger 
     *         start
     *
     * @see AbstractDICookie#getVirtualMachine()
     */
    public void waitRunning () throws DebuggerStartException {
        synchronized (LOCK2) {
            if (getState () == STATE_DISCONNECTED) {
                if (exception != null) 
                    throw new DebuggerStartException (exception);
                else 
                    return;
            }
            if (!starting && state != STATE_STARTING || exception != null) {
                return ; // We're already running
            }
            try {
                LOCK2.wait ();
            } catch (InterruptedException e) {
                 throw new DebuggerStartException (e);
            }
            
            if (exception != null) 
                throw new DebuggerStartException (exception);
            else 
                return;
        }
    }

    /**
     * Returns <code>true</code> if this debugger supports Pop action.
     *
     * @return <code>true</code> if this debugger supports Pop action
     */
    public boolean canPopFrames () {
        VirtualMachine vm = getVirtualMachine ();
        if (vm == null) return false;
        return vm.canPopFrames ();
    }

    /**
     * Returns <code>true</code> if this debugger supports fix & continue 
     * (HotSwap).
     *
     * @return <code>true</code> if this debugger supports fix & continue
     */
    public boolean canFixClasses () {
        VirtualMachine vm = getVirtualMachine ();
        if (vm == null) return false;
        return vm.canRedefineClasses ();
    }

    /**
     * Implements fix & continue (HotSwap). Map should contain class names
     * as a keys, and byte[] arrays as a values.
     *
     * @param classes a map from class names to be fixed to byte[] 
     */
    public void fixClasses (Map<String, byte[]> classes) {
        synchronized (LOCK) {
            
            // 1) redefine classes
            Map<ReferenceType, byte[]> map = new HashMap<ReferenceType, byte[]>();
            Iterator<String> i = classes.keySet ().iterator ();
            VirtualMachine vm = getVirtualMachine();
            if (vm == null) {
                return ; // The session has finished
            }
            while (i.hasNext ()) {
                String className = i.next ();
                List<ReferenceType> classRefs = vm.classesByName (className);
                int j, jj = classRefs.size ();
                for (j = 0; j < jj; j++)
                    map.put (
                        classRefs.get (j), 
                        classes.get (className)
                    );
            }
            vm.redefineClasses (map);

            // update breakpoints
            fixBreakpoints();
            
            // 2) pop obsoleted frames
            JavaFXThread t = getCurrentThread ();
            if (t != null && t.isSuspended()) {
                CallStackFrame frame = getCurrentCallStackFrame ();

                //PATCH #52209
                if (t.getStackDepth () < 2 && frame.isObsolete()) return;
                try {
                    if (!frame.equals (t.getCallStack (0, 1) [0])) return;
                } catch (AbsentInformationException ex) {
                    return;
                }

                //PATCH #52209
                if (frame.isObsolete () && ((CallStackFrameImpl) frame).canPop()) {
                    frame.popFrame ();
                    setState (STATE_RUNNING);
                    updateCurrentCallStackFrame (t);
                    setState (STATE_STOPPED);
                }
            }
            
        }
    }
    
    public void fixBreakpoints() {
        Session s = getSession();
        DebuggerEngine de = s.getEngineForLanguage ("JavaFX");	//NOI18N
        BreakpointsEngineListener bel = null;
        List lazyListeners = de.lookup(null, LazyActionsManagerListener.class);
        for (int li = 0; li < lazyListeners.size(); li++) {
            Object service = lazyListeners.get(li);
            if (service instanceof BreakpointsEngineListener) {
                bel = (BreakpointsEngineListener) service;
                break;
            }
        }
        // Just reset the time stamp so that new line numbers are taken.
        EditorContextBridge.getContext().disposeTimeStamp(this);
        EditorContextBridge.getContext().createTimeStamp(this);
        bel.fixBreakpointImpls ();
    }
    
    public Session getSession() {
        return lookupProvider.lookupFirst(null, Session.class);
    }
    
    private Boolean canBeModified;
    private Object canBeModifiedLock = new Object();
    
    public boolean canBeModified() {
        VirtualMachine vm = getVirtualMachine ();
        if (vm == null) return false;
        synchronized (canBeModifiedLock) {
            if (canBeModified == null) {
                try {
                    java.lang.reflect.Method canBeModifiedMethod =
                            com.sun.jdi.VirtualMachine.class.getMethod("canBeModified", new Class[] {});	//NOI18N
                    Object modifiable = canBeModifiedMethod.invoke(vm, new Object[] {});
                    canBeModified = (Boolean) modifiable;
                } catch (NoSuchMethodException nsmex) {
                    // On JDK 1.4 we do not know... we suppose that can
                    canBeModified = Boolean.TRUE;
                } catch (IllegalAccessException iaex) {
                    canBeModified = Boolean.TRUE;
                } catch (InvocationTargetException itex) {
                    canBeModified = Boolean.TRUE;
                }
            }
            return canBeModified.booleanValue();
        }
        // return vm.canBeModified(); -- After we'll build on JDK 1.5
    }

    private SmartSteppingFilter smartSteppingFilter;

    /** 
     * Returns instance of SmartSteppingFilter.
     *
     * @return instance of SmartSteppingFilter
     */
    public SmartSteppingFilter getSmartSteppingFilter () {
        if (smartSteppingFilter == null) {
            smartSteppingFilter = lookupProvider.lookupFirst(null, SmartSteppingFilter.class);
            smartSteppingFilter.addExclusionPatterns (
                (Set) Properties.getDefault ().getProperties ("debugger").	//NOI18N
                    getProperties ("sources").getProperties ("class_filters").	//NOI18N
                    getCollection (
                        "enabled", 	//NOI18N
                        Collections.EMPTY_SET
                    )
            );
        }
        return smartSteppingFilter;
    }

    CompoundSmartSteppingListener compoundSmartSteppingListener;
    
    private CompoundSmartSteppingListener getCompoundSmartSteppingListener () {
        if (compoundSmartSteppingListener == null)
            compoundSmartSteppingListener = lookupProvider.lookupFirst(null, CompoundSmartSteppingListener.class);
        return compoundSmartSteppingListener;
    }
    
    /**
     * Test whether we should stop here according to the smart-stepping rules.
     */
    boolean stopHere(JavaFXThread t) {
        return getCompoundSmartSteppingListener ().stopHere 
                     (lookupProvider, t, getSmartSteppingFilter());
    }
    
    /**
     * Helper method that fires JavaFXBreakpointEvent on JavaFXBreakpoints.
     *
     * @param breakpoint a breakpoint to be changed
     * @param event a event to be fired
     */
    public void fireBreakpointEvent (
        JavaFXBreakpoint breakpoint, 
        JavaFXBreakpointEvent event
    ) {
        super.fireBreakpointEvent (breakpoint, event);
    }

    /**
    * Adds property change listener.
    *
    * @param l new listener.
    */
    public void addPropertyChangeListener (PropertyChangeListener l) {
        pcs.addPropertyChangeListener (l);
    }

    /**
    * Removes property change listener.
    *
    * @param l removed listener.
    */
    public void removePropertyChangeListener (PropertyChangeListener l) {
        pcs.removePropertyChangeListener (l);
    }

    /**
    * Adds property change listener.
    *
    * @param l new listener.
    */
    public void addPropertyChangeListener (String propertyName, PropertyChangeListener l) {
        pcs.addPropertyChangeListener (propertyName, l);
    }

    /**
    * Removes property change listener.
    *
    * @param l removed listener.
    */
    public void removePropertyChangeListener (String propertyName, PropertyChangeListener l) {
        pcs.removePropertyChangeListener (propertyName, l);
    }

    
    // internal interface ......................................................

    public void popFrames (ThreadReference thread, StackFrame frame) {
        synchronized (LOCK) {
            JavaFXThreadImpl threadImpl = (JavaFXThreadImpl) getThread(thread);
            setState (STATE_RUNNING);
            try {
                threadImpl.popFrames(frame);
                updateCurrentCallStackFrame (threadImpl);
            } catch (IncompatibleThreadStateException ex) {
                ErrorManager.getDefault().notify(ex);
            } finally {
                setState (STATE_STOPPED);
            }
        }
    }

    public void setException (Exception e) {
        synchronized (LOCK2) {
            exception = e;
            starting = false;
            LOCK2.notifyAll ();
        }
    }

    public void setCurrentThread (JavaFXThread thread) {
        Object oldT = currentThread;
        currentThread = (JavaFXThreadImpl) thread;
        if (thread != oldT)
            firePropertyChange (PROP_CURRENT_THREAD, oldT, currentThread);
        updateCurrentCallStackFrame (thread);
    }

    /**
     * Set the current thread and call stack, but do not fire changes.
     * @return The PropertyChangeEvent associated with this change, it can have
     *         attached other PropertyChangeEvents as a propagation ID.
     */
    private PropertyChangeEvent setCurrentThreadNoFire(JavaFXThread thread) {
        Object oldT = currentThread;
        currentThread = (JavaFXThreadImpl) thread;
        PropertyChangeEvent evt = null;
        if (thread != oldT)
            evt = new PropertyChangeEvent(this, PROP_CURRENT_THREAD, oldT, currentThread);
        PropertyChangeEvent evt2 = updateCurrentCallStackFrameNoFire(thread);
        if (evt == null) evt = evt2;
        else if (evt2 != null) evt.setPropagationId(evt2);
        return evt;
    }

    public void setCurrentCallStackFrame (CallStackFrame callStackFrame) {
        CallStackFrame old = setCurrentCallStackFrameNoFire(callStackFrame);
        if (old == callStackFrame) return ;
        firePropertyChange (
            PROP_CURRENT_CALL_STACK_FRAME,
            old,
            callStackFrame
        );
    }
    
    private CallStackFrame setCurrentCallStackFrameNoFire (CallStackFrame callStackFrame) {
        CallStackFrame old;
        synchronized (this) {
            if (callStackFrame == currentCallStackFrame) return callStackFrame;
            old = currentCallStackFrame;
            currentCallStackFrame = callStackFrame;
        }
        return old;
    }

    /**
     * Used by AbstractVariable.
     */
    public Value evaluateIn (String expression, int pos) throws InvalidExpressionException {
        Expression expr = null;
        try {
            expr = Expression.parse (expression, Expression.LANGUAGE_JAVA_1_5);
            return evaluateIn (expr, pos);
        } catch (ParseException e) {
            InvalidExpressionException iee = new InvalidExpressionException(e.getMessage());
            iee.initCause(e);
            throw iee;
        }
    }

    //PATCH 48174
    public void setAltCSF(StackFrame sf) {
        altCSF = sf;
    }
    
    public StackFrame getAltCSF() {
        return altCSF;
    }
    
    /**
     * Used by WatchesModel & BreakpointImpl.
     */
    public Value evaluateIn (Expression expression, int pos) 
    throws InvalidExpressionException {
        synchronized (LOCK) {
            
            CallStackFrameImpl csf = (CallStackFrameImpl) 
                getCurrentCallStackFrame ();
            if (csf != null) {
                JavaFXThread frameThread = csf.getThread();
                try {
                    Value value = evaluateIn (expression, csf.getStackFrame (), csf.getFrameDepth(), pos);
                    try {
                        csf.getThread();
                    } catch (InvalidStackFrameException isfex) {
                        // The frame is invalidated, set the new current...
                        int depth = csf.getFrameDepth();
                        try {
                            CallStackFrame csf2 = frameThread.getCallStack(depth, depth + 1)[0];
                            setCurrentCallStackFrameNoFire(csf2);
                        } catch (AbsentInformationException aiex) {
                            setCurrentCallStackFrame(null);
                        }
                    }
                    return value;
                } catch (com.sun.jdi.VMDisconnectedException e) {
                    // Causes kill action when something is being evaluated. 
                    return null;
                }
            }
            //PATCH 48174
            if (altCSF != null) {
                try {
                    if (!altCSF.thread().isSuspended()) {
                        altCSF = null; // Already invalid
                    } else {
                        // TODO XXX : Can be resumed in the mean time !!!!
                        return evaluateIn (expression, altCSF, 0, pos);
                    }
                } catch (InvalidStackFrameException isfex) {
                    // Will be thrown when the altCSF is invalid
                    altCSF = null; // throw it
                } catch (com.sun.jdi.VMDisconnectedException e) {
                    // Causes kill action when something is being evaluated. 
                    return null;
                }
            }
            throw new InvalidExpressionException
                    ("No current context (stack frame)");	//NOI18N
            
        }
    }

    private InvalidExpressionException methodCallsUnsupportedExc;

    /**
     * Used by BreakpointImpl.
     */
    public  Value evaluateIn (Expression expression, final StackFrame frame, int frameDepth, int pos) 
    throws InvalidExpressionException {
        synchronized (LOCK) {
            if (frame == null)
                throw new InvalidExpressionException ("No current context");	//NOI18N

            // TODO: get imports from the source file
            List<String> imports = new ArrayList<String>();
            List<String> staticImports = new ArrayList<String>();
            imports.add ("java.lang.*");	//NOI18N
            try {
                imports.addAll (Arrays.asList (EditorContextBridge.getContext().getImports (
                    getEngineContext ().getURL (frame, "JavaFX")	//NOI18N
                )));
                final ThreadReference tr = frame.thread();
                final List<EventRequest>[] disabledBreakpoints =
                        new List[] { null };
                final JavaFXThreadImpl[] resumedThread = new JavaFXThreadImpl[] { null };
                boolean useNewEvaluator = !Boolean.getBoolean("debugger.evaluatorOld");
                EvaluationContext context;
                if (useNewEvaluator) {
                    Expression2 expression2 = Expression2.parse(expression.getExpression(), expression.getLanguage());
                    org.netbeans.modules.debugger.javafx.expr.TreeEvaluator evaluator2 = 
                        expression2.evaluator(
                            context = new EvaluationContext(
                                frame,
                                frameDepth,
                                imports, 
                                staticImports,
                                methodCallsUnsupportedExc == null,
                                new Runnable() {
                                    public void run() {
                                        if (disabledBreakpoints[0] == null) {
                                            JavaFXThreadImpl theResumedThread = (JavaFXThreadImpl) getThread(tr);
                                            try {
                                                theResumedThread.notifyMethodInvoking();
                                            } catch (PropertyVetoException pvex) {
                                                throw new RuntimeException(
                                                    new InvalidExpressionException (pvex.getMessage()));
                                            }
                                            disabledBreakpoints[0] = disableAllBreakpoints ();
                                            resumedThread[0] = theResumedThread;
                                        }
                                    }
                                },
                                this
                            )
                        );
                        evaluator2.setPosition(pos);
                    try {
                        return evaluator2.evaluate ();
                    } finally {
                        if (methodCallsUnsupportedExc == null && !context.canInvokeMethods()) {
                            methodCallsUnsupportedExc =
                                    new InvalidExpressionException(new UnsupportedOperationException());
                        }
                        if (disabledBreakpoints[0] != null) {
                            enableAllBreakpoints (disabledBreakpoints[0]);
                        }
                        if (resumedThread[0] != null) {
                            resumedThread[0].notifyMethodInvokeDone();
                        }
                    }
                } else {
                    org.netbeans.modules.debugger.javafx.expr.Evaluator evaluator = 
                        expression.evaluator (
                            context = new EvaluationContext (
                                frame,
                                frameDepth,
                                imports, 
                                staticImports,
                                methodCallsUnsupportedExc == null,
                                new Runnable() {
                                    public void run() {
                                        if (disabledBreakpoints[0] == null) {
                                            JavaFXThreadImpl theResumedThread = (JavaFXThreadImpl) getThread(tr);
                                            try {
                                                theResumedThread.notifyMethodInvoking();
                                            } catch (PropertyVetoException pvex) {
                                                throw new RuntimeException(
                                                    new InvalidExpressionException (pvex.getMessage()));
                                            }
                                            disabledBreakpoints[0] = disableAllBreakpoints ();
                                            resumedThread[0] = theResumedThread;
                                        }
                                    }
                                },
                                this
                            )
                        );
                    try {
                        return evaluator.evaluate ();
                    } finally {
                        if (methodCallsUnsupportedExc == null && !context.canInvokeMethods()) {
                            methodCallsUnsupportedExc =
                                    new InvalidExpressionException(new UnsupportedOperationException());
                        }
                        if (disabledBreakpoints[0] != null) {
                            enableAllBreakpoints (disabledBreakpoints[0]);
                        }
                        if (resumedThread[0] != null) {
                            resumedThread[0].notifyMethodInvokeDone();
                        }
                    }
                }
            } catch (EvaluationException e) {
                InvalidExpressionException iee = new InvalidExpressionException (e);
                iee.initCause (e);
                throw iee;
            } catch (EvaluationException2 e) {
                InvalidExpressionException iee = new InvalidExpressionException (e);
                iee.initCause (e);
                throw iee;
            } catch (IncompatibleThreadStateException itsex) {
                ErrorManager.getDefault().notify(itsex);
                IllegalStateException isex = new IllegalStateException(itsex.getLocalizedMessage());
                isex.initCause(itsex);
                throw isex;
            } catch (RuntimeException rex) {
                Throwable cause = rex.getCause();
                if (cause instanceof InvalidExpressionException) {
                    throw (InvalidExpressionException) cause;
                } else {
                    throw rex;
                }
            }
        }
    }
    
    /**
     * Used by AbstractVariable.
     */
    public Value invokeMethod (
        ObjectReference reference,
        Method method,
        Value[] arguments
    ) throws InvalidExpressionException {
        if (currentThread == null)
            throw new InvalidExpressionException ("No current context"); //NOI18N	
        synchronized (LOCK) {
            if (methodCallsUnsupportedExc != null) {
                throw methodCallsUnsupportedExc;
            }
            boolean threadSuspended = false;
            JavaFXThread frameThread = null;
            CallStackFrameImpl csf = null;
            JavaFXThreadImpl thread = null;
            List<EventRequest> l = null;
            try {
                // Remember the current stack frame, it might be necessary to re-set.
                csf = (CallStackFrameImpl) getCurrentCallStackFrame ();
                if (csf != null) {
                    try {
                        frameThread = csf.getThread();
                    } catch (InvalidStackFrameException isfex) {}
                }
                ThreadReference tr = getEvaluationThread();
                thread = (JavaFXThreadImpl) getThread(tr);
                synchronized (thread) {
                    threadSuspended = thread.isSuspended();
                    if (!threadSuspended) {
                        throw new InvalidExpressionException ("No current context");	//NOI18N
                    }
                    try {
                        thread.notifyMethodInvoking();
                    } catch (PropertyVetoException pvex) {
                        throw new InvalidExpressionException (pvex.getMessage());
                    }
                }
                l = disableAllBreakpoints ();
                return org.netbeans.modules.debugger.javafx.expr.TreeEvaluator.
                    invokeVirtual (
                        reference,
                        method,
                        tr,
                        Arrays.asList (arguments)
                    );
            } catch (InvalidExpressionException ieex) {
                if (ieex.getTargetException() instanceof UnsupportedOperationException) {
                    methodCallsUnsupportedExc = ieex;
                }
                throw ieex;
            } finally {
                if (l != null) {
                    enableAllBreakpoints (l);
                }
                if (threadSuspended) {
                    thread.notifyMethodInvokeDone();
                }
                if (frameThread != null) {
                    try {
                        csf.getThread();
                    } catch (InvalidStackFrameException isfex) {
                        // The current frame is invalidated, set the new current...
                        int depth = csf.getFrameDepth();
                        try {
                            CallStackFrame csf2 = frameThread.getCallStack(depth, depth + 1)[0];
                            setCurrentCallStackFrameNoFire(csf2);
                        } catch (AbsentInformationException aiex) {
                            setCurrentCallStackFrame(null);
                        }
                    }
                }
            }
        }
    }

    public static String getGenericSignature (TypeComponent component) {
        if (tcGenericSignatureMethod == null) return null;
        try {
            return (String) tcGenericSignatureMethod.invoke 
                (component, new Object[0]);
        } catch (IllegalAccessException e) {
            e.printStackTrace();
            return null;    // should not happen
        } catch (InvocationTargetException e) {
            e.printStackTrace();
            return null;    // should not happen
        }
    }

    public static String getGenericSignature (LocalVariable component) {
        if (lvGenericSignatureMethod == null) return null;
        try {
            return (String) lvGenericSignatureMethod.invoke(component, new Object[0]);
        } catch (IllegalAccessException e) {
            e.printStackTrace();
            return null;    // should not happen
        } catch (InvocationTargetException e) {
            e.printStackTrace();
            return null;    // should not happen
        }
    }

    public VirtualMachine getVirtualMachine () {
        return virtualMachine;
    }

    public Operator getOperator () {
        return operator;
    }

    public void setStarting () {
        setState (STATE_STARTING);
    }
    
    public synchronized void setAttaching(AbstractDICookie cookie) {
        this.attachingCookie = cookie;
    }
    
    public void setRunning (VirtualMachine vm, Operator o) {
        if (logger.isLoggable(Level.FINE)) {
            logger.fine("Start - JavaFXDebuggerImpl.setRunning ()");
            JavaFXUtils.printFeatures (logger, vm);
        }
        synchronized (LOCK2) {
            starting = true;
        }
        synchronized (this) {
            virtualMachine = vm;
        }
        synchronized (canBeModifiedLock) {
            canBeModified = null; // Reset the can be modified flag
        }
        
        initGenericsSupport ();
        EditorContextBridge.getContext().createTimeStamp(this);

        
        operator = o;
        
//        Iterator i = getVirtualMachine ().allThreads ().iterator ();
//        while (i.hasNext ()) {
//            ThreadReference tr = (ThreadReference) i.next ();
//            if (tr.isSuspended ()) {
//                if (startVerbose)
//                    System.out.println("\nS JavaFXDebuggerImpl.setRunning () - " +
//                        "thread supended"
//                    );
//                setState (STATE_RUNNING);
//                synchronized (LOCK) {
//                    virtualMachine.resume ();
//                }
//                if (startVerbose)
//                    System.out.println("\nS JavaFXDebuggerImpl.setRunning () - " +
//                        "thread supended - VM resumed - end"
//                    );
//                synchronized (LOCK2) {
//                    LOCK2.notify ();
//                }
//                return;
//            }
//        }
        
        synchronized (this) {
            if (threadsCache != null) {
                threadsCache.setVirtualMachine(vm);
            }
        }
        
        setState (STATE_RUNNING);
        synchronized (this) {
            vm = virtualMachine; // re-take the VM, it can be nulled by finish()
        }
        if (vm != null) {
            synchronized (LOCK) {
                vm.resume();
            }
        }
        
        logger.fine("   JavaFXDebuggerImpl.setRunning () finished, VM resumed.");
        synchronized (LOCK2) {
            starting = false;
            LOCK2.notifyAll ();
        }
    }

    /**
    * Performs stop action.
    */
    public void setStoppedState (ThreadReference thread) {
        PropertyChangeEvent evt;
        synchronized (LOCK) {
            // this method can be called in stopped state to switch 
            // the current thread only
            JavaFXThread t = getThread (thread);
            checkJSR45Languages (t);
            evt = setCurrentThreadNoFire(t);
            PropertyChangeEvent evt2 = setStateNoFire(STATE_STOPPED);
            
            if (evt == null) evt = evt2;
            else if (evt2 != null) {
                PropertyChangeEvent evt3 = evt;
                while(evt3.getPropagationId() != null) evt3 = (PropertyChangeEvent) evt3.getPropagationId();
                evt3.setPropagationId(evt2);
            }
        }
        if (evt != null) {
            do {
                firePropertyChange(evt);
                evt = (PropertyChangeEvent) evt.getPropagationId();
            } while (evt != null);
        }
    }
    
    /**
     * Can be called if the current thread is resumed after stop.
     */
    public void setRunningState() {
        setState(STATE_RUNNING);
    }
    
    /**
    * Performs stop action and disable a next call to resume()
    */
    public void setStoppedStateNoContinue (ThreadReference thread) {
        PropertyChangeEvent evt;
        synchronized (LOCK) {
            // this method can be called in stopped state to switch 
            // the current thread only
            evt = setStateNoFire(STATE_RUNNING);
            JavaFXThread t = getThread (thread);
            checkJSR45Languages (t);
            PropertyChangeEvent evt2 = setCurrentThreadNoFire(t);
            
            if (evt == null) evt = evt2;
            else if (evt2 != null) evt.setPropagationId(evt2);
            
            evt2 = setStateNoFire(STATE_STOPPED);
            
            if (evt == null) evt = evt2;
            else if (evt2 != null) {
                PropertyChangeEvent evt3 = evt;
                while(evt3.getPropagationId() != null) evt3 = (PropertyChangeEvent) evt3.getPropagationId();
                evt3.setPropagationId(evt2);
            }
            
            doContinue = false;
        }
        if (evt != null) {
            do {
                firePropertyChange(evt);
                evt = (PropertyChangeEvent) evt.getPropagationId();
            } while (evt != null);
        }
    }
    
    
    private boolean finishing;

    /**
     * Used by KillActionProvider.
     */
    public void finish () {
        //Workaround for #56233
        //synchronized (LOCK) { 
            synchronized (this) {
                if (finishing) {
                    // Can easily be called twice - from the operator termination
                    return ;
                }
                finishing = true;
            }
            logger.fine("StartActionProvider.finish ()");
            AbstractDICookie di = lookupProvider.lookupFirst(null, AbstractDICookie.class);
            if (getState () == STATE_DISCONNECTED) return;
            Operator o = getOperator();
            if (o != null) o.stop();
            synchronized (this) {
                if (attachingCookie != null) {
                    if (attachingCookie instanceof ListeningDICookie) {
                        ListeningDICookie listeningCookie = (ListeningDICookie) attachingCookie;
                        try {
                            listeningCookie.getListeningConnector().stopListening(listeningCookie.getArgs());
                        } catch (java.io.IOException ioex) {
                        } catch (com.sun.jdi.connect.IllegalConnectorArgumentsException icaex) {
                        } catch (IllegalArgumentException iaex) {
                        }
                    }
                }
            }
            try {
                waitRunning(); // First wait till the debugger comes up
            } catch (DebuggerStartException dsex) {
                // We do not want to start it anyway when we're finishing - do not bother
            }
            VirtualMachine vm;
            synchronized (this) {
                vm = virtualMachine;
            }
            if (vm != null) {
                try {
                    if (di instanceof AttachingDICookie) {
                        logger.fine(" StartActionProvider.finish() VM dispose");
                        vm.dispose ();
                    } else {
                        logger.fine(" StartActionProvider.finish() VM exit");
                        vm.exit (0);
                    }
                } catch (VMDisconnectedException e) {
                    logger.fine(" StartActionProvider.finish() VM exception " + e);
                    // debugee VM is already disconnected (it finished normally)
                }
            }
            synchronized (this) {
                virtualMachine = null;
            }
            setState (STATE_DISCONNECTED);
            if (jsr45EngineProviders != null) {
                for (Iterator<JSR45DebuggerEngineProvider> i = jsr45EngineProviders.iterator(); i.hasNext();) {
                    JSR45DebuggerEngineProvider provider = i.next();
                    provider.getDesctuctor().killEngine();
                }
                jsr45EngineProviders = null;
            }
            javaFXEngineProvider.getDestructor ().killEngine ();
            logger.fine (" StartActionProvider.finish() end.");
            
            //Notify LOCK2 so that no one is waiting forever
            synchronized (LOCK2) {
                starting = false;
                LOCK2.notifyAll ();
            }
            EditorContextBridge.getContext().disposeTimeStamp(this);
        //}
    }

    /**
     * Suspends the target virtual machine (if any).
     * Used by PauseActionProvider.
     *
     * @see  com.sun.jdi.ThreadReference#suspend
     */
    public void suspend () {
        VirtualMachine vm;
        synchronized (this) {
            vm = virtualMachine;
        }
        synchronized (LOCK) {
            if (getState () == STATE_STOPPED)
                return;
            if (vm != null) {
                logger.fine("VM suspend");
                vm.suspend ();
                // Check the suspended count
                List<ThreadReference> threads = vm.allThreads();
                for (ThreadReference t : threads) {
                    while (t.suspendCount() > 1) t.resume();
                }
            }
            setState (STATE_STOPPED);
        }
        notifySuspendAll();
    }
    
    public void notifySuspendAll() {
        Collection threads = threadsTranslation.getTranslated();
        for (Iterator it = threads.iterator(); it.hasNext(); ) {
            Object threadOrGroup = it.next();
            if (threadOrGroup instanceof JavaFXThreadImpl) {
                int status = ((JavaFXThreadImpl) threadOrGroup).getState();
                boolean invalid = (status == JavaFXThread.STATE_NOT_STARTED ||
                                   status == JavaFXThread.STATE_UNKNOWN ||
                                   status == JavaFXThread.STATE_ZOMBIE);
                if (!invalid) {
                    try {
                        ((JavaFXThreadImpl) threadOrGroup).notifySuspended();
                    } catch (ObjectCollectedException ocex) {
                        invalid = true;
                    }
                }
                if (invalid) {
                    threadsTranslation.remove(((JavaFXThreadImpl) threadOrGroup).getThreadReference());
                }
            }
        }
    }
    
    /**
     * Used by ContinueActionProvider & StepActionProvider.
     */
    public void resume () {
        synchronized (LOCK) {
            if (!doContinue) {
                doContinue = true;
                // Continue the next time and do nothing now.
                return ;
            }
        }
        if (operator.flushStaledEvents()) {
            return ;
        }
        setState (STATE_RUNNING);
        notifyToBeResumedAll();
        VirtualMachine vm;
        synchronized (this) {
            vm = virtualMachine;
        }
        synchronized (LOCK) {
            if (vm != null) {
                logger.fine("VM resume");
                vm.resume ();
            }
        }
    }
    
    /** DO NOT CALL FROM ANYWHERE BUT JavaFXThreadImpl.resume(). */
    public boolean currentThreadToBeResumed() {
        synchronized (LOCK) {
            if (!doContinue) {
                doContinue = true;
                // Continue the next time and do nothing now.
                return false;
            }
        }
        if (operator.flushStaledEvents()) {
            return false;
        }
        setState (STATE_RUNNING);
        return true;
    }
    
    public void resumeCurrentThread() {
        synchronized (LOCK) {
            if (!doContinue) {
                doContinue = true;
                // Continue the next time and do nothing now.
                return ;
            }
        }
        if (operator.flushStaledEvents()) {
            return ;
        }
        setState (STATE_RUNNING);
        currentThread.resume();
    }
    
    public void notifyToBeResumedAll() {
        Collection threads = threadsTranslation.getTranslated();
        for (Iterator it = threads.iterator(); it.hasNext(); ) {
            Object threadOrGroup = it.next();
            if (threadOrGroup instanceof JavaFXThreadImpl) {
                int status = ((JavaFXThreadImpl) threadOrGroup).getState();
                boolean invalid = (status == JavaFXThread.STATE_NOT_STARTED ||
                                   status == JavaFXThread.STATE_UNKNOWN ||
                                   status == JavaFXThread.STATE_ZOMBIE);
                if (!invalid) {
                    ((JavaFXThreadImpl) threadOrGroup).notifyToBeResumed();
                } else {
                    threadsTranslation.remove(((JavaFXThreadImpl) threadOrGroup).getThreadReference());
                }
            }
        }
    }
    
    public synchronized ThreadsCache getThreadsCache() {
        if (threadsCache == null) {
            threadsCache = new ThreadsCache(this);
        }
        return threadsCache;
    }
    
    public JavaFXThreadGroup[] getTopLevelThreadGroups() {
        ThreadsCache tc = getThreadsCache();
        if (tc == null) {
            return new JavaFXThreadGroup[0];
        }
        List<ThreadGroupReference> groupList = tc.getTopLevelThreadGroups();
        JavaFXThreadGroup[] groups = new JavaFXThreadGroup[groupList.size()];
        for (int i = 0; i < groups.length; i++) {
            groups[i] = getThreadGroup((ThreadGroupReference) groupList.get(i));
        }
        return groups;
    }

    public JavaFXThread getThread (ThreadReference tr) {
        return (JavaFXThread) threadsTranslation.translate (tr);
    }

    public JavaFXThread getExistingThread (ThreadReference tr) {
        return (JavaFXThread) threadsTranslation.translateExisting(tr);
    }

    public JavaFXThreadGroup getThreadGroup (ThreadGroupReference tgr) {
        return (JavaFXThreadGroup) threadsTranslation.translate (tgr);
    }
    
    public Variable getLocalVariable(LocalVariable lv, Value v) {
        return (Variable) localsTranslation.translate(lv, v);
    }
    
    public JavaFXClassType getClassType(ReferenceType cr) {
        return (JavaFXClassType) localsTranslation.translate (cr);
    }

    public Variable getVariable (Value value) {
        return getLocalsTreeModel ().getVariable (value);
    }
    
    public ExpressionPool getExpressionPool() {
        return expressionPool;
    }
    
    synchronized void setSingleThreadStepResumeDecision(Boolean decision) {
        singleThreadStepResumeDecision = decision;
    }
    
    synchronized Boolean getSingleThreadStepResumeDecision() {
        return singleThreadStepResumeDecision;
    }
    
    public synchronized void setStepInterruptByBptResumeDecision(Boolean decision) {
        stepInterruptByBptResumeDecision = decision;
    }
    
    public synchronized Boolean getStepInterruptByBptResumeDecision() {
        return stepInterruptByBptResumeDecision;
    }


    // private helper methods ..................................................
    
    private static final java.util.regex.Pattern jvmVersionPattern =
            java.util.regex.Pattern.compile ("(\\d+)\\.(\\d+)\\.(\\d+)(_\\d+)?(-\\w+)?");	//NOI18N
    private static java.lang.reflect.Method  tcGenericSignatureMethod;
    private static java.lang.reflect.Method  lvGenericSignatureMethod;


    private void initGenericsSupport () {
        tcGenericSignatureMethod = null;
        if (Bootstrap.virtualMachineManager ().minorInterfaceVersion () >= 5) {
            VirtualMachine vm;
            synchronized (this) {
                vm = virtualMachine;
            }
            if (vm == null) return ;
            java.util.regex.Matcher m = jvmVersionPattern.matcher(vm.version ());
            if (m.matches ()) {
                int minor = Integer.parseInt (m.group (2));
                if (minor >= 5) {
                    try {
                        tcGenericSignatureMethod = TypeComponent.class.
                            getMethod ("genericSignature", new Class [0]);	//NOI18N
                        lvGenericSignatureMethod = LocalVariable.class.
                            getMethod ("genericSignature", new Class [0]);	//NOI18N
                    } catch (NoSuchMethodException e) {
                        // the method is not available, ignore generics
                    }
                }
            }
        }
    }
    
    private PropertyChangeEvent setStateNoFire (int state) {
        if (state == this.state) return null;
        int o = this.state;
        this.state = state;
        //PENDING HACK see issue 46287
        System.setProperty(
            "org.openide.awt.SwingBrowserImpl.do-not-block-awt",	//NOI18N
            String.valueOf (state != STATE_DISCONNECTED)
        );
        return new PropertyChangeEvent(this, PROP_STATE, new Integer (o), new Integer (state));
    }

    private void setState (int state) {
        PropertyChangeEvent evt = setStateNoFire(state);
        if (evt != null) {
            firePropertyChange(evt);
        }
    }
    
    /**
     * Fires property change.
     */
    private void firePropertyChange (String name, Object o, Object n) {
        pcs.firePropertyChange (name, o, n);
        //System.err.println("ALL Change listeners count = "+pcs.getPropertyChangeListeners().length);
    }

    /**
     * Fires property change.
     */
    private void firePropertyChange (PropertyChangeEvent evt) {
        pcs.firePropertyChange (evt);
        //System.err.println("ALL Change listeners count = "+pcs.getPropertyChangeListeners().length);
    }

    private SourcePath engineContext;
    public synchronized SourcePath getEngineContext () {
        if (engineContext == null)
            engineContext = lookupProvider.lookupFirst(null, SourcePath.class);
        return engineContext;
    }

    private LocalsTreeModel localsTreeModel;
    private LocalsTreeModel getLocalsTreeModel () {
        if (localsTreeModel == null)
            localsTreeModel = (LocalsTreeModel) lookupProvider.
                lookupFirst ("LocalsView", TreeModel.class);	//NOI18N
        return localsTreeModel;
    }

    private ThreadReference getEvaluationThread () {
        if (currentThread != null) return currentThread.getThreadReference ();
        VirtualMachine vm;
        synchronized (this) {
            vm = virtualMachine;
        }
        if (vm == null) return null;
        List l = vm.allThreads ();
        if (l.size () < 1) return null;
        int i, k = l.size ();
        ThreadReference thread = null;
        for (i = 0; i < k; i++) {
            ThreadReference t = (ThreadReference) l.get (i);
            if (t.isSuspended ()) {
                thread = t;
                if (t.name ().equals ("Finalizer"))	//NOI18N
                    return t;
            }
        }
        return thread;
    }

    private void updateCurrentCallStackFrame (JavaFXThread thread) {
        if ( (thread == null) ||
             (thread.getStackDepth () < 1))
            setCurrentCallStackFrame (null);
        else
        try {
            setCurrentCallStackFrame (thread.getCallStack (0, 1) [0]);
        } catch (AbsentInformationException e) {
            setCurrentCallStackFrame (null);
        }
    }

    /**
     * @param thread The thread to take the top frame from
     * @return A PropertyChangeEvent or <code>null</code>.
     */
    private PropertyChangeEvent updateCurrentCallStackFrameNoFire(JavaFXThread thread) {
        CallStackFrame old;
        CallStackFrame callStackFrame;
        if ( (thread == null) ||
             (thread.getStackDepth () < 1))
            old = setCurrentCallStackFrameNoFire(callStackFrame = null);
        else
        try {
            old = setCurrentCallStackFrameNoFire(callStackFrame = thread.getCallStack (0, 1) [0]);
        } catch (AbsentInformationException e) {
            old = setCurrentCallStackFrameNoFire(callStackFrame = null);
        }
        if (old == callStackFrame) return null;
        else return new PropertyChangeEvent(this, PROP_CURRENT_CALL_STACK_FRAME,
                                            old, callStackFrame);
    }
    
    private List<EventRequest> disableAllBreakpoints () {
        logger.fine ("disableAllBreakpoints() start.");
        List<EventRequest> l = new ArrayList<EventRequest>();
        VirtualMachine vm = getVirtualMachine ();
        if (vm == null) return l;
        EventRequestManager erm = vm.eventRequestManager ();
        l.addAll (erm.accessWatchpointRequests ());
        l.addAll (erm.breakpointRequests ());
        l.addAll (erm.classPrepareRequests ());
        l.addAll (erm.classUnloadRequests ());
        l.addAll (erm.exceptionRequests ());
        l.addAll (erm.methodEntryRequests ());
        l.addAll (erm.methodExitRequests ());
        l.addAll (erm.modificationWatchpointRequests ());
//        l.addAll (erm.stepRequests ());
        l.addAll (erm.threadDeathRequests ());
        l.addAll (erm.threadStartRequests ());
        int i = l.size () - 1;
        for (; i >= 0; i--)
            if (!l.get (i).isEnabled ())
                l.remove (i);
            else
                l.get (i).disable ();
        operator.breakpointsDisabled();
        logger.fine ("disableAllBreakpoints() end.");
        return l;
    }
    
    private void enableAllBreakpoints (List<EventRequest> l) {
        logger.fine ("enableAllBreakpoints() start.");
        operator.breakpointsEnabled();
        int i, k = l.size ();
        for (i = 0; i < k; i++)
            try {
                l.get (i).enable ();
            } catch (IllegalThreadStateException ex) {
                // see #53163
                // this can occurre if there is some "old" StepRequest and
                // thread named in the request has died
            } catch (InvalidRequestStateException ex) {
                // workaround for #51176
            }
        logger.fine ("enableAllBreakpoints() end.");
    }

    private void checkJSR45Languages (JavaFXThread t) {
        if (t.getStackDepth () > 0)
            try {
                CallStackFrame f = t.getCallStack (0, 1) [0];
                List l = f.getAvailableStrata ();
                int i, k = l.size ();
                for (i = 0; i < k; i++) {
                    if (!languages.contains (l.get (i))) {
                        String language = (String) l.get (i);
                        DebuggerManager.getDebuggerManager ().startDebugging (
                            createJSR45DI (language)
                        );
                        languages.add (language);
                    }
                } // for
                String stratum = f.getDefaultStratum ();
                if ( (stratum != null) &&
                     (!stratum.equals (lastStratumn))
                )
                    javaFXEngineProvider.getSession ().setCurrentLanguage (stratum);
                lastStratumn = stratum;
            } catch (AbsentInformationException e) {
                System.out.println("NoInformationException");	//NOI18N
            }
    }
 
    private Set<JSR45DebuggerEngineProvider> jsr45EngineProviders;

    private DebuggerInfo createJSR45DI (final String language) {
        if (jsr45EngineProviders == null) {
            jsr45EngineProviders = new HashSet<JSR45DebuggerEngineProvider>(1);
        }
        JSR45DebuggerEngineProvider provider = new JSR45DebuggerEngineProvider(language);
        jsr45EngineProviders.add(provider);
        return DebuggerInfo.create (
            "netbeans-JavaFX-JSR45DICookie-" + language,	//NOI18N
            new Object[] {
                new DelegatingSessionProvider () {
                    public Session getSession (
                        DebuggerInfo debuggerInfo
                    ) {
                        return javaFXEngineProvider.getSession ();
                    }
                },
                provider
            }
        );
    }
    
    public JavaFXStep createJavaFXStep(int size, int depth) {
        Session session = lookupProvider.lookupFirst(null, Session.class);
        return new JavaFXStepImpl(this, session, size, depth);
    }
    
    /*public synchronized Heap getHeap() {
        if (virtualMachine != null && canGetInstanceInfo(virtualMachine)) {
            return new HeapImpl(virtualMachine);
        } else {
            return null;
        }
    }*/
    
    public List<JavaFXClassType> getAllClasses() {
        List<ReferenceType> classes;
        synchronized (this) {
            if (virtualMachine == null) {
                classes = Collections.emptyList();
            } else {
                classes = virtualMachine.allClasses();
            }
        }
        return new ClassTypeList(this, classes);
    }
    
    public List<JavaFXClassType> getClassesByName(String name) {
        List<ReferenceType> classes;
        synchronized (this) {
            if (virtualMachine == null) {
                classes = Collections.emptyList();
            } else {
                classes = virtualMachine.classesByName(name);
            }
        }
        return new ClassTypeList(this, classes);
    }
    
    public long[] getInstanceCounts(List<JavaFXClassType> classTypes) throws UnsupportedOperationException {
        if (Java6Methods.isJDK6()) {
            VirtualMachine vm;
            synchronized (this) {
                vm = virtualMachine;
            }
            if (vm == null) {
                return new long[classTypes.size()];
            }
            if (classTypes instanceof ClassTypeList) {
                ClassTypeList cl = (ClassTypeList) classTypes;
                return Java6Methods.instanceCounts(vm, cl.getTypes());
            } else {
                List<ReferenceType> types = new ArrayList<ReferenceType>(classTypes.size());
                for (JavaFXClassType clazz : classTypes) {
                    types.add(((JavaFXClassTypeImpl) clazz).getType());
                }
                return Java6Methods.instanceCounts(vm, types);
            }
        } else {
            throw new UnsupportedOperationException("Not supported.");	//NOI18N
        }
    }
    
    public synchronized boolean canGetInstanceInfo() {
        return virtualMachine != null && canGetInstanceInfo(virtualMachine);
    }
    
    private static boolean canGetInstanceInfo(VirtualMachine vm) {
        if (Java6Methods.isJDK6()) {
            try {
                java.lang.reflect.Method canGetInstanceInfoMethod = VirtualMachine.class.getMethod("canGetInstanceInfo", new Class[] {});	//NOI18N
                Object canGetInstanceInfo = canGetInstanceInfoMethod.invoke(vm, new Object[] {});
                return Boolean.TRUE.equals(canGetInstanceInfo);
            } catch (Exception ex) {
                ErrorManager.getDefault().notify(ex);
            }
        }
        return false;
    }
}
