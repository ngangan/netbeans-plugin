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
 * Portions Copyrighted 2007 Sun Microsystems, Inc.
 */

/*
 * DebugService.java
 * Some of the code in this class is based on Martin Ryzl's Dbd.java
 *
 * Created on December 9, 2002, 1:50 PM
 */

package debugger.framework;

import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.Bootstrap;
import com.sun.jdi.Field;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.LocalVariable;
import com.sun.jdi.Location;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.StackFrame;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.Value;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.connect.AttachingConnector;
import com.sun.jdi.connect.Connector;
import com.sun.jdi.connect.IllegalConnectorArgumentsException;
import com.sun.jdi.event.Event;
import com.sun.jdi.event.EventIterator;
import com.sun.jdi.event.EventQueue;
import com.sun.jdi.event.EventSet;
import com.sun.jdi.event.ExceptionEvent;
import com.sun.jdi.event.VMDeathEvent;
import com.sun.jdi.event.VMDisconnectEvent;
import com.sun.jdi.request.BreakpointRequest;
import com.sun.jdi.request.EventRequestManager;
import com.sun.jdi.request.StepRequest;

/**
 * @author kh148139
 */
public class DebugServiceImpl implements DebuggerEventListener, DebugService {

    private Communicator com;
    private VirtualMachine vm;
    private VMCapabilities vmCapapbilities;
    private EventRequestManager eventRequestManager;
    private ArrayList tests = new ArrayList();
    private boolean debugSessionOver;
    private PrintStream log;
	private PrintStream ref;

    public DebugServiceImpl(PrintStream log, PrintStream ref) {
        this.log = log;
        this.ref = ref;
    }

    /**
     * Starts the debugging service. All DebuggerTest instances must be registered with this service before this method is called.
     * 
     * @param port
     * @throws IOException
     * @throws IllegalConnectorArgumentsException
     * @throws InterruptedException
     */
    public void startService(int port) throws IOException, IllegalConnectorArgumentsException, InterruptedException {
        this.com = new Communicator(port);

        //ADDED
        Thread t = new Thread(this.com);
        t.start();
        t.join();
    }

    private void setVM(VirtualMachine vm) {
        this.vm = vm;
        this.eventRequestManager = vm.eventRequestManager();
        this.vmCapapbilities = new VMCapabilities(vm);
        ListIterator it = this.tests.listIterator();
        while (it.hasNext()) {
            DebuggerTest test = (DebuggerTest) it.next();
            if (test.isVMCompatible(vmCapapbilities)) {
                test.registerRequests(eventRequestManager);
            }
            else {
                it.remove();
            }
        }
    }

    private class Communicator implements Runnable {

        private EventQueue eventQueue;

        public Communicator(int port) throws IOException, IllegalConnectorArgumentsException {
            List connectors = Bootstrap.virtualMachineManager().attachingConnectors();
            if (connectors.size() == 0) {
                throw new RuntimeException("No attaching connector.");
            }
            AttachingConnector attachingConnector = null;
            Iterator it = connectors.iterator();
            while (it.hasNext()) {
                AttachingConnector ac = (AttachingConnector) it.next();
                if ("dt_socket".equals(ac.transport().name())) {
                    attachingConnector = ac;
                    break;
                }
            }
            if (attachingConnector == null) {
                if (connectors.size() == 0) {
                    throw new RuntimeException("No attaching connector.");
                }
            }
            Map map = attachingConnector.defaultArguments();
            Connector.Argument caa = (Connector.Argument) map.get("port");
            if (caa == null) {
                log.println("Port argument not supported by AttachingConnector.");
            }
            caa.setValue(Integer.toString(port));

            ///ADDED
            VirtualMachine vm = null;
            boolean attached = false;
            long ctime = System.currentTimeMillis();
            long timeout = 10000;
            Throwable problem = null;

            while (!attached && (System.currentTimeMillis() - ctime) < timeout) {
                try {
                    log.println("... trying");
                    Thread.sleep(1000);
                    vm = attachingConnector.attach(map);

                }
                catch (IOException ioe) {
                    problem = ioe;

                }
                catch (Throwable t) {
                    problem = t;
                }

                if (vm != null)
                    attached = true;
            }

            if (!attached) {
                if (problem != null) {
                    throw new RuntimeException(problem);
                }
                else {
                    //probably not killed emulator is blocking port
                    //could we go with another port?
                    throw new RuntimeException("Timeout");
                }
            }

            log.println("Attached!");

            this.eventQueue = vm.eventQueue();
            DebugServiceImpl.this.setVM(vm);
        }

        public void run() {
            try {
                EventSet eventSet;
                EventIterator eventIterator;
                log.println("Events:\n");
                while (!isDebugSessionOver()) {
                    try {
                        eventSet = this.eventQueue.remove();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                        break;
                    }
                    eventIterator = eventSet.eventIterator();
                    //notify all DebuggerTest instances that an event has come
                    while (eventIterator.hasNext()) {
                        Event event = eventIterator.nextEvent();
                        log.println("\t" + event.toString());
                        DebugServiceImpl.this.eventUpdate(null, event);
                        synchronized (tests) {
                            Iterator testIterator = DebugServiceImpl.this.tests.iterator();
                            while (testIterator.hasNext()) {
                                DebuggerTest test = (DebuggerTest) testIterator.next();
                                test.eventUpdate(DebugServiceImpl.this, event);
                            }
                        }
                    }
                    DebugServiceImpl.this.getVM().resume();
                }
            }
            catch (Throwable t) {
                t.printStackTrace(log);
                t.printStackTrace(ref);
                //try to somehow smooth the end of running vm
                //now emulator hangs when NPE occurs here
                //will this work ?
                DebugServiceImpl.this.debugSessionOver = true;
            }
        }

        private boolean isDebugSessionOver() {
            return DebugServiceImpl.this.debugSessionOver;
        }
    }

    public void eventUpdate(DebugService service, Event event) {
        if (event instanceof VMDeathEvent || event instanceof VMDisconnectEvent) {
            this.debugSessionOver = true;
        }
        else if (event instanceof ExceptionEvent) {
            ExceptionEvent ee = (ExceptionEvent) event;
            log.println("Exception event: " + event.toString());
            log.println("Exception : " + ee.exception().toString());
//          ref.println("Exception event: " + event.toString());
//          ref.println("Exception : " + ee.exception().toString());
        }
    }

    public void addDebuggerTest(DebuggerTest test) {
        synchronized (this.tests) {
            this.tests.add(test);
        }
    }

    /***********************************************************************************************************************************************************************************************************************************************************************************
     * VARIABLE & FIELD VALUES
     */
    //Tested
    public Value getFieldValue(ThreadReference threadReference, String fieldName) throws IncompatibleThreadStateException {
        Location location = threadReference.frame(0).location();
        ReferenceType refType = location.declaringType();
        StackFrame frame = threadReference.frame(0);
        List fields = refType.allFields();
        Iterator it = fields.iterator();
        Field field = null;
        while (it.hasNext()) {
            Field fieldTmp = (Field) it.next();
            if (fieldTmp.name().equals(fieldName))
                field = fieldTmp;
        }
        if (field == null)
            return null;
        if (field.isStatic()) {
            return refType.getValue(field);
        }
        ObjectReference thisRef = frame.thisObject();
        return thisRef.getValue(field);
    }

    //Tested
    public Value getLocalVariableValue(ThreadReference threadReference, String variableName) throws IncompatibleThreadStateException, AbsentInformationException {
        StackFrame frame = threadReference.frame(0);
        LocalVariable variable = frame.visibleVariableByName(variableName);
        if (variable == null)
            return null;
        return frame.getValue(variable);
    }

    /***********************************************************************************************************************************************************************************************************************************************************************************
     * THREADS
     */
    //Tested
    public void suspendThread(ThreadReference threadReference, long durationMS) {
        threadReference.suspend();
        Timer resumeTimer = new Timer(true);
        resumeTimer.schedule(new ThreadResumeTimeTask(threadReference), durationMS);
    }

    private class ThreadResumeTimeTask extends TimerTask {

        private ThreadReference threadToResume;

        public ThreadResumeTimeTask(ThreadReference threadToResume) {
            this.threadToResume = threadToResume;
        }

        public void run() {
            log.println("\t\tresume : " + threadToResume.referenceType().name());
            this.threadToResume.resume();
        }
    }

    /***********************************************************************************************************************************************************************************************************************************************************************************
     * BREAKPOINTS
     */
    //Tested
    public BreakpointRequest setBreakPoint(ReferenceType refType, int lineNumber) throws AbsentInformationException, IncompatibleThreadStateException {
        List lineLocations = refType.allLineLocations();
        Iterator it = lineLocations.iterator();
        Location location = null;
        while (it.hasNext()) {
            Location tmp = (Location) it.next();
            if (lineNumber == tmp.lineNumber()) {
                location = tmp;
                break;
            }
        }
        if (location == null) {
            throw new RuntimeException(refType.name() + " does NOT contain location for line: " + lineNumber);
        }
        BreakpointRequest breakpointRequest = this.eventRequestManager.createBreakpointRequest(location);
        breakpointRequest.enable();
        return breakpointRequest;
    }

    //Tested
    public void removeBreakPoint(BreakpointRequest originalRequest) {
        //using public void deleteEventRequest(EventRequest eventRequest)
        //may cause a ConcurrentModificationException. Instead using deleteEventRequests(List)
        //as described in JDI JavaDoc
        List removeList = new ArrayList();
        removeList.add(originalRequest);
        this.eventRequestManager.deleteEventRequests(removeList);
    }

    //Tested
    public List removeBreakPoints(String className, int lineNumber) throws AbsentInformationException {
        List breakpointRequests = this.eventRequestManager.breakpointRequests();
        ArrayList removed = new ArrayList();
        Iterator it = breakpointRequests.iterator();
        while (it.hasNext()) {
            BreakpointRequest breakpointRequest = (BreakpointRequest) it.next();
            if (breakpointRequest.location().lineNumber() == lineNumber && breakpointRequest.location().declaringType().name().equals(className)) {
                removed.add(breakpointRequest);
            }
        }
        this.eventRequestManager.deleteEventRequests(removed);
        return removed;
    }

    //Tested
    public void removeAllBreakPoints() {
        this.eventRequestManager.deleteAllBreakpoints();
    }

    /***********************************************************************************************************************************************************************************************************************************************************************************
     * STEPPING
     */
    //Tested
    public StepRequest stepOver(ThreadReference threadReference, String classFilter) {
        return this.doStep(threadReference, StepRequest.STEP_LINE, StepRequest.STEP_OVER, classFilter);
    }

    //Tested
    public StepRequest stepInto(ThreadReference threadReference, String classFilter) {
        return this.doStep(threadReference, StepRequest.STEP_LINE, StepRequest.STEP_INTO, classFilter);
    }

    //Tested
    public StepRequest stepOut(ThreadReference threadReference, String classFilter) {
        return this.doStep(threadReference, StepRequest.STEP_LINE, StepRequest.STEP_OUT, classFilter);
    }

    private StepRequest lastRequest;

    private StepRequest doStep(ThreadReference threadReference, int size, int depth, String classFilter) {

        if (this.lastRequest != null) {
            this.eventRequestManager.deleteEventRequest(lastRequest);
        }
        StepRequest stepRequest = this.eventRequestManager.createStepRequest(threadReference, size, depth);
        stepRequest.addClassFilter(classFilter);
        stepRequest.addCountFilter(1);
        stepRequest.setEnabled(true);
        this.lastRequest = stepRequest;
        return stepRequest;
    }

    /***********************************************************************************************************************************************************************************************************************************************************************************
     * VM
     */
    //Tested
    public VirtualMachine getVM() {
        return this.vm;
    }

    public void suspendVM(long durationMS) {
    	DebugServiceImpl.this.getVM().suspend();
        Timer resumeTimer = new Timer(true);
        resumeTimer.schedule(new VMResumeTimeTask(), durationMS);
    }
    private class VMResumeTimeTask extends TimerTask {
        public void run() {
            log.println("\t\tresume VM");
            ref.println("\t\tresume VM");
            DebugServiceImpl.this.getVM().resume();
        }
    }

    //Tested
    public void printVMCapabilities() {
        log.println("canAddMethod :\t" + this.vmCapapbilities.canAddMethod());
        log.println("canGetBytecodes :\t" + this.vmCapapbilities.canGetBytecodes());
        log.println("canGetCurrentContendedMonitor :\t" + this.vmCapapbilities.canGetCurrentContendedMonitor());
        log.println("canGetMonitorInfo :\t" + this.vmCapapbilities.canGetMonitorInfo());
        log.println("canGetOwnedMonitorInfo :\t" + this.vmCapapbilities.canGetOwnedMonitorInfo());
        log.println("canGetSourceDebugExtension :\t" + this.vmCapapbilities.canGetSourceDebugExtension());
        log.println("canGetSyntheticAttribute :\t" + this.vmCapapbilities.canGetSyntheticAttribute());
        log.println("canPopFrames :\t" + this.vmCapapbilities.canPopFrames());
        log.println("canRedefineClasses :\t" + this.vmCapapbilities.canRedefineClasses());
        log.println("canRequestVMDeathEvent :\t" + this.vmCapapbilities.canRequestVMDeathEvent());
        log.println("canUnrestrictedlyRedefineClasses :\t" + this.vmCapapbilities.canUnrestrictedlyRedefineClasses());
        log.println("canUseInstanceFilters :\t" + this.vmCapapbilities.canUseInstanceFilters());
        log.println("canWatchFieldAccess :\t" + this.vmCapapbilities.canWatchFieldAccess());
        log.println("canWatchFieldModification :\t" + this.vmCapapbilities.canWatchFieldModification());
    }

}