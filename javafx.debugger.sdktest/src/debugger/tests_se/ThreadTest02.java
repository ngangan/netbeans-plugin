/*
 * Created on Mar 29, 2004
 */
package debugger.tests_se;

import java.util.ArrayList;

import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.Location;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.event.BreakpointEvent;
import com.sun.jdi.event.Event;
import com.sun.jdi.event.ThreadStartEvent;
import com.sun.jdi.request.EventRequestManager;
import com.sun.jdi.request.ThreadStartRequest;

import debugger.framework.DebugService;
import debugger.framework.DebuggerTest;
import debugger.framework.VMCapabilities;

/**
 * @author kh148139
 */
public class ThreadTest02 implements DebuggerTest {

    private final static int BREAKPOINT_LINE = 30;

    private final static long SUSPEND_DURATION = 8000;

    public static String THREAD_CLASS_NAME = "j2se_tests.thread.test02.TestCaseThread02";
    public static String RUNNABLE_CLASS_NAME = "j2se_tests.thread.test02.TestCaseRunnable02";
    public static String MAIN_CLASS_NAME = "j2se_tests.thread.test02.ThreadTestCase02";

    private DebugService debugService;
    private ArrayList requests = new ArrayList();

    private java.io.PrintStream ref;

    public ThreadTest02(DebugService debugService, java.io.PrintStream ref) {
        this.ref = ref;
        this.debugService = debugService;
        this.debugService.addDebuggerTest(this);
    }

    public boolean isVMCompatible(VMCapabilities vmCapabilities) {
        return true;
    }

    public void eventUpdate(DebugService service, Event event) {
        if (event instanceof ThreadStartEvent) {
            ThreadStartEvent threadStartEvent = (ThreadStartEvent) event;
            String threadClassName = threadStartEvent.thread().referenceType().name();
            if (threadClassName.equals(THREAD_CLASS_NAME) || threadClassName.equals(RUNNABLE_CLASS_NAME)) {
                ref.println("\t\tThreadStartEvent : " + threadClassName);
                ReferenceType refType = threadStartEvent.thread().referenceType();
                if (threadClassName.equals(THREAD_CLASS_NAME)) {
                    ref.println("\t\tsuspendThread : " + threadClassName);
                    this.debugService.suspendThread(threadStartEvent.thread(), SUSPEND_DURATION);
                }
                try {
                    this.debugService.setBreakPoint(refType, BREAKPOINT_LINE);
                }
                catch (AbsentInformationException e) {
                    e.printStackTrace();
                }
                catch (IncompatibleThreadStateException e) {
                    e.printStackTrace();
                }
            }
        }
        else if (event instanceof BreakpointEvent) {
            BreakpointEvent breakpointEvent = (BreakpointEvent) event;
            Location location = breakpointEvent.location();
            ref.println("\t\tBreakpoint reached in " + location.declaringType().name() + " line " + location.lineNumber());
        }
    }

    public void registerRequests(EventRequestManager eventRequestManager) {
        ThreadStartRequest tsr = eventRequestManager.createThreadStartRequest();
        tsr.enable();
        this.requests.add(tsr);
    }

    public void unregisterRequests(EventRequestManager eventRequestManager) {
        eventRequestManager.deleteEventRequests(requests);
    }

}