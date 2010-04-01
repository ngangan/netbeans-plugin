/*
 * Created on Mar 12, 2004
 */
package debugger.tests_se;

import java.util.ArrayList;
import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.Location;
import com.sun.jdi.event.BreakpointEvent;
import com.sun.jdi.event.ClassPrepareEvent;
import com.sun.jdi.event.Event;
import com.sun.jdi.event.StepEvent;
import com.sun.jdi.request.ClassPrepareRequest;
import com.sun.jdi.request.EventRequestManager;

import debugger.framework.DebugService;
import debugger.framework.DebuggerTest;
import debugger.framework.VMCapabilities;

/**
 * @author kh148139
 */
public class StepTest01 implements DebuggerTest {

    private final static int BREAKPOINT_LINE = 56;
    public static String CLASS_NAME = "j2se_tests.step.test01.StepTestCase01";

    private DebugService debugService;
    private ArrayList requests = new ArrayList();
    private java.io.PrintStream ref;

    public StepTest01(DebugService debugService, java.io.PrintStream ref) {
        this.ref = ref;
        this.debugService = debugService;
        this.debugService.addDebuggerTest(this);
    }

    public boolean isVMCompatible(VMCapabilities vmCapabilities) {
        return true;
    }

    public void eventUpdate(DebugService service, Event event) {
        Location location = null;
        if (event instanceof ClassPrepareEvent) {
            ClassPrepareEvent classPrepareEvent = (ClassPrepareEvent) event;
            if (classPrepareEvent.referenceType().name().equals(CLASS_NAME)) {
                try {
                    this.debugService.setBreakPoint(classPrepareEvent.referenceType(), BREAKPOINT_LINE);
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
            location = breakpointEvent.location();
            ref.println("\t\tbreakpoint reached in class: " + location.declaringType().name() + " on line: " + location.lineNumber());
            this.debugService.stepOver(breakpointEvent.thread(), CLASS_NAME);
        }
        else if (event instanceof StepEvent) {
            StepEvent stepEvent = (StepEvent) event;
            location = stepEvent.location();
            ref.println("\t\tstepped in class: " + location.declaringType().name() + " to line: " + location.lineNumber());
            if (location.lineNumber() == 57) {
                this.debugService.stepInto(stepEvent.thread(), CLASS_NAME);
            }
            else if (location.lineNumber() == 58) {
                this.debugService.stepInto(stepEvent.thread(), CLASS_NAME);
            }
            else if (location.lineNumber() == 46) {
                this.debugService.stepInto(stepEvent.thread(), CLASS_NAME);
            }
            else if (location.lineNumber() == 51) {
                this.debugService.stepOut(stepEvent.thread(), CLASS_NAME);
            }
            else if (location.lineNumber() == 58) {
                this.debugService.getVM().resume();
            }
            else {
                this.debugService.stepOver(stepEvent.thread(), CLASS_NAME);
            }
        }
    }

    public void registerRequests(EventRequestManager eventRequestManager) {
        ClassPrepareRequest cpr = eventRequestManager.createClassPrepareRequest();
        cpr.addClassFilter(CLASS_NAME);
        cpr.enable();
        this.requests.add(cpr);
    }

    public void unregisterRequests(EventRequestManager eventRequestManager) {
        eventRequestManager.deleteEventRequests(this.requests);
    }

}