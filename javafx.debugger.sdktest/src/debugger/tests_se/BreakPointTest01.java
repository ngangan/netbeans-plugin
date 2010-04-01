/*
 * Created on Mar 24, 2004
 */
package debugger.tests_se;

import java.util.ArrayList;
import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.Location;
import com.sun.jdi.event.BreakpointEvent;
import com.sun.jdi.event.ClassPrepareEvent;
import com.sun.jdi.event.Event;
import com.sun.jdi.request.BreakpointRequest;
import com.sun.jdi.request.ClassPrepareRequest;
import com.sun.jdi.request.EventRequestManager;

import debugger.framework.DebugService;
import debugger.framework.DebuggerTest;
import debugger.framework.VMCapabilities;

/**
 * @author kh148139
 */
public class BreakPointTest01 implements DebuggerTest {

    private final static int BREAKPOINT_LINE = 31;
    public static String CLASS_NAME = "j2se_tests.breakpoint.test01.BreakpointTestCase01";

    private DebugService debugService;
    private ArrayList requests = new ArrayList();
    private java.io.PrintStream ref;

    public BreakPointTest01(DebugService debugService, java.io.PrintStream ref) {
        this.ref = ref;
        this.debugService = debugService;
        this.debugService.addDebuggerTest(this);
    }

    public boolean isVMCompatible(VMCapabilities vmCapabilities) {
        return true;
    }

    BreakpointRequest req;
    int hits;

    public void eventUpdate(DebugService service, Event event) {
        Location location = null;
        //set the initial breakpoint
        if (event instanceof ClassPrepareEvent) {
            ClassPrepareEvent classPrepareEvent = (ClassPrepareEvent) event;
            if (classPrepareEvent.referenceType().name().equals(CLASS_NAME)) {
                try {
                    req = this.debugService.setBreakPoint(classPrepareEvent.referenceType(), BREAKPOINT_LINE);
                    this.debugService.setBreakPoint(classPrepareEvent.referenceType(), BREAKPOINT_LINE + 1);
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
            if (location.declaringType().name().equals(CLASS_NAME)) {
                ref.println("\t\tbreakpoint reached in class: " + location.declaringType().name() + " on line: " + location.lineNumber());
                if (location.lineNumber() == BREAKPOINT_LINE + 1 && ++hits == 3) {
                    try {
                        this.debugService.removeBreakPoint(req);
                        this.debugService.removeBreakPoints(CLASS_NAME, BREAKPOINT_LINE + 1);
                        this.debugService.setBreakPoint(breakpointEvent.location().declaringType(), 51);
                    }
                    catch (AbsentInformationException e) {
                        e.printStackTrace();
                    }
                    catch (IncompatibleThreadStateException e) {
                        e.printStackTrace();
                    }
                }
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