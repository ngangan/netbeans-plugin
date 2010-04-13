/*
 * Created on Mar 24, 2004
 */
package debugger.tests_se;

import java.io.PrintStream;
import java.util.ArrayList;

import com.sun.jdi.ClassType;
import com.sun.jdi.event.ClassPrepareEvent;
import com.sun.jdi.event.Event;
import com.sun.jdi.request.ClassPrepareRequest;
import com.sun.jdi.request.EventRequestManager;

import debugger.framework.DebugService;
import debugger.framework.DebuggerTest;
import debugger.framework.VMCapabilities;


/**
 * @author kh148139
 */
public class SuperTypeTest01 implements DebuggerTest {

    public final static String CLASS_NAME = "j2se_tests.supertype.test01.SuperTypeTestCase01";
	
	private DebugService debugService;
	private ArrayList requests = new ArrayList();
	
    private PrintStream ref;
    
	public SuperTypeTest01(DebugService debugService, java.io.PrintStream ref) {
        this.ref = ref;
		this.debugService = debugService;
		this.debugService.addDebuggerTest(this);
	}

	public boolean isVMCompatible(VMCapabilities vmCapabilities) {
		return true;
	}

	public void eventUpdate(DebugService service, Event event) {
		if (event instanceof ClassPrepareEvent) {
			ClassPrepareEvent classPrepareEvent = (ClassPrepareEvent) event;
            ClassType classType = (ClassType) classPrepareEvent.referenceType();
			if (classType.name().equals(CLASS_NAME)) {
                ClassType superType = null;
                do {
                    superType = classType.superclass();
                    ref.println("ClassType = " + classType.name() + "\t\t superType = " + superType);
                    classType = superType;
                } while (classType != null);
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
