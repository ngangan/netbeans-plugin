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