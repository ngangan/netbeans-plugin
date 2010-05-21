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
 * Created on Apr 1, 2004
 */
package debugger.tests_se;

import java.util.ArrayList;

import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.IntegerValue;
import com.sun.jdi.Location;
import com.sun.jdi.StringReference;
import com.sun.jdi.ThreadReference;
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
 *  
 */
public class EvaluationTest01 implements DebuggerTest {

    public static String CLASS_NAME = "j2se_tests.evaluation.test01.EvaluationTestCase01";

    private DebugService debugService;
    private ArrayList requests = new ArrayList();
    private java.io.PrintStream ref;

    public EvaluationTest01(DebugService debugService, java.io.PrintStream ref) {
        this.ref = ref;
        this.debugService = debugService;
        this.debugService.addDebuggerTest(this);
    }

    public boolean isVMCompatible(VMCapabilities vmCapabilities) {
        return true;
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

    public void eventUpdate(DebugService service, Event event) {
        if (event instanceof ClassPrepareEvent) {
            ClassPrepareEvent classPrepareEvent = (ClassPrepareEvent) event;
            if (classPrepareEvent.referenceType().name().equals(CLASS_NAME)) {
                try {
                    this.debugService.setBreakPoint(classPrepareEvent.referenceType(), 17);
                    this.debugService.setBreakPoint(classPrepareEvent.referenceType(), 31);
                    this.debugService.setBreakPoint(classPrepareEvent.referenceType(), 55);
                    this.debugService.setBreakPoint(classPrepareEvent.referenceType(), 60);
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
            ref.println("\t\tbreakpoint reached in class: " + location.declaringType().name() + " on line: " + location.lineNumber());
            this.evaluateVariables(location, breakpointEvent.thread());
            this.debugService.stepOver(breakpointEvent.thread(), CLASS_NAME);
        }
        else if (event instanceof StepEvent) {
            StepEvent stepEvent = (StepEvent) event;
            Location location = stepEvent.location();
            ref.println("\t\tstepped in class: " + location.declaringType().name() + " on line: " + location.lineNumber());
            this.evaluateVariables(stepEvent.location(), stepEvent.thread());
            this.debugService.stepOver(stepEvent.thread(), CLASS_NAME);
        }
    }

    private void evaluateVariables(Location loc, ThreadReference thread) {
        if (loc.lineNumber() == 55) {
            try {
                StringReference strRef = (StringReference) this.debugService.getLocalVariableValue(thread, "str");
                ref.println("\t\t\tValue of 'str' = " + strRef.value());
            }
            catch (IncompatibleThreadStateException e) {
                e.printStackTrace();
            }
            catch (AbsentInformationException e) {
                e.printStackTrace();
            }
        }
        if (loc.lineNumber() > 35 && loc.lineNumber() < 59) {
            try {
                IntegerValue intVal = (IntegerValue) this.debugService.getFieldValue(thread, "intVar");
                ref.println("\t\t\tValue of 'intVar' = " + (intVal != null ? Integer.toString(intVal.value()) : "null"));
            }
            catch (IncompatibleThreadStateException e) {
                e.printStackTrace();
            }
        }
        try {
            IntegerValue intVal = (IntegerValue) this.debugService.getFieldValue(thread, "intVarStat");
            ref.println("\t\t\tValue of 'intVar' = " + (intVal != null ? Integer.toString(intVal.value()) : "null"));
        }
        catch (IncompatibleThreadStateException e) {
            e.printStackTrace();
        }

    }
}