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
 * Created on Apr 2, 2004
 */
package debugger.framework;

import com.sun.jdi.VirtualMachine;


/**
 * @author kh148139
 *
 */
public class VMCapabilities {
	private boolean canAddMethod;
	private boolean canGetBytecodes;
	private boolean canGetCurrentContendedMonitor;
	private boolean canGetMonitorInfo;
	private boolean canGetOwnedMonitorInfo;
	private boolean canGetSourceDebugExtension;
	private boolean canGetSyntheticAttribute;
	private boolean canPopFrames;
	private boolean canRedefineClasses;
	private boolean canRequestVMDeathEvent;
	private boolean canUnrestrictedlyRedefineClasses;
	private boolean canUseInstanceFilters;
	private boolean canWatchFieldAccess;
	private boolean canWatchFieldModification;
	
	public VMCapabilities(VirtualMachine vm) {
		this.canAddMethod = vm.canAddMethod();
		this.canGetBytecodes = vm.canGetBytecodes();
		this.canGetCurrentContendedMonitor = vm.canGetCurrentContendedMonitor();
		this.canGetMonitorInfo = vm.canGetMonitorInfo();
		this.canGetOwnedMonitorInfo = vm.canGetOwnedMonitorInfo();
		this.canGetSourceDebugExtension = vm.canGetSourceDebugExtension();
		this.canGetSyntheticAttribute = vm.canGetSyntheticAttribute();
		this.canPopFrames = vm.canPopFrames();
		this.canRedefineClasses = vm.canRedefineClasses();
		this.canRequestVMDeathEvent = vm.canRequestVMDeathEvent();
		this.canUnrestrictedlyRedefineClasses = vm.canUnrestrictedlyRedefineClasses();
		this.canUseInstanceFilters = vm.canUseInstanceFilters();
		this.canWatchFieldAccess = vm.canWatchFieldAccess();
		this.canWatchFieldModification = vm.canWatchFieldModification();
	}
	
	public boolean canAddMethod() {
		return this.canAddMethod;
	}
	public boolean canGetBytecodes() {
		return this.canGetBytecodes;
	}
	public boolean canGetCurrentContendedMonitor() {
		return this.canGetCurrentContendedMonitor;
	}
	public boolean canGetMonitorInfo() {
		return this.canGetMonitorInfo;
	}
	public boolean canGetOwnedMonitorInfo() {
		return this.canGetOwnedMonitorInfo;
	}
	public boolean canGetSourceDebugExtension() {
		return this.canGetSourceDebugExtension;
	}
	public boolean canGetSyntheticAttribute() {
		return this.canGetSyntheticAttribute;
	}
	public boolean canPopFrames() {
		return this.canPopFrames;
	}
	public boolean canRedefineClasses() {
		return this.canRedefineClasses;
	}
	public boolean canRequestVMDeathEvent() {
		return this.canRequestVMDeathEvent;
	}
	public boolean canUnrestrictedlyRedefineClasses() {
		return this.canUnrestrictedlyRedefineClasses;
	}
	public boolean canUseInstanceFilters() {
		return this.canUseInstanceFilters;
	}
	public boolean canWatchFieldAccess() {
		return this.canWatchFieldAccess;
	}
	public boolean canWatchFieldModification() {
		return this.canWatchFieldModification;
	}
}
