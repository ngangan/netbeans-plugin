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
