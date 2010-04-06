/*
 * Created on Mar 12, 2004
 */
package debugger.framework;

import com.sun.jdi.request.EventRequestManager;


/**
 * @author kh148139
 *
 */
public interface DebuggerTest extends DebuggerEventListener {
	
	/**
	 * Determines if the test is suitable to run in the target VM depending
	 * on the VM's capabilities.
	 * 
	 * @param vmCapabilities
	 * @return
	 */
	public boolean isVMCompatible(VMCapabilities vmCapabilities);
	
	/**
	 * Register any initial requests before the VM is resumed, usually this
	 * means registering the intial <code>ClassPrepareRequest</code>, 
	 * <code>ThreadStartRequest</code>, or <code>BreakpointRequest</code>.
	 * 
	 * @param eventRequestManager
	 */
	public void registerRequests(EventRequestManager eventRequestManager);
	
	/**
	 * Remove all requests that are no longer needed once the test is over.
	 * 
	 * @param eventRequestManager
	 */
	public void unregisterRequests(EventRequestManager eventRequestManager);
	
}
