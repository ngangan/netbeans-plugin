/*
 * Created on Mar 12, 2004
 */
package debugger.framework;

import java.io.IOException;
import java.util.List;

import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.Value;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.connect.IllegalConnectorArgumentsException;
import com.sun.jdi.request.BreakpointRequest;
import com.sun.jdi.request.StepRequest;

/**
 * @author kh148139
 *
 */
public interface DebugService {

	/**
	 * Starts the debugging service by connecting to the remote VM and resuming it.
	 * All DebuggerTest instances must be registered with this service before this method is called.
	 * 
	 * @param port
	 * @throws IOException
	 * @throws IllegalConnectorArgumentsException
	 * @throws InterruptedException
	 */
	public abstract void startService(int port) throws IOException, IllegalConnectorArgumentsException, InterruptedException;

	/**
	 * Adds a test that will be run once the <code>startService(int port)</code> is called.
	 * @param test
	 */
	public abstract void addDebuggerTest(DebuggerTest test);

	/**
	 * Explicitly resumes the remote VM.
	 *
	 */
	public abstract VirtualMachine getVM();

	/**
	 * Suspends the remove VM for the specified duration.
	 * 
	 * @param durationMS suspension duration in MS
	 */
	public void suspendVM(long durationMS);
	
	
	/**
	 * Creates and enables a breakpoint in the given location.
	 * 
	 * @param className
	 * @param lineNumber
	 * @return the request that creates the breakpoint, needed to remove the breakpoint later
	 * @throws AbsentInformationException
	 */
	public BreakpointRequest setBreakPoint(ReferenceType refType, int lineNumber) throws AbsentInformationException, IncompatibleThreadStateException;

	/**
	 * Removes a breakpoint that was previously added by calling <code>setBreakPoint(String className, int lineNumber)</code>
	 * 
	 * @param originalRequest
	 */
	public abstract void removeBreakPoint(BreakpointRequest originalRequest);

	/**
	 * Removes all breakpoints set at the location.
	 * 
	 * @param className
	 * @param lineNumber
	 * @return list of all <code>BreakpointRequests</code> for which a breakpoint was removed
	 * @throws AbsentInformationException
	 */
	public abstract List removeBreakPoints(String className, int lineNumber) throws AbsentInformationException;

	/**
	 * Removes all breakpoints in the remote VM.
	 */
	public abstract void removeAllBreakPoints();

	/**
	 * Steps over one line of code.
	 * 
	 * @param threadReference
	 * @param classFilter
	 * @return
	 */
	public abstract StepRequest stepOver(ThreadReference threadReference, String classFilter);

	/**
	 * Steps into code once.
	 * 
	 * @param threadReference
	 * @param classFilter
	 * @return
	 */
	public abstract StepRequest stepInto(ThreadReference threadReference, String classFilter);

	/**
	 * Steps out of code once.
	 * 
	 * @param threadReference
	 * @param classFilter
	 * @return
	 */
	public abstract StepRequest stepOut(ThreadReference threadReference, String classFilter);

	/**
	 * Gets a <code>Value</code> for a field. The field can either be static of non-static.
	 * This value should be cast to an expected type such as
	 * <code>IntegerValue</code> or <code>StringReference</code> etc. 
	 * before it can be of much further use.
	 * 
	 * @param threadReference
	 * @param className
	 * @param fieldName
	 * @return field value or <code>null</code> if no such field exists in scope
	 * @throws IncompatibleThreadStateException if thread referenced by threadReference isn't suspended.
	 */
	public Value getFieldValue(ThreadReference threadReference, String fieldName) throws IncompatibleThreadStateException;
	/**
	 * Gets a <code>Value</code> for a local variable. 
	 * This value should be cast to an expected type such as
	 * <code>IntegerValue</code> or <code>StringReference</code> etc. 
	 * before it can be of much further use.
	 * 
	 * @param threadReference
	 * @param variableName
	 * @return local variable value or <code>null</code> if no such local variable exists in scope
	 * @throws IncompatibleThreadStateException  if thread referenced by threadReference isn't suspended.
	 * @throws AbsentInformationException if code running in VM is missing debugging information
	 */
	public abstract Value getLocalVariableValue(ThreadReference threadReference, String variableName) throws IncompatibleThreadStateException, AbsentInformationException;
	
	public abstract void printVMCapabilities();


	/**
	 * Suspends a thread in the remove VM for the specified duration.
	 * 
	 * @param threadReference the thread that will be suspended
	 * @param durationMS suspension duration in MS
	 */
	public void suspendThread(ThreadReference threadReference, long durationMS);

}