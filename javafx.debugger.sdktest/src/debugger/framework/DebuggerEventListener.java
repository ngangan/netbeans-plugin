/*
 * Created on Mar 12, 2004
 */
package debugger.framework;

import com.sun.jdi.event.Event;

/**
 * @author kh148139
 *
 */
public interface DebuggerEventListener {

	public void eventUpdate(DebugService service, Event event);

}