/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package tests;

import debugger.framework.DebugServiceDriverSE;
import junit.framework.Test;
import junit.framework.TestResult;

/**
 *
 * @author Michal Skvor <michal.skvor at sun.com>
 */
public class Runner {

    public static void main( String[] args ) {
        Test t = DebugServiceDriverSE.suite();

        TestResult tr = new TestResult();
        t.run( tr );
    }
}
