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
 * Created on Mar 11, 2004
 */
package debugger.framework;

import java.io.IOException;

import debugger.tests_se.ReferenceTypeTest01;
import debugger.tests_se.VMTest01;
import debugger.tests_se.BreakPointTest01;
import debugger.tests_se.EvaluationTest01;
import debugger.tests_se.StepTest01;
import debugger.tests_se.SuperTypeTest01;
import debugger.tests_se.ThreadTest02;

import org.netbeans.junit.NbTestCase;
import junit.framework.Test;
import junit.framework.TestSuite;
import java.text.MessageFormat;
import java.io.*;

/**
 * @author kh148139
 *  
 *  command line to compile:	[CDC]/bin/javac -target 1.4 -source 1.4 -bootclasspath [CDC]/lib/btclasses.zip [FILE.java]
 *  command line to debug:		[CDC]/bin/cvm -Xrunjdwp:transport=dt_socket,server=y,address=8000 -Xdebug [CLASS]
 *  
 */
public class DebugServiceDriverSE extends NbTestCase {

    /** classes to test must be ! same order as in test suite */

    static int testNo = 0;
    Process process;

    //must set from ant from system properties
    static String bootClasspath = System.getProperty("bootClasspath");
    static int address = 8000; //Integer.parseInt(System.getProperty( "address" ));

    public DebugServiceDriverSE(String name) {
        super(name);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest( new DebugServiceDriverSE( "breakpointTest01" ));
//        suite.addTest(new DebugServiceDriverSE("evaluationTest01"));
//        suite.addTest(new DebugServiceDriverSE("stepTest01"));
//        suite.addTest(new DebugServiceDriverSE("threadTest02"));
//        suite.addTest(new DebugServiceDriverSE("superTypeTest01"));
//        suite.addTest( new DebugServiceDriverSE( "vmTest01" ));
//        suite.addTest(new DebugServiceDriverSE("referenceTypeTest01"));
        return suite;
    }

    
    
    @Override
    public void compareReferenceFiles() {
        String product = System.getProperty("product");
        String platform = System.getProperty("platform");
        String[] suffixes = {
            "." + product + "." + platform,
            "." + product,
            "." + platform,
            ""
        };
        String dataDir = System.getProperty("xtest.data");
        String fullClassName = this.getClass().getName();
        String className = fullClassName;
        int lastDot = fullClassName.lastIndexOf('.');
        if (lastDot != -1) {
            className = fullClassName.substring(lastDot + 1);
        }
        String goldenFileNameBase = className + "/" + this.getName();
        String goldenFileName = null;
        for (int i = 0; i < suffixes.length && goldenFileName == null; i++) {
            String goldenFilePath = "data/goldenFiles/"
                    + goldenFileNameBase + suffixes[i] + ".pass";
            if (this.getClass().getResource(goldenFilePath) != null) {
                goldenFileName = this.getName() + suffixes[i] + ".pass";
            }
        }
        File testFile = null;
        try {
            testFile = new File(getWorkDir(),this.getName()+".ref");
        } catch( IOException ioe ) {
            ioe.printStackTrace();
        }
        assertNotNull(goldenFileName);
        compareReferenceFiles(
                this.getName()+".ref",
                goldenFileName,
                this.getName()+".diff");
    }
    
    /**
     * Put additional tests here
     */
    public void superTypeTest01() {
        try {
            DebugService debugService = new DebugServiceImpl(getLog(), getRef());
            new SuperTypeTest01(debugService, getRef());
            process = startProcess(getProcessCommandLine(SuperTypeTest01.CLASS_NAME));
            debugService.startService(address);
            log("DebugService finished superTypeTest01");
            killProcess(process);
            compareReferenceFiles();
        }
        catch (Exception e) {
            e.printStackTrace(getLog());
            e.printStackTrace();
            fail();
        }
    }

    public void evaluationTest01() {
        try {
            DebugService debugService = new DebugServiceImpl(getLog(), getRef());
            new EvaluationTest01(debugService, getRef());
            process = startProcess(getProcessCommandLine(EvaluationTest01.CLASS_NAME));
            debugService.startService(address);
            log("DebugService finished evaluationTest01");
            killProcess(process);
            compareReferenceFiles();
        }
        catch (Exception e) {
            e.printStackTrace(getLog());
            e.printStackTrace();
            fail();
        }
    }

    public void breakpointTest01() {
        try {
            DebugService debugService = new DebugServiceImpl(getLog(), getRef());
            new BreakPointTest01(debugService, getRef());
            process = startProcess(getProcessCommandLine(BreakPointTest01.CLASS_NAME));
            debugService.startService(address);
            log("DebugService finished breakpointTest01");
            killProcess(process);
            compareReferenceFiles();
        }
        catch (Exception e) {
            e.printStackTrace(getLog());
            e.printStackTrace();
            fail();
        }
    }

    public void stepTest01() {
        try {
            DebugService debugService = new DebugServiceImpl(getLog(), getRef());
            new StepTest01(debugService, getRef());
            process = startProcess(getProcessCommandLine(StepTest01.CLASS_NAME));
            debugService.startService(address);
            log("DebugService finished stepTest01");
            killProcess(process);
            compareReferenceFiles();
        }
        catch (Exception e) {
            e.printStackTrace(getLog());
            e.printStackTrace();
            fail();
        }
    }

    public void threadTest02() {
        try {
            DebugService debugService = new DebugServiceImpl(getLog(), getRef());
            new ThreadTest02(debugService, getRef());
            process = startProcess(getProcessCommandLine(ThreadTest02.MAIN_CLASS_NAME));
            debugService.startService(address);
            log("DebugService finished threadTest02");
            killProcess(process);
            compareReferenceFiles();
        }
        catch (Exception e) {
            e.printStackTrace(getLog());
            e.printStackTrace();
            fail();
        }
    }

    public void vmTest01() {
        try {
            DebugService debugService = new DebugServiceImpl(getLog(), getRef());
            new VMTest01(debugService, getRef());
            process = startProcess( getProcessCommandLine( VMTest01.MAIN_CLASS_NAME ));
            debugService.startService( address );
            log( "DebugService finished vmTest01" );
            killProcess(process);
            compareReferenceFiles();
        }
        catch (Exception e) {
            e.printStackTrace(getLog());
            e.printStackTrace();
            fail();
        }
    }
    
    public void referenceTypeTest01() {
        try {
            DebugService debugService = new DebugServiceImpl(getLog(), getRef());
            new ReferenceTypeTest01(debugService, getRef());
            process = startProcess(getProcessCommandLine(ReferenceTypeTest01.MAIN_CLASS_NAME));
            debugService.startService(address);
            log("DebugService finished referenceTypeTest01");
            killProcess(process);
            compareReferenceFiles();
        }
        catch (Exception e) {
            e.printStackTrace(getLog());
            e.printStackTrace();
            fail();
        }
    }

    /*
     * Some infrastructure
     */

    /** always starts debugging process before test execution */
    protected String getProcessCommandLine( String className ) {

        /*
         * check if address is busy
         */
        boolean canconnect = false;
        java.net.Socket sc = new java.net.Socket();
        try {
            log("... trying to check socket on " + address);
            sc.connect(new java.net.InetSocketAddress("localhost", address), 2000);

        }
        catch (java.io.IOException ioe) {
            log(ioe.getMessage());
            canconnect = true;
        }

        if (canconnect) {
            log("... socket successfuly opened.");
            //close socket
            try {
                log("... closing socket.");
                sc.close();
            }
            catch (Exception e) {
                log(e.getMessage());
            }
        }
        else {
            address = address + new java.util.Random().nextInt(1000);
            log("... changing address to " + address);
        }

        //String commandline = MessageFormat.format(System.getProperty("processcommandline"), new Object[]{midletClass, Integer.toString(address)});
//        String visagehome = "D:/netbeans/visage/build/netbeans/visage/visage-sdk/";
        String visagehome = System.getProperty( "visage.home" );
        String cmd = visagehome + "/bin/visage -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address={1} -cp tests/TestSuite.jar {0}";
        String commandline = MessageFormat.format( cmd, new Object[]{ className , Integer.toString( address )});
        return commandline;
    }

    protected void killProcess(Process processToKill) {
        log("Process killed.");
        processToKill.destroy();
        try {
            processToKill.waitFor();
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    private Process startProcess(String commandline) {
        try {
            log("Starting VM with command line:");
            log(commandline);
            process = Runtime.getRuntime().exec(commandline);
            log(process.toString());
            redirectOutput();

        }
        catch (Throwable t) {
            t.printStackTrace();
        }

        return process;
    }

    protected void redirectOutput(/* final PrintStream out, final PrintStream err */) {
        //output
        Thread outReader = new Thread() {

            public void run() {
                try {
                    BufferedReader bfr = new BufferedReader(new InputStreamReader(process.getInputStream()));
                    String line = null;
                    while (bfr != null && null != (line = bfr.readLine()))
                        log(line);

                    bfr.close();
                }
                catch (IOException ioe) {
                    // just ignore it :)
                    //ioe.printStackTrace();
                }
            }
        };

        //error
        Thread errReader = new Thread() {

            public void run() {
                try {
                    BufferedReader bfr = new BufferedReader(new InputStreamReader(process.getErrorStream()));
                    String line = null;
                    while (bfr != null && null != (line = bfr.readLine()))
                        //err.println(line);
                        log(line);

                    bfr.close();

                }
                catch (IOException ioe) {
                    // just ignore it :)
                    //ioe.printStackTrace();
                }
            }
        };

        outReader.start();
        errReader.start();
    }

    public void log(String message) {
        getLog().println(message);
        System.out.println(message);
    }

}