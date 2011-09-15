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
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 */


package org.netbeans.modules.visage.debugger.breakpoints;

import org.netbeans.modules.visage.debugger.Context;
import org.netbeans.spi.debugger.DebuggerServiceRegistration;
import org.openide.util.NbBundle;

import org.netbeans.spi.viewmodel.NodeModel;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.UnknownTypeException;

/**
 *
 * @author Michal Skvor
 */
@DebuggerServiceRegistration( path="BreakpointsView", types={ org.netbeans.spi.viewmodel.NodeModel.class } )
public class VisageBreakpointsNodeModel implements NodeModel {

    public static final String LINE_BREAKPOINT =
        "org/netbeans/modules/debugger/resources/breakpointsView/Breakpoint";

    public static final String LINE_BREAKPOINT_DISABLED =
        "org/netbeans/modules/debugger/resources/breakpointsView/DisabledBreakpoint";

    public String getDisplayName( Object o ) throws UnknownTypeException {
        if( o instanceof VisageLineBreakpoint ) {
            VisageLineBreakpoint b = (VisageLineBreakpoint) o;
            return NbBundle.getMessage( VisageBreakpointsNodeModel.class,
                    "CTL_Visage_Line_Breakpoint",
                    Context.getFileName( b ),
                    "" + b.getLineNumber()
                );
        }
        throw new UnknownTypeException( o );
    }

    public String getShortDescription( Object o ) throws UnknownTypeException {
        if( o instanceof VisageLineBreakpoint ) {
            return NbBundle.getMessage(
                    VisageBreakpointsNodeModel.class,
                    "CTL_Visage_Line_Breakpoint",
                    Context.getFileName((VisageLineBreakpoint)o ),
                    "" + ((VisageLineBreakpoint)o ).getLineNumber()
                );
        }
        throw new UnknownTypeException( o );
    }

    public String getIconBase( Object o ) throws UnknownTypeException {
        if( o instanceof VisageLineBreakpoint ) {
            VisageLineBreakpoint breakpoint = (VisageLineBreakpoint)o;
            if( breakpoint.isEnabled()) {
                return LINE_BREAKPOINT;
            } else {
                return LINE_BREAKPOINT_DISABLED;
            }
        }
        throw new UnknownTypeException( o );
    }

    /**
     *
     * @param l the listener to add
     */
    public void addModelListener( ModelListener l ) {
//        listeners.add (l);
    }

    /**
     *
     * @param l the listener to remove
     */
    public void removeModelListener( ModelListener l ) {
//        listeners.remove (l);
    }
}
