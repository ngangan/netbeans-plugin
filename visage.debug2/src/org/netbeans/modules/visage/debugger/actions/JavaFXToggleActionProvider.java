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


package org.netbeans.modules.visage.debugger.actions;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.Set;
import org.netbeans.api.debugger.ActionsManager;
import org.netbeans.api.debugger.Breakpoint;
import org.netbeans.api.debugger.DebuggerManager;
import org.netbeans.api.debugger.jpda.JPDADebugger;
import org.netbeans.modules.visage.debugger.Context;
import org.netbeans.modules.visage.debugger.breakpoints.VisageLineBreakpoint;
import org.netbeans.spi.debugger.ActionsProvider.Registration;
import org.netbeans.spi.debugger.ActionsProviderSupport;
import org.netbeans.spi.debugger.ContextProvider;
import org.netbeans.spi.debugger.ui.EditorContextDispatcher;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.URLMapper;

/**
 *
 * @author Michal Skvor
 */
@Registration(path="")
public class VisageToggleActionProvider extends ActionsProviderSupport implements PropertyChangeListener {

    private JPDADebugger debugger;

    public VisageToggleActionProvider() {
        propertyChange( null );
        Context.addPropertyChangeListener( this );
    }

    public VisageToggleActionProvider( ContextProvider contextProvider ) {
        debugger = contextProvider.lookupFirst(null, JPDADebugger.class);
        debugger.addPropertyChangeListener( JPDADebugger.PROP_STATE, this );
        Context.addPropertyChangeListener( this );
    }
    
    @Override
    public void doAction( Object action ) {
        DebuggerManager d = DebuggerManager.getDebuggerManager ();

        // 1) get source name & line number
        int ln = Context.getCurrentLineNumber ();
        String url = Context.getCurrentURL ();
        if (url == null) return;

        // 2) find and remove existing line breakpoint
        VisageLineBreakpoint lb = findBreakpoint( url, ln );
        if (lb != null) {
            d.removeBreakpoint( lb );
            return;
        }
        lb = VisageLineBreakpoint.create( url, ln );
        d.addBreakpoint( lb );
    }

    static VisageLineBreakpoint findBreakpoint( String url, int lineNumber ) {
        Breakpoint[] breakpoints = DebuggerManager.getDebuggerManager().getBreakpoints();
        for( int i = 0; i < breakpoints.length; i++ ) {
            if( !( breakpoints[i] instanceof VisageLineBreakpoint )) {
                continue;
            }
            VisageLineBreakpoint lb = (VisageLineBreakpoint) breakpoints[i];
            if( !lb.getURL().equals( url )) continue;
            if( lb.getLineNumber() == lineNumber ) {
                return lb;
            }
        }
        return null;
    }

    @Override
    public Set getActions() {
        return Collections.singleton( ActionsManager.ACTION_TOGGLE_BREAKPOINT );
    }

    public void propertyChange( PropertyChangeEvent evt ) {
        String url = EditorContextDispatcher.getDefault().getCurrentURLAsString();
        FileObject fo;
        try {
            fo = URLMapper.findFileObject(new URL(url));
        } catch (MalformedURLException muex) {
            fo = null;
        }
        setEnabled( ActionsManager.ACTION_TOGGLE_BREAKPOINT,
            (EditorContextDispatcher.getDefault().getCurrentLineNumber () >= 0) &&
            (fo != null && "text/x-fx".equals(fo.getMIMEType())));    
    }    
}
