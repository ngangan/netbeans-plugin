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


package org.netbeans.modules.javafx.debugger.variablesfiltering;

import java.awt.datatransfer.Transferable;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import org.netbeans.api.debugger.jpda.Field;
import org.netbeans.api.debugger.jpda.ObjectVariable;
import org.netbeans.spi.debugger.DebuggerServiceRegistration;
import org.netbeans.spi.viewmodel.ExtendedNodeModel;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.NodeModel;
import org.netbeans.spi.viewmodel.NodeModelFilter;
import org.netbeans.spi.viewmodel.UnknownTypeException;
import org.openide.util.datatransfer.PasteType;

/**
 *
 * @author Michal Skvor
 */
@DebuggerServiceRegistration( path="netbeans-JPDASession/FX/LocalsView", types={ org.netbeans.spi.viewmodel.NodeModelFilter.class } )
public class JavaFXVariablesNodeModelFilter implements NodeModelFilter {

    private final Collection<ModelListener> modelListeners = new HashSet<ModelListener>();

    public JavaFXVariablesNodeModelFilter() {
    }
    
    public String getDisplayName( NodeModel original, Object node ) throws UnknownTypeException {
        if( node instanceof Field ) {
            Field f = (Field)node;
            return f.getName();
        }
        String dn = original.getDisplayName( node );
        return dn;
    }

    public boolean canRename( ExtendedNodeModel original, Object node ) throws UnknownTypeException {
        return false;
    }

    public boolean canCopy( ExtendedNodeModel original, Object node ) throws UnknownTypeException {
        return false;
    }

    public boolean canCut( ExtendedNodeModel original, Object node ) throws UnknownTypeException {
        return false;
    }

    public Transferable clipboardCopy( ExtendedNodeModel original, Object node ) throws IOException, UnknownTypeException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Transferable clipboardCut( ExtendedNodeModel original, Object node ) throws IOException, UnknownTypeException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public PasteType[] getPasteTypes( ExtendedNodeModel original, Object node, Transferable t ) throws UnknownTypeException {
        return new PasteType[0];
    }

    public void setName( ExtendedNodeModel original, Object node, String name ) throws UnknownTypeException {
    }

    public String getIconBaseWithExtension( ExtendedNodeModel original, Object node ) throws UnknownTypeException {
        String ib = "";
        ib = original.getIconBaseWithExtension( node );
        return ib;
    }

    public String getIconBase( NodeModel original, Object node ) throws UnknownTypeException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public String getShortDescription( NodeModel original, Object node ) throws UnknownTypeException {
        String sd = "";

        return sd;
    }

    public void addModelListener( ModelListener l ) {
        modelListeners.add( l );
    }

    public void removeModelListener( ModelListener l ) {
        modelListeners.remove( l );
    }

}
