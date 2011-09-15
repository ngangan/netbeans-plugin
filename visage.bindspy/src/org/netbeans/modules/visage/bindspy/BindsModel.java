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


package org.netbeans.modules.javafx.bindspy;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;

/**
 *
 * @author Michal Skvor <michal.skvor at sun.com>
 */
public class BindsModel {

//    private HashSet<String> variables = new HashSet<String>();
    private HashMap<String, BindVariable> nodes = new HashMap<String, BindVariable>();

    private BindVariable lastNode;

    public boolean variableExists( String variable ) {
        return nodes.containsKey( variable );
    }

    public void addVariable( BindVariable bindVariable ) {
        lastNode = bindVariable;
        nodes.put( bindVariable.getVariableName(), lastNode );
    }

    public void addConnection( String variableName ) {
        BindConnection connection = new BindConnection( variableName, lastNode.getDirection());
        lastNode.addDependency( connection );
        if( nodes.get( variableName ) == null ) {
            BindVariable bn = new BindVariable( variableName, BindVariable.NONE );
            nodes.put( variableName, bn );
        }
    }

    public BindVariable getVariable( String variableName ) {
        return nodes.get( variableName );
    }

    public boolean hasReference( BindVariable bindVariable ) {
        String varName = bindVariable.getVariableName();
        for( BindVariable bv : nodes.values()) {
            if( bindVariable.getConnections().size() > 0 ) return true;
            for( BindConnection c : bv.getConnections()) {
                if( varName.equals( c.getVariableName())) {
                    return true;
                }
            }
        }
        return false;
    }

    public Collection<BindVariable> getNodes() {
        return nodes.values();
    }

    public static final class BindVariable {

        public static final int NONE = 0;
        public static final int UNIDIRECTIONAL = 1;
        public static final int BIDIRECTIONAL = 2;
        private int direction;

        private String variableName;
        private HashSet<BindConnection> dependencies = new HashSet<BindConnection>();

        private String url;
        private long startPosition;
        private long endPosition;

        public BindVariable( String variableName, int type ) {
            this.variableName = variableName;
            this.direction = type;
        }

        public void setDirection( int direction ) {
            this.direction = direction;
        }

        public void setStartPosition( long startPosition ) {
            this.startPosition = startPosition;
        }

        public long getStartPosition() {
            return startPosition;
        }

        public void setEndPosition( long endPosition ) {
            this.endPosition = endPosition;
        }

        public long getEndPosition() {
            return endPosition;
        }

        public void setURL( String url ) {
            this.url = url;
        }

        public String getURL() {
            return url;
        }

        public String getVariableName() {
            return variableName;
        }

        public int getDirection() {
            return direction;
        }

        public void addDependency( BindConnection connection ) {
            dependencies.add( connection );
        }

        public HashSet<BindConnection> getConnections() {
            return dependencies;
        }
    }

    public static final class BindConnection {
        public static final int NONE = 0;
        public static final int UNIDIRECTIONAL = 1;
        public static final int BIDIRECTIONAL = 2;

        private int direction;
        private String variableName;

        public BindConnection( String variableName, int direction ) {
            this.variableName = variableName;
            this.direction = direction;
        }

        public String getVariableName() {
            return variableName;
        }

        public int getBindDirection() {
            return direction;
        }
        
    }
}
