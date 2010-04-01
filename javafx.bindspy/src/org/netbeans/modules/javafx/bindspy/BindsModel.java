/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
