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

import com.sun.javafx.jdi.FXField;
import com.sun.javafx.jdi.FXSequenceReference;
import com.sun.javafx.jdi.FXValue;
import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.Value;
import java.util.ArrayList;
import java.util.List;
import org.netbeans.api.debugger.jpda.ClassVariable;
import org.netbeans.api.debugger.jpda.Field;
import org.netbeans.api.debugger.jpda.InvalidExpressionException;
import org.netbeans.api.debugger.jpda.JPDAClassType;
import org.netbeans.api.debugger.jpda.LocalVariable;
import org.netbeans.api.debugger.jpda.ObjectVariable;
import org.netbeans.api.debugger.jpda.Super;
import org.netbeans.api.debugger.jpda.This;
import org.netbeans.modules.debugger.jpda.expr.JDIVariable;
import org.netbeans.spi.debugger.DebuggerServiceRegistration;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.TreeModel;
import org.netbeans.spi.viewmodel.TreeModelFilter;
import org.netbeans.spi.viewmodel.UnknownTypeException;

/**
 *
 * @author Michal Skvor
 */
@DebuggerServiceRegistration( path="netbeans-JPDASession/FX/LocalsView",types={ org.netbeans.spi.viewmodel.TreeModelFilter.class } )
public class JavaFXVariablesFilter implements TreeModelFilter {

    /** Creates new JavaFXVariablesFilter instance */
    public JavaFXVariablesFilter() {}

    /**
     *
     * Returns filtered root of hierarchy.
     *
     * @param   original the original tree model
     * @return  filtered root of hierarchy
     */
    public Object getRoot( TreeModel original ) {
        return original.getRoot();
    }

    public Object[] getChildren( TreeModel original, Object parent, int from, int to ) throws UnknownTypeException {
        Object[] visibleChildren = null;

        if( parent.equals( original.getRoot())) {
            // Retrieve children
            int parentChildrenCount = original.getChildrenCount( parent );
            Object[] children = original.getChildren( parent, 0, parentChildrenCount );
            parentChildrenCount = children.length;
            List vc = new ArrayList();
            for( int j = 0; j < parentChildrenCount; j++ ) {
                Object child = children[j];
                if( child instanceof JPDAClassType || child instanceof This ) {
                    Object[] ch = original.getChildren( child, from, to );
                    for( int i = 0; i < ch.length; i++ ) {
                        Object obj = ch[i];
    //                    System.out.println(" - " + obj );
                        if( obj instanceof ClassVariable ) {
                            ClassVariable cv = (ClassVariable)obj;
    //                        vc.add( obj );
                        } else if( obj instanceof Field ) {
                            Field f = (Field)obj;
                            vc.add( obj );
                        }
                    }
                } else if( child instanceof LocalVariable ) {
                    LocalVariable local = (LocalVariable)child;
                    // Helper Sequences out
//                    if( local.getDeclaredType().startsWith( "com.sun.javafx.runtime." )) continue;
//                    if( local.getName().endsWith( "$ind" )) continue;
//                    if( local.getName().endsWith( "$limit" )) continue;
//                    // Skip all internal jfx$ variables
//                    if( local.getName().startsWith( "jfx$" )) continue;
                    vc.add( child );
                }
            }
            List<Object> vvv = vc.subList( from, to > vc.size() ? vc.size() : to );
            for( Object o : vvv ) {
//                System.out.println(" - " + o );
            }
            return vc.subList( from, to > vc.size() ? vc.size() : to ).toArray();
        } else {
            // Root static class
            boolean seqType = false;
            List vc = new ArrayList();
            if( parent instanceof ObjectVariable ) {
                ObjectVariable ov = (ObjectVariable)parent;

                JDIVariable jdiv = (JDIVariable)ov;
                Value v = jdiv.getJDIValue();
                if( v instanceof FXSequenceReference ) {
                    FXSequenceReference seq = (FXSequenceReference)v;
                    
                    for( int i = 0; i < seq.length(); i++ ) {
                        vc.add( new SequenceField( "[" + i + "]", (FXValue) seq.getValue( i )));
                    }
                    seqType = true;
                } 
            }
            if( !seqType ) {
                Object[] children = original.getChildren( parent, from, to );
                for( int i = 0; i < children.length; i++ ) {
                    Object child = children[i];
                    if( child instanceof Field ) {
                        Field f = (Field)child;
    //                    if( f.getName().startsWith( "$" )) {
                            vc.add( child );
    //                    }
                    } else {
    //                    System.out.println(" - " + child.toString());
                    }
                }
            }
            visibleChildren = vc.subList( from, to > vc.size() ? vc.size() : to ).toArray();
        }
        return visibleChildren;
    }

    public int getChildrenCount(TreeModel original, Object node) throws UnknownTypeException {
        return Integer.MAX_VALUE;
    }

    public boolean isLeaf( TreeModel original, Object node ) throws UnknownTypeException {
        boolean il;

        if( node instanceof Field ) {
            Field f = (Field)node;
            if( "java.lang.String".equals( f.getType())) {
                return true;
            }
        }
        il = original.isLeaf( node );

        return il;
    }

    public void addModelListener( ModelListener l ) {
    }

    public void removeModelListener( ModelListener l ) {
    }

    /**
     * Helper representation of Sequence
     */
    private class SequenceField implements Field {

        private String name;
        private FXValue value;

        public SequenceField( String name, FXValue value ) {
            this.name = name;
            this.value = value;
        }

        public String getName() {
            return name;
        }

        public String getClassName() {
            return value.type().name();
        }

        public JPDAClassType getDeclaringClass() {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        public String getDeclaredType() {
            return value.type().name();
        }

        public boolean isStatic() {
            return false;
        }

        public void setValue(String string) throws InvalidExpressionException {
            throw new UnsupportedOperationException("Not supported yet.");
        }

        public String getType() {
            return value.type().name();
        }

        public String getValue() {
            return value.toString();
        }
    }
}
