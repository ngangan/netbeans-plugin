/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
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

package org.netbeans.modules.debugger.javafx.ui.models;

import java.util.HashSet;
import java.util.logging.Logger;
import org.netbeans.api.debugger.javafx.Field;
import org.netbeans.api.debugger.javafx.InvalidExpressionException;
import org.netbeans.api.debugger.javafx.ObjectVariable;
import org.netbeans.api.debugger.javafx.Variable;
import org.netbeans.spi.debugger.javafx.VariablesFilterAdapter;
import org.netbeans.spi.debugger.ui.Constants;
import org.netbeans.spi.viewmodel.TableModel;
import org.netbeans.spi.viewmodel.TreeModel;
import org.netbeans.spi.viewmodel.UnknownTypeException;
import org.openide.ErrorManager;


/**
 *
 * @author   Jan Jancura
 */
public class JavaVariablesFilter extends VariablesFilterAdapter {
    
    public String[] getSupportedTypes () {
        return new String[] {
            "java.lang.String",	//NOI18N
            "java.lang.StringBuffer",	//NOI18N
            
            "java.lang.Character",	//NOI18N
            "java.lang.Integer",	//NOI18N
            "java.lang.Float",	//NOI18N
            "java.lang.Byte",	//NOI18N
            "java.lang.Boolean",	//NOI18N
            "java.lang.Double",	//NOI18N
            "java.lang.Long",	//NOI18N
            "java.lang.Short",	//NOI18N
            
            "java.lang.ref.WeakReference",	//NOI18N
            
            "java.util.ArrayList",	//NOI18N
            "java.util.HashSet",	//NOI18N
            "java.util.LinkedHashSet",	//NOI18N
            "java.util.LinkedList",	//NOI18N
            "java.util.Stack",	//NOI18N
            "java.util.TreeSet",	//NOI18N
            "java.util.Vector",	//NOI18N
            "java.util.Hashtable",	//NOI18N
            "java.util.Hashtable$Entry",	//NOI18N
            "java.util.HashMap",	//NOI18N
            "java.util.HashMap$Entry",	//NOI18N
            "java.util.IdentityHashMap",	//NOI18N
            "java.util.AbstractMap$SimpleEntry",	//NOI18N
            "java.util.TreeMap",	//NOI18N
            "java.util.TreeMap$Entry",	//NOI18N
            "java.util.WeakHashMap",	//NOI18N
            "java.util.LinkedHashMap",	//NOI18N
            "java.util.LinkedHashMap$Entry",	//NOI18N
            
            "java.beans.PropertyChangeSupport"	//NOI18N
        };
    }
    
    public String[] getSupportedAncestors () {
        return new String[] {
        };
    }
    
    /** 
     * Returns filtered children for given parent on given indexes.
     *
     * @param   original the original tree model
     * @throws  NoInformationException if the set of children can not be
     *          resolved
     * @throws  ComputingException if the children resolving process 
     *          is time consuming, and will be performed off-line 
     * @throws  UnknownTypeException if this TreeModelFilter implementation is not
     *          able to resolve dchildren for given node type
     *
     * @return  children for given parent on given indexes
     */
    public Object[] getChildren (
        TreeModel original, 
        Variable variable, 
        int from, 
        int to
    ) throws UnknownTypeException {
        
        String type = variable.getType ();
        
        if (isToArrayType (type)) {
            ObjectVariable ov = (ObjectVariable) variable;
            try {
                ov = (ObjectVariable) ov.invokeMethod (
                    "toArray",	//NOI18N
                    "()[Ljava/lang/Object;",	//NOI18N
                    new Variable [0]
                );
                if (ov == null) {
                    return new Object[] {};
                }
                return original.getChildren(ov, from, to);
            } catch (NoSuchMethodException e) {
                Field elementData = ov.getField("elementData");	//NOI18N
                if (elementData != null) {
                    return original.getChildren(elementData, from, to);
                } else {
                    ErrorManager.getDefault().notify(e);
                }
            } catch (InvalidExpressionException e) {
                // Not a supported operation (e.g. J2ME, see #45543)
                // Or missing context or any other reason
                Logger.getLogger(JavaVariablesFilter.class.getName()).fine("invokeMethod(toArray) "+e.getLocalizedMessage());
                return original.getChildren (variable, from, to);
            }
        }
        if (isMapMapType (type)) 
            try {
                ObjectVariable ov = (ObjectVariable) variable;
                ov = (ObjectVariable) ov.invokeMethod (
                    "entrySet",	//NOI18N
                    "()Ljava/util/Set;",	//NOI18N
                    new Variable [0]
                );
                if (ov == null) {
                    return new Object[] {};
                }
                ov = (ObjectVariable) ov.invokeMethod (
                    "toArray",	//NOI18N
                    "()[Ljava/lang/Object;",	//NOI18N
                    new Variable [0]
                );
                if (ov == null) {
                    return new Object[] {};
                }
                return original.getChildren(ov, from, to);
            } catch (InvalidExpressionException e) {
                // Not a supported operation (e.g. J2ME, see #45543)
                // Or missing context or any other reason
                Logger.getLogger(JavaVariablesFilter.class.getName()).fine("invokeMethod(entrySet) "+e.getLocalizedMessage());
                return original.getChildren (variable, from, to);
            } catch (NoSuchMethodException e) {
                ErrorManager.getDefault().notify(e);
            }
        if ( isMapEntryType (type)
        ) {
            ObjectVariable ov = (ObjectVariable) variable;
            Field[] fs = new Field [2];
            fs [0] = ov.getField ("key");	//NOI18N
            fs [1] = ov.getField ("value");	//NOI18N
            return fs;
        }
        if ( "java.beans.PropertyChangeSupport".equals (type)	//NOI18N
        ) 
            try {
                ObjectVariable ov = (ObjectVariable) variable;
                return ((ObjectVariable) ov.invokeMethod (
                    "getPropertyChangeListeners",
                    "()[Ljava/beans/PropertyChangeListener;",	//NOI18N
                    new Variable [0]
                )).getFields (from, to);
            } catch (InvalidExpressionException e) {
                // Not a supported operation (e.g. J2ME, see #45543)
                // Or missing context or any other reason
                Logger.getLogger(JavaVariablesFilter.class.getName()).fine("invokeMethod(getPropertyChangeListeners) "+e.getLocalizedMessage());
                return original.getChildren (variable, from, to);
            } catch (NoSuchMethodException e) {
                ErrorManager.getDefault().notify(e);
            }
//        if ( type.equals ("java.lang.ref.WeakReference")
//        ) 
//            try {
//                ObjectVariable ov = (ObjectVariable) variable;
//                return new Object [] {ov.invokeMethod (
//                    "get",
//                    "()Ljava/lang/Object;",
//                    new Variable [0]
//                )};
//            } catch (NoSuchMethodException e) {
//                e.printStackTrace ();
//            }
        if ( "java.lang.ref.WeakReference".equals (type)	//NOI18N
        ) {
            ObjectVariable ov = (ObjectVariable) variable;
            return new Object [] {ov.getField ("referent")};	//NOI18N
        }
        return original.getChildren (variable, from, to);
    }

    /**
     * Returns number of filtered children for given variable.
     *
     * @param   original the original tree model
     * @param   variable a variable of returned fields
     *
     * @throws  NoInformationException if the set of children can not be
     *          resolved
     * @throws  ComputingException if the children resolving process
     *          is time consuming, and will be performed off-line
     * @throws  UnknownTypeException if this TreeModelFilter implementation is not
     *          able to resolve dchildren for given node type
     *
     * @return  number of filtered children for given variable
     */
    public int getChildrenCount (TreeModel original, Variable variable) 
    throws UnknownTypeException {
        
        return Integer.MAX_VALUE;
    }

    /**
     * Returns true if node is leaf.
     * 
     * @param   original the original tree model
     * @throws  UnknownTypeException if this TreeModel implementation is not
     *          able to resolve dchildren for given node type
     * @return  true if node is leaf
     */
    public boolean isLeaf (TreeModel original, Variable variable) 
    throws UnknownTypeException {
        String type = variable.getType ();

        // PATCH for J2ME
        if ( isLeafType (type) 
        ) return true;
        return original.isLeaf (variable);
    }
    
    public Object getValueAt (
        TableModel original, 
        Variable variable, 
        String columnID
    ) throws UnknownTypeException {

        if (!(variable instanceof ObjectVariable)) {
            return original.getValueAt (variable, columnID);
        }
        String type = variable.getType ();
        ObjectVariable ov = (ObjectVariable) variable;
        if ( isMapEntryType (type) &&
             ( columnID == Constants.LOCALS_VALUE_COLUMN_ID ||
               columnID == Constants.WATCH_VALUE_COLUMN_ID)
        ) {
            return ov.getField ("key").getValue () + "=>" + 	//NOI18N
                   ov.getField ("value").getValue ();	//NOI18N
        }
        if ( isGetValueType (type) &&
             ( columnID == Constants.LOCALS_VALUE_COLUMN_ID ||
               columnID == Constants.WATCH_VALUE_COLUMN_ID)
        ) {
            return ov.getField ("value").getValue ();	//NOI18N
        }
        if ( isToStringValueType (type) &&
             ( columnID == Constants.LOCALS_VALUE_COLUMN_ID ||
               columnID == Constants.WATCH_VALUE_COLUMN_ID)
        ) {
            try {
                return "\""+ov.getToStringValue ()+"\"";	//NOI18N
            } catch (InvalidExpressionException ex) {
                // Not a supported operation (e.g. J2ME, see #45543)
                // Or missing context or any other reason
                Logger.getLogger(JavaVariablesFilter.class.getName()).fine("getToStringValue() "+ex.getLocalizedMessage());
                if ( (ex.getTargetException () != null) &&
                     (ex.getTargetException () instanceof 
                       UnsupportedOperationException)
                ) {
                    // PATCH for J2ME. see 45543
                    return original.getValueAt (variable, columnID);
                }
                return ex.getLocalizedMessage ();
            }
        }
        return original.getValueAt (variable, columnID);
    }

    public void setValueAt(TableModel original, Variable variable,
                           String columnID, Object value) throws UnknownTypeException {
        String type = variable.getType();
        if (isToStringValueType(type) &&
            (columnID == Constants.LOCALS_VALUE_COLUMN_ID ||
             columnID == Constants.WATCH_VALUE_COLUMN_ID)) {
            String expression = (String) value;
            if (expression.startsWith("\"") && expression.endsWith("\"") && expression.length() > 1) {	//NOI18N
                // Create a new StringBuffer object with the desired content:
                expression = "new " + type + "(\"" + convertToStringInitializer(expression.substring(1, expression.length() - 1)) + "\")";	//NOI18N
                original.setValueAt(variable, columnID, expression);
                return ;
            }
        }
        original.setValueAt(variable, columnID, value);
    }
    
    private static String convertToStringInitializer (String s) {
        StringBuffer sb = new StringBuffer ();
        int i, k = s.length ();
        for (i = 0; i < k; i++)
            switch (s.charAt (i)) {
                case '\b':
                    sb.append ("\\b");	//NOI18N
                    break;
                case '\f':
                    sb.append ("\\f");	//NOI18N
                    break;
                case '\\':
                    sb.append ("\\\\");	//NOI18N
                    break;
                case '\t':
                    sb.append ("\\t");	//NOI18N
                    break;
                case '\r':
                    sb.append ("\\r");	//NOI18N
                    break;
                case '\n':
                    sb.append ("\\n");	//NOI18N
                    break;
                case '\"':
                    sb.append ("\\\"");	//NOI18N
                    break;
                default:
                    sb.append (s.charAt (i));
            }
        return sb.toString();
    }
    
    
    // other methods ...........................................................
    
    private static HashSet getValueType;
    private static boolean isGetValueType (String type) {
        if (getValueType == null) {
            getValueType = new HashSet ();
            getValueType.add ("java.lang.Character");	//NOI18N
            getValueType.add ("java.lang.Integer");	//NOI18N
            getValueType.add ("java.lang.Float");	//NOI18N
            getValueType.add ("java.lang.Byte");	//NOI18N
            getValueType.add ("java.lang.Boolean");	//NOI18N
            getValueType.add ("java.lang.Double");	//NOI18N
            getValueType.add ("java.lang.Long");	//NOI18N
            getValueType.add ("java.lang.Short");	//NOI18N
        }
        return getValueType.contains (type);
    }
    
    private static HashSet leafType;
    private static boolean isLeafType (String type) {
        if (leafType == null) {
            leafType = new HashSet ();
            leafType.add ("java.lang.String");	//NOI18N
            leafType.add ("java.lang.Character");	//NOI18N
            leafType.add ("java.lang.Integer");	//NOI18N
            leafType.add ("java.lang.Float");	//NOI18N
            leafType.add ("java.lang.Byte");	//NOI18N
            leafType.add ("java.lang.Boolean");	//NOI18N
            leafType.add ("java.lang.Double");	//NOI18N
            leafType.add ("java.lang.Long");	//NOI18N
            leafType.add ("java.lang.Short");	//NOI18N
        }
        return leafType.contains (type);
    }
    
    private static HashSet toStringValueType;
    private static boolean isToStringValueType (String type) {
        if (toStringValueType == null) {
            toStringValueType = new HashSet ();
            toStringValueType.add ("java.lang.StringBuffer");	//NOI18N
        }
        return toStringValueType.contains (type);
    }
    
    private static HashSet mapEntryType;
    private static boolean isMapEntryType (String type) {
        if (mapEntryType == null) {
            mapEntryType = new HashSet ();
            mapEntryType.add ("java.util.HashMap$Entry");	//NOI18N
            mapEntryType.add ("java.util.Hashtable$Entry");	//NOI18N
            mapEntryType.add ("java.util.AbstractMap$SimpleEntry");	//NOI18N
            mapEntryType.add ("java.util.LinkedHashMap$Entry");	//NOI18N
            mapEntryType.add ("java.util.TreeMap$Entry");	//NOI18N
        }
        return mapEntryType.contains (type);
    }
    
    private static HashSet mapMapType;
    private static boolean isMapMapType (String type) {
        if (mapMapType == null) {
            mapMapType = new HashSet ();
            mapMapType.add ("java.util.HashMap");	//NOI18N
            mapMapType.add ("java.util.IdentityHashMap");	//NOI18N
            mapMapType.add ("java.util.Hashtable");	//NOI18N
            mapMapType.add ("java.util.TreeMap");	//NOI18N
            mapMapType.add ("java.util.WeakHashMap");	//NOI18N
            mapMapType.add ("java.util.LinkedHashMap");	//NOI18N
            mapMapType.add ("java.util.concurrent.ConcurrentHashMap");	//NOI18N
            mapMapType.add ("java.util.EnumMap");	//NOI18N
        }
        return mapMapType.contains (type);
    }
    
    private static HashSet toArrayType;
    private static boolean isToArrayType (String type) {
        if (toArrayType == null) {
            toArrayType = new HashSet ();
            toArrayType.add ("java.util.ArrayList");	//NOI18N
            toArrayType.add ("java.util.HashSet");	//NOI18N
            toArrayType.add ("java.util.LinkedHashSet");	//NOI18N
            toArrayType.add ("java.util.LinkedList");	//NOI18N
            toArrayType.add ("java.util.Stack");	//NOI18N
            toArrayType.add ("java.util.TreeSet");	//NOI18N
            toArrayType.add ("java.util.Vector");	//NOI18N
            toArrayType.add ("java.util.concurrent.CopyOnWriteArraySet");	//NOI18N
            toArrayType.add ("java.util.EnumSet");	//NOI18N
        }
        return toArrayType.contains (type);
    }
}
