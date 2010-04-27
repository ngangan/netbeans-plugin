/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
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
package org.netbeans.modules.javafx.fxd.composer.uistub;

import com.sun.javafx.tools.fxd.container.generator.IndentedWriter;
import com.sun.javafx.tools.fxd.container.generator.PackagerUtils;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 *
 * @author Pavel
 */
public class AttributeHolder {
    private static final String GROUP_TYPE = "Group";
    private static final String NODE_TYPE = "Node";

    private final Map<String, AttributeInstance> m_attrs = new TreeMap<String, AttributeInstance>();
    private boolean m_strictTypes = false;
    
    public String getDeclarationType( final String typeName) {
        return m_strictTypes ? typeName : NODE_TYPE;
    }
    
    private abstract class AttributeInstance {
        protected       String  m_attrName;
        protected       String  m_varName;
        protected final String  m_attrType;

        protected AttributeInstance( String attrName, String attrType) {
            m_attrName = attrName;
            m_attrType = attrType;
            m_varName = m_attrName;
        }
        
        public final String getName() {
            return m_attrName;
        }

        public final String getVarName() {
            return m_varName;
        }

        public final String getType() {
            return getDeclarationType( m_attrType);
        }
        
        protected final boolean normalizeName() throws AttributeConflictException {
            String ident = PackagerUtils.str2ident( m_varName);
            if ( ident.equals(m_varName)) {
                return false;
            } else {
                m_varName = ident;
                for ( AttributeInstance attr : m_attrs.values()) {
                    if ( attr != this) {
                        if ( attr.m_varName.equals( m_varName)) {
                            throw new AttributeConflictException( "The attribute " + ident + " is duplicated!");
                        }
                    }
                }
                return true;
            }
        }
        
        public abstract boolean isArray();

        public abstract void processNames() throws AttributeConflictException;
        
        public abstract void writeDeclaration( IndentedWriter writer, boolean isGlobal) throws IOException;

        public final  void writeInitialisation( IndentedWriter writer, boolean isGlobal) throws IOException {
            if ( isGlobal || isArray()) {
                StringBuilder sb = new StringBuilder();
                sb.append( getVarName());
                sb.append( "=");
                appendInitialisation(sb, isGlobal);
                sb.append(';');

                writer.write(sb.toString());
            }
        }
                
        protected void appendAccessorCall(StringBuilder sb, String name) {
            String  typeName = getDeclarationType(getType());

            String returnedType;
            if ( GROUP_TYPE.equals( typeName)) {
                sb.append("getGroup");
                returnedType = GROUP_TYPE;
            } else {
                sb.append( "getNode");
                returnedType = NODE_TYPE;
            }

            sb.append("(\"");
            sb.append(name);
            sb.append("\")");

            if ( !returnedType.equals( typeName)) {
                sb.append(" as ");
                sb.append(typeName);
            }
        }                

        protected abstract void appendInitialisation( StringBuilder sb, boolean isGlobal);        
    }
    
    private final class SingleAttribute extends AttributeInstance {
        
        public SingleAttribute( String name, String type) {
            super( name, type);
        }

        public boolean isArray() {
            return false;
        }

        public void processNames() throws AttributeConflictException {
            normalizeName();
        }
        
        public void writeDeclaration( IndentedWriter writer, boolean isGlobal) throws IOException { 
            if ( isGlobal) {
                writer.write( String.format( "public-read protected var %s: %s;", getVarName(),
                    getDeclarationType(getType())));                    
            }
        }

        protected void appendInitialisation( StringBuilder sb, boolean isGlobal) {
            appendAccessorCall( sb, getName());
        }
    }

    private static final class ArrayAttributeIndex implements Comparable {
        public       int    index;
        public final String name;
        public final String type;

        public ArrayAttributeIndex( int index, String name, String type) {
            this.index = index;
            this.name  = name;
            this.type  = type;
        }

        public int compareTo(Object o) {
            ArrayAttributeIndex aai = (ArrayAttributeIndex) o;
            if ( aai.index == index) {
                return 0;
            } else {
                return index < aai.index ? -1 : 1;
            }
        }
    }
    
    private final class ArrayAttribute extends AttributeInstance {
        private final List<ArrayAttributeIndex> m_indices = new ArrayList<ArrayAttributeIndex>();
        
        public ArrayAttribute( String attrName, String type) {
            super( attrName, type);
        }
        
        public void addIndex(final int index, String name, String type) throws AttributeConflictException {
            m_indices.add( new ArrayAttributeIndex(index, name, type));
//            Integer obj = Integer.valueOf(index);
//            if ( m_indices.contains(obj)) {
//                throw new AttributeConflictException( String.format(  "The attrinute %s[%d] is duplicated!", getName(), index));
//            }
//            m_indices.add(obj);
        }
        
        public boolean isArray() {
            return true;
        }

        public void processNames() throws AttributeConflictException {
            normalizeName();
            handleIndexes();
            
//            int previous = -1;
//            for ( ArrayAttributeIndex aai : m_indices) {
//                assert aai.index > previous : "Array indices must be greater than zero and sorted - " + getName();
//                aai.shape.setName(String.format("%s_%d_", getName(), aai.index));
//                previous = aai.index;
//            }
        }
        
        protected void handleIndexes() {
            Collections.sort(m_indices);
            int length = m_indices.size();
            
            while(m_indices.get(0).index == -1) {
                int freeIndex = 0;
                for (int i = 1; i < length; i++) {
                    int index = m_indices.get(i).index;
                    
                    if ( index >= 0) {
                        if ( index != freeIndex) {
                            break;
                        }
                        freeIndex++;
                    }                    
                }
                m_indices.get(0).index = freeIndex;
                Collections.sort(m_indices);                
            }
        }
        
        public void writeDeclaration( IndentedWriter writer, boolean isGlobal) throws IOException { 
            String type = getDeclarationType(getType());
            if ( isGlobal) {
                writer.write( String.format( "public-read protected var %s: %s[];", getVarName(), type));
            } else {
                for ( ArrayAttributeIndex aai : m_indices) {
                    writer.write( String.format( "var %s: %s;", aai.name, type));
                }                
            }                
        }

        protected void appendInitialisation( StringBuilder sb, boolean isGlobal) {
            sb.append( "[");
            //TODO report missing indices
            for ( ArrayAttributeIndex aai : m_indices) {
                if ( isGlobal) {
                    appendAccessorCall(sb, aai.name);
                } else {
                    sb.append( aai.name);
                }
                sb.append(',');
            }
            //remove the trailing comma separator
            int l = sb.length() - 1;
            if ( l > 0) {
                sb.setLength(l);
            } 
            sb.append(']');
        }
    }
    
    public AttributeHolder() {        
    }
        
    public void setStrictTypes( final boolean strictTypes) {
        m_strictTypes = strictTypes;
    }
        
    public void add( String name, String type) throws AttributeConflictException {
        String shapeName = name.trim();
        Object [] parts = splitName(shapeName);
        
        if ( parts != null) {
            String            attrName = (String) parts[0];
            AttributeInstance attr     = m_attrs.get(attrName);

            if ( attr == null) {
                attr = new ArrayAttribute( attrName, type);
                m_attrs.put(attrName, attr);
            } else {
                if ( !attr.isArray()) {
                    //TODO
                    throw new AttributeConflictException( "Element ID conflict, the ID '" + attrName + "' is used both for array and single member!");
                }
                String attrType  = getDeclarationType(attr.getType());
                String shapeType = getDeclarationType(attr.getType());
                
                if ( !attrType.equals(shapeType)) {
                    //TODO
                    throw new AttributeConflictException( "Element ID conflict, the ID '" + attrName + "' is both " + attrType + " and " + shapeType + " types!");
                }
            }
            ((ArrayAttribute)attr).addIndex(((Integer) parts[1]).intValue(),name, type);
        } else {
            AttributeInstance attr = m_attrs.get(shapeName);

            if ( attr != null) {
                //TODO
                throw new AttributeConflictException( "Element ID conflict, the ID '" + shapeName + "' is already used!");
            }
            m_attrs.put(shapeName, new SingleAttribute(name, type));
        }
    }
        
    public void processNames() throws AttributeConflictException {
        // use temporary array for keys to prevent the ConcurrentModificationException
        for ( String key : m_attrs.keySet().toArray( new String[ m_attrs.size()])) {
            m_attrs.get(key).processNames();
        }           
    }
    
    public void serializeDeclarations( IndentedWriter writer, boolean isGlobal) throws IOException {
        if ( !isGlobal && hasArrays()) {
            writer.write("//declare array variables");
        }
        
        for ( AttributeInstance attr : m_attrs.values()) {
            attr.writeDeclaration(writer, isGlobal);
        }           
    }

    public void serializeUpdates( IndentedWriter writer, boolean isGlobal) throws IOException {
        if ( !isGlobal && hasArrays()) {
            writer.write("//fill array variables");
        }

        for ( AttributeInstance attr : m_attrs.values()) {
            attr.writeInitialisation(writer, isGlobal);
        }           
    }
    
    protected boolean hasArrays() {
        for ( AttributeInstance attr : m_attrs.values()) {
            if (attr.isArray()) {
                return true;
            }
        }           
        return false;
    }    
    
 //   private static final String LEFT_BRACKET  = "_x5B_";
 //   private static final String RIGHT_BRACKET = "_x5D_";
            
    private static Object [] splitName( String name) throws AttributeConflictException {
        /*
        int len = name.length();
        int p2 = 0;
        
        if ( name.endsWith("]")) {
            p2 = len - 1;
  //      } else if ( name.endsWith(RIGHT_BRACKET)) {
  //          p2 = len - RIGHT_BRACKET.length();
        } else {
            return null;
        }
        
        int p0, p1;
        p0 = p1 = 0;
        
        if ( (p0=name.lastIndexOf('[', p2)) != -1) {
            p1 = p0 + 1;
 //       } else if ( (p0=name.lastIndexOf(LEFT_BRACKET, p2)) != -1) {
 //           p1 = p0 + LEFT_BRACKET.length();
        }
        
        if ( p1 > 1 && p2 >= p1) {
            String indexStr = name.substring(p1,p2).trim();
            try {
                int index;
                
                if ( indexStr.length() > 0) {
                    index = Integer.parseInt(indexStr);
                    if ( index < 0) {
                        throw new AttributeConflictException( "Negative array index is not allowed: '" + name + "'!");
                    }               
                } else {
                    index = -1;
                }
                return new Object[] { name.substring(0, p0), Integer.valueOf(index)};
            } catch( NumberFormatException e) {
                throw new AttributeConflictException( "Illegal array specifier: '" + name + "'!");
            }
        }
         */
        return null;
    }    
}
