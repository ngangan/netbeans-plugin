/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.model;

import com.sun.javafx.tools.fxd.container.scene.fxd.FXDParser;
import java.util.Enumeration;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import com.sun.javafx.tools.fxd.FXDElement;
import com.sun.javafx.tools.fxd.FXDNode;
import com.sun.javafx.tools.fxd.FXDNodeArray;
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;

/**
 *
 * @author Pavel Benes
 */
final class DocumentElementWrapper {
    private DocumentElementWrapper() {}
    
    private static abstract class FXDElementWrapper implements FXDElement {
        protected DocumentElement m_de;
        protected final boolean m_remapID;
        
        public FXDElementWrapper( final DocumentElement de, boolean remapID) {
            m_de = de;
            m_remapID = remapID;
        }
        
        public void release() {
            m_de = null;
        }
    }
    
    private static class FXDNodeWrapper extends FXDElementWrapper implements FXDNode {        
        public FXDNodeWrapper(final DocumentElement de, boolean remapID) {
            super(de, remapID);
        }        

        public String getTypeName() {
            return m_de.getName();
        }

        public int getAttrNum() {
            return m_de.getAttributes().getAttributeCount();
        }

        public Object getAttrValue(String name) {
            if ( m_remapID && FXDNode.ATTR_NAME_ID.equals(name)) {
                //override existing ID with the element start offset
                return Integer.toString(m_de.getStartOffset());
            } else {
                String strValue = (String) m_de.getAttributes().getAttribute(name);
                if ( strValue != null) {
                    return FXDParser.parseValue(strValue);
                } else {
                    if ( m_de.getAttributes().isDefined(name)) {
                        int elemCount = m_de.getElementCount();
                        for ( int i = 0; i < elemCount; i++) {
                            DocumentElement de = m_de.getElement(i);
                            if ( name.equals(de.getName())) {
                                if ( FXDFileModel.FXD_ATTRIBUTE.equals( de.getType())) {
                                    assert de.getElementCount() == 1;
                                    return wrap(de.getElement(0), false, m_remapID);
                                } else {
                                    assert FXDFileModel.FXD_ATTRIBUTE_ARRAY.equals( de.getType());
                                    return wrap(de, false, m_remapID);
                                }
                            }
                        }
                        System.err.println("Attribute " + name + " not found on the element " + m_de.getName() + "!"); //NOI18N
                    }
                    return null;
                }
            }
        }

        @SuppressWarnings("unchecked")         
        public Enumeration<String> getAttrNames() {
            return (Enumeration<String>) m_de.getAttributes().getAttributeNames();
        }

        public int getKind() {
            return FXDElement.KIND_OBJECT;
        }

        public boolean isLeaf() {            
            return m_de.getElementCount() == 0;
        }

        public Enumeration<FXDElement> children() {
            final List<DocumentElement> childDE = new ArrayList<DocumentElement>();
            collectChildren(m_de, childDE);
            return new Enumeration<FXDElement>() {
                private int m_index = 0;

                public boolean hasMoreElements() {
                    return m_index < childDE.size();
                }

                public FXDElement nextElement() {
                    if ( m_index >= childDE.size()) {
                        throw new NoSuchElementException();
                    }
                    return wrap( childDE.get(m_index++), false, m_remapID);
                }
            };
        }        
    }

    private static void collectChildren( final DocumentElement de, List<DocumentElement> childrenList) {
        for ( int i = 0; i < de.getElementCount(); i++) {
            DocumentElement child = de.getElement(i);
            String type = child.getType();
            if ( FXDFileModel.FXD_NODE.equals( type) || 
                 FXDFileModel.FXD_ATTRIBUTE_ARRAY.equals(type)) {
                childrenList.add( child);
            } else {
                collectChildren(child, childrenList);
            }
        }
    }
    
    private static final class FXDRootNodeWrapper extends FXDNodeWrapper {
        public FXDRootNodeWrapper(final DocumentElement de, boolean remapID) {
            super(de, remapID);
        }
        
        @Override
        public Object getAttrValue(String name) {  //NOI18N
            if ("cache".equals(name)) { // NOI18N
                return Boolean.TRUE;
            } else {
                return super.getAttrValue(name);
            }
        }        
    }
    
    private static final class FXDNodeArrayWrapper extends FXDElementWrapper implements FXDNodeArray {

        public FXDNodeArrayWrapper(final DocumentElement de, boolean remapID) {
            super(de, remapID);
        }        

        public int getLength() {
            return m_de.getElementCount();
        }

        public Object elementAt(int index) {
            DocumentElement de = m_de.getElement(index);
        
            if ( FXDFileModel.FXD_ARRAY_ELEM.equals(de.getType())){
                return de.getName();
            } else {
                return wrap( de, false, m_remapID);
            }
        }

        public int getKind() {
            return FXDElement.KIND_ARRAY;
        }

        public boolean isLeaf() {
            return m_de.getElementCount() == 0;
        }

        public Enumeration<FXDElement> children() {
            return new Enumeration<FXDElement>() {
                private int m_index = advance(0);

                public boolean hasMoreElements() {
                    return m_index < m_de.getElementCount();
                }

                public FXDElement nextElement() {
                    if ( m_index >= m_de.getElementCount()) {
                        throw new NoSuchElementException();
                    }
                    FXDElement elem = wrap( m_de.getElement(m_index), false, m_remapID);
                    m_index = advance(m_index+1);
                    return elem;
                }
                
                private int advance(int index) {
                    while( index < m_de.getElementCount() &&
                       FXDFileModel.FXD_ARRAY_ELEM.equals(m_de.getElement(index).getType())) {
                       index++;
                    }
                    return index;
                }
            };
        }        
    }

    public static FXDElement wrap( final DocumentElement de, boolean isRoot, boolean remapId) {
        String type = de.getType();
        if ( FXDFileModel.FXD_NODE.equals( type)) {
            return isRoot ? new FXDRootNodeWrapper(de, remapId) : new FXDNodeWrapper(de, remapId);
        } else if ( FXDFileModel.FXD_ATTRIBUTE_ARRAY.equals(type)) {
            return new FXDNodeArrayWrapper(de, remapId);
        } else if ( FXDFileModel.FXD_ERROR.equals(type)) {
            // nothing to do
            return null;
        } else {    
            throw new RuntimeException( "Unknown DocumentElement type: " + type); //NOI18N
        }
    }
}
