/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.model;

import com.sun.javafx.tools.fxd.container.scene.fxd.FXDParser;
import java.util.Enumeration;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.javafx.fxd.composer.source.FXDDocumentModelProvider;
import com.sun.javafx.tools.fxd.FXDElement;
import com.sun.javafx.tools.fxd.FXDNode;
import com.sun.javafx.tools.fxd.FXDNodeArray;

/**
 *
 * @author Pavel Benes
 */
final class DocumentElementWrapper {
    private DocumentElementWrapper() {}
    
    private static abstract class FXDElementWrapper implements FXDElement {
        protected DocumentElement m_de;
        
        public FXDElementWrapper( final DocumentElement de) {
            m_de = de;
        }
        
        public void release() {
            m_de = null;
        }
    }
    
    private static final class FXDNodeWrapper extends FXDElementWrapper implements FXDNode {
        public FXDNodeWrapper(final DocumentElement de) {
            super(de);
        }        
        
        public String getTypeName() {
            return m_de.getName();
        }

        public int getAttrNum() {
            return m_de.getAttributes().getAttributeCount();
        }

        public Object getAttrValue(String name) {
            if ( FXDNode.ATTR_NAME_ID.equals(name)) {
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
                                if ( FXDDocumentModelProvider.FXD_ATTRIBUTE.equals( de.getType())) {
                                    assert de.getElementCount() == 1;
                                    return wrap(de.getElement(0));
                                } else {
                                    assert FXDDocumentModelProvider.FXD_ATTRIBUTE_ARRAY.equals( de.getType());
                                    return wrap(de);
                                }
                            }
                        }
                        System.err.println("Attribute " + name + " not found on the element " + m_de.getName() + "!"); //NOI18N
                    }
                    return null;
                }
            }
        }

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
            //TODO Implement
            throw new UnsupportedOperationException("Not supported yet."); //NOI18N
        }
    }
    
    private static final class FXDNodeArrayWrapper extends FXDElementWrapper implements FXDNodeArray {

        public FXDNodeArrayWrapper(final DocumentElement de) {
            super(de);
        }        

        public int getLength() {
            return m_de.getElementCount();
        }

        public Object elementAt(int index) {
            return wrap( m_de.getElement(index));
        }

        public int getKind() {
            return FXDElement.KIND_ARRAY;
        }

        public boolean isLeaf() {
            return m_de.getElementCount() == 0;
        }

        public Enumeration<FXDElement> children() {
            throw new UnsupportedOperationException("Not supported yet."); //NOI18N
        }        
    }

    public static FXDElement wrap( final DocumentElement de) {
        if ( FXDDocumentModelProvider.FXD_NODE.equals( de.getType())) {
            return new FXDNodeWrapper(de);
        } else if ( FXDDocumentModelProvider.FXD_ATTRIBUTE_ARRAY.equals(de.getType())) {
            return new FXDNodeArrayWrapper(de);
        } else {
            throw new RuntimeException( "Unknown DocumentElement type: " + de.getType()); //NOI18N
        }
    }
}
