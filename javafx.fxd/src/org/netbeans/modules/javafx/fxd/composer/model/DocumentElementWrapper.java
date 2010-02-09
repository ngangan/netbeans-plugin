/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.model;

import com.sun.javafx.tools.fxd.container.scene.fxd.FXDException;
import com.sun.javafx.tools.fxd.container.scene.fxd.FXDSyntaxErrorException;
import java.io.IOException;
import java.util.Enumeration;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import org.netbeans.modules.editor.structure.api.DocumentElement;
import com.sun.javafx.tools.fxd.*;

import com.sun.javafx.tools.fxd.container.scene.fxd.ContentHandler;
import com.sun.javafx.tools.fxd.container.scene.fxd.FXDParser;
import java.io.Reader;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.netbeans.modules.javafx.fxd.schemamodel.FXDSchemaHelper;
import org.openide.util.Exceptions;

/**
 *
 * @author Pavel Benes
 */
final class DocumentElementWrapper {
    private DocumentElementWrapper() {}

    private static abstract class FXDElementWrapper implements com.sun.javafx.tools.fxd.FXDElement {
        protected DocumentElement m_de;
        protected List<DocumentElement> m_children;
        private static FXDParser m_parser = createParser();

        public FXDElementWrapper( final DocumentElement de) {
            m_de = de;
            m_children = m_de.getChildren();

            /* it is not necessary. remove in case will not find cases when it is useful
            m_de.addDocumentElementListener(new DocumentElementListener() {

                public void elementAdded(DocumentElementEvent dee) {
                    //System.out.println(m_de.getName()+ " elementAdded "+dee.getChangedChild());
                    m_children = m_de.getChildren();
                }

                public void elementRemoved(DocumentElementEvent dee) {
                    //System.out.println(m_de.getName()+ " elementRemoved "+dee.getChangedChild());
                    m_children = m_de.getChildren();
                }

                public void childrenReordered(DocumentElementEvent dee) {
                    //System.out.println(m_de.getName()+ " childrenReordered "+dee.getChangedChild());
                }

                public void contentChanged(DocumentElementEvent dee) {
                    //System.out.println(m_de.getName()+ " contentChanged "+dee.getChangedChild());
                }

                public void attributesChanged(DocumentElementEvent dee) {
                    //System.out.println(m_de.getName()+ " attributesChanged "+dee.getChangedChild());
                }
            });
             */
        }
        
        public void release() {
            m_de = null;
            m_children = null;
        }

        private static FXDParser createParser() {
            try {
                return new FXDParser(new FakeReader(), new FakeContentHandler());
            } catch (IOException ex) {
                Exceptions.printStackTrace(ex);
            }
            return null;
        }

        Object parseValue(String strValue) {
            try {
                // TODO: providing FXDParser will allow to support references
                return FXDParser.parseValue(strValue, m_parser);
            } catch (FXDSyntaxErrorException ex) {
                Logger.getLogger(this.getClass().getName()).
                        log(Level.WARNING, "Exception while parsing \"" + strValue + "\" value", ex);
            } catch (FXDException ex) {
                Logger.getLogger(this.getClass().getName()).
                        log(Level.WARNING, "Exception while parsing \"" + strValue + "\" value", ex);
            }
            return strValue;
        }
    }

    private static final Enumeration ID_ELEM = new Enumeration() {
        public boolean hasMoreElements() {
            return true;
        }

        public Object nextElement() {
            return FXDObjectElement.ATTR_NAME_ID;
        }
    };

    private static class FXDNodeWrapper extends FXDElementWrapper implements FXDObjectElement, Enumeration {
        private final boolean     m_injectID;
        private       Enumeration m_attrEnum = null;
        protected     int         m_refID = -1;

        public FXDNodeWrapper(final DocumentElement de) {
            super(de);
            m_injectID = m_de.getAttributes().isDefined(FXDObjectElement.ATTR_NAME_ID) == false &&
                         isIDSupported(getTypeName());
        }        

        public String getTypeName() {
            return m_de.getName();
        }

        public int getAttrNum() {
            int attrNum = m_de.getAttributes().getAttributeCount();
            if ( !m_injectID) {
                attrNum++;
            }
            return attrNum;
        }

        public Object getAttrValue(String name) {
          if ( FXDObjectElement.ATTR_NAME_ID.equals(name)) {
                //override existing ID with the element start offset
                return FXDFileModel.getElementId(m_de);
            } else {
                String strValue = (String) m_de.getAttributes().getAttribute(name);
                if ( strValue != null) {
                    return parseValue(strValue);
                } else {
                    if ( m_de.getAttributes().isDefined(name)) {
                        for ( DocumentElement cde : m_children) {
                            if ( name.equals(cde.getName())) {
                                if ( FXDFileModel.FXD_ATTRIBUTE.equals( cde.getType())) {
                                    assert cde.getElementCount() == 1;
                                    return wrap(cde.getChildren().get(0), false);
                                } else {
                                    assert FXDFileModel.FXD_ATTRIBUTE_ARRAY.equals( cde.getType());
                                    return wrap(cde, false);
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
        public Enumeration getAttrNames() {
           Enumeration attrNames = m_de.getAttributes().getAttributeNames();
            assert attrNames != null;

            if ( m_injectID) {
                m_attrEnum = ID_ELEM;
                return this;
            } else {
                return attrNames;
            }
        }

        public int getKind() {
            return com.sun.javafx.tools.fxd.FXDElement.KIND_OBJECT;
        }

        public boolean isLeaf() {            
            return m_de.getElementCount() == 0;
        }

        public Enumeration children() {
            final List<DocumentElement> childDE = new ArrayList<DocumentElement>();
            collectChildren(m_de, childDE);
            return new Enumeration() {
                private int m_index = 0;

                public boolean hasMoreElements() {
                    return m_index < childDE.size();
                }

                public Object nextElement() {
                    if ( m_index >= childDE.size()) {
                        throw new NoSuchElementException();
                    }
                    return wrap( childDE.get(m_index++), false);
                }
            };
        }

        @Override
        public void release() {
            super.release();
            m_attrEnum = null;
        }

        public boolean hasMoreElements() {
            assert m_attrEnum != null;
            return m_attrEnum.hasMoreElements();
        }

        public Object nextElement() {
           assert m_attrEnum != null;
            Object o = m_attrEnum.nextElement();
            if ( m_attrEnum == ID_ELEM) {
                m_attrEnum = m_de.getAttributes().getAttributeNames();
            }
            return o;
        }

        public synchronized int getReferenceID() {
            return m_refID;
        }


        public synchronized void setReferenceID(int refID) {
            m_refID = refID;
        }

        // TODO do we need and can support meta? or 
        public int getMetaNum() {
            return 0;
        }

        public Enumeration getMetaKeys() {
            return new Enumeration() {

                public boolean hasMoreElements() {
                    return false;
                }

                public Object nextElement() {
                    return null;
                }
            };
        }

        public Object getMetaValue(String string) {
            return null;
        }
    }

    private static void collectChildren( final DocumentElement de, List<DocumentElement> childrenList) {
        List<DocumentElement> children = de.getChildren();
        for (DocumentElement child : children){
            String type = child.getType();
            if ( FXDFileModel.FXD_NODE.equals( type) || 
                 FXDFileModel.FXD_ATTRIBUTE_ARRAY.equals(type)) {
                childrenList.add( child);
            } else {
                collectChildren(child, childrenList);
            }
        }
    }
    
    private static final class FXDRootNodeWrapper extends FXDNodeWrapper implements FXDRootElement {
        public FXDRootNodeWrapper(final DocumentElement de) {
            super(de);
        }
        
        @Override
        public Object getAttrValue(String name) {  //NOI18N
            if ("cache".equals(name)) { // NOI18N
                return Boolean.TRUE;
            } else {
                return super.getAttrValue(name);
            }
        }

        public int getAllChildrenNum() {
            return -1;
        }

    }
    
    private static final class FXDNodeArrayWrapper extends FXDElementWrapper implements FXDArrayElement {

        public FXDNodeArrayWrapper(final DocumentElement de) {
            super(de);
        }        

        public int getLength() {
            return m_de.getElementCount();
        }

        public Object elementAt(int index) {
            DocumentElement de = m_children.get(index);

            if ( FXDFileModel.FXD_ARRAY_ELEM.equals(de.getType())){
                return parseValue(de.getName());
            } else {
                return wrap( de, false);
            }
        }

        public int getKind() {
            return com.sun.javafx.tools.fxd.FXDElement.KIND_ARRAY;
        }

        public boolean isLeaf() {
            return m_de.getElementCount() == 0;
        }

        public Enumeration children() {
            return new Enumeration() {
                private int m_index = advance(0);

                public boolean hasMoreElements() {
                    return m_index < m_children.size();
                }

                public Object nextElement() {
                    if ( m_index >= m_children.size()) {
                        throw new NoSuchElementException();
                    }
                    com.sun.javafx.tools.fxd.FXDElement elem = wrap( m_children.get(m_index), false);
                    m_index = advance(m_index+1);
                    return elem;
                }
                
                private int advance(int index) {
                    while( index < m_children.size() &&
                       FXDFileModel.FXD_ARRAY_ELEM.equals(m_children.get(index).getType())) {
                       index++;
                    }
                    return index;
                }
            };
        }        
    }

    public static com.sun.javafx.tools.fxd.FXDElement wrap( final DocumentElement de, boolean isRoot) {
        String type = de.getType();
        if ( FXDFileModel.FXD_NODE.equals( type)) {
            return isRoot ? new FXDRootNodeWrapper(de) : new FXDNodeWrapper(de);
        } else if ( FXDFileModel.FXD_ATTRIBUTE_ARRAY.equals(type)) {
            return new FXDNodeArrayWrapper(de);
        } else if ( FXDFileModel.FXD_ERROR.equals(type)) {
            // nothing to do
            return null;
        } else {    
            throw new RuntimeException( "Unknown DocumentElement type: " + type); //NOI18N
        }
    }

    private static Map<String,Boolean> CLASSES_WITH_ID = new HashMap<String, Boolean>();

    public static boolean isIDSupported( String typeName) {
        Boolean state = CLASSES_WITH_ID.get(typeName);
        if ( state == null) {
            state = Boolean.valueOf( isIDSupportedImpl(typeName));
            CLASSES_WITH_ID.put(typeName, state);
        }
        return state.booleanValue();
    }

    private static boolean isIDSupportedImpl( String typeName) {
        for ( String importStr : FXDSchemaHelper.getDefaultImports()) {
            String className = importStr.concat(typeName);
            try {
                Class clazz = Class.forName(className);
                try {
                    Method m = clazz.getMethod("get$id");
                    assert m != null;
                    return true;
                } catch( Exception e) {
                    return false;
                }
            } catch (ClassNotFoundException ex) {
            }
        }
        return false;
    }

    private static class FakeReader extends Reader {

        @Override
        public int read(char[] cbuf, int off, int len) throws IOException {
            return 0;
        }

        @Override
        public void close() throws IOException {
        }
    }

    private static class FakeContentHandler implements ContentHandler {

        public void parsingStarted(FXDParser fxdp) {
        }

        public Object startNode(String string, int i, boolean bln) throws FXDException {
            return null;
        }

        public Object startNodeArray(int i) throws FXDException {
            return null;
        }

        public FXDReference createReference(String string) throws FXDException {
            // TODO support references?
            return null;
        }

        public void attribute(Object o, String string, String string1, int i, int i1, boolean bln) throws FXDException {
        }

        public void endNode(Object o, int i) throws FXDException {
        }

        public void arrayElement(Object o, String string, int i, int i1) throws FXDException {
        }

        public void endNodeArray(Object o, int i) throws FXDException {
        }
    }
}
