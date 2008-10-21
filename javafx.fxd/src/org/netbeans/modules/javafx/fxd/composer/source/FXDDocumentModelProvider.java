/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.source;
       
import com.sun.javafx.tools.fxd.container.scene.fxd.ContentHandler;
import com.sun.javafx.tools.fxd.container.scene.fxd.FXDParser;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import org.netbeans.editor.BaseDocument;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.editor.structure.api.DocumentModel;
import org.netbeans.modules.editor.structure.api.DocumentModel.DocumentChange;
import org.netbeans.modules.editor.structure.api.DocumentModel.DocumentModelModificationTransaction;
import org.netbeans.modules.editor.structure.api.DocumentModel.DocumentModelTransactionCancelledException;
import org.netbeans.modules.editor.structure.api.DocumentModelException;
import org.netbeans.modules.editor.structure.spi.DocumentModelProvider;

import org.netbeans.modules.javafx.fxd.composer.model.FXDFileModel;
import static org.netbeans.modules.javafx.fxd.composer.source.TextParser.Direction.BACKWARD;
import org.openide.util.Exceptions;

/**
 *
 * @author Pavel Benes
 */
public final class FXDDocumentModelProvider implements DocumentModelProvider {
        
    private static final Map<String,String> NO_ATTRS = new HashMap<String,String>(1);
    
    private static final class NodeBuilder {
        private final String             m_typeName;
        private final int                m_startOffset;
        private final Map<String,String> m_attributes = new HashMap<String, String>();
        
        private NodeBuilder( String typeName, int startOff) {
            m_typeName = typeName;
            m_startOffset = startOff;
        }
        
        public Map<String,String> getAttributeMap() {
            return m_attributes;
        }
        
        protected void addAttribute( String name, String value, int startOff, int endOff) {
            m_attributes.put(name, value);
        }
        
        protected void build(DocumentModelModificationTransaction trans, int endOffset) throws BadLocationException, DocumentModelTransactionCancelledException {
            trans.addDocumentElement( m_typeName, FXDFileModel.FXD_NODE, m_attributes, m_startOffset, endOffset);
        }

        public boolean isEqual(final AttributeSet attrs) {
            assert attrs != null;
            if ( attrs.getAttributeCount() == m_attributes.size()) {
                for (Entry<String, String> entry : m_attributes.entrySet()) {
                    String value1 = entry.getValue();
                    Object value2 = attrs.getAttribute(entry.getKey());
                    
                    if ( value1 != value2) {
                        if ( value1 == null || !value1.equals(value2)) {
                            return false;
                        }
                    }
                }
                return true;
            }
            return false;
        }
    }
    
    public void updateModel(final DocumentModelModificationTransaction trans, final DocumentModel model, final DocumentChange[] changes) throws DocumentModelException, DocumentModelTransactionCancelledException {
        BaseDocument doc = (BaseDocument) model.getDocument();
        
        TextParser textParser = new TextParser( doc.getText());
        
        System.err.println("Model update started ...");

        if ( textParser.gotoChar('{')) {
            textParser.fetchChar(BACKWARD);
            textParser.skipWhite(BACKWARD);
            textParser.skipNonWhite(BACKWARD);

            int nodesStartPos = textParser.getPosition() + 1;
            CharSequence nodes = textParser.subsequence(nodesStartPos, textParser.getSize());
            FXDParser fxdParser = new FXDParser(nodes, new ContentHandler() {
                private boolean m_isLastNode = true;
                
                public Object startNode(String typeName, int startOff) {
                    return new NodeBuilder(typeName, startOff);
                }

                public void attribute(Object node, String name, String value, int startOff, int endOff) throws Exception {
                    NodeBuilder deb = (NodeBuilder) node; 
                    if ( value == null) {
                        if ( m_isLastNode) {
                            //System.err.println(String.format("Adding attribute %s <%d, %d>", name, startOff, endOff));
                            trans.addDocumentElement(name, FXDFileModel.FXD_ATTRIBUTE, NO_ATTRS, startOff, endOff);
                        } else {
                            //System.err.println(String.format("Adding array attribute %s <%d, %d>", name, startOff, endOff));
                            trans.addDocumentElement(name, FXDFileModel.FXD_ATTRIBUTE_ARRAY, NO_ATTRS, startOff, endOff);
                        }
                    }
                    deb.addAttribute( name, value, startOff, endOff);
                }

                public void endNode(Object node, int endOff) throws Exception {
                    //System.err.println("Node ended");
                    NodeBuilder deb = (NodeBuilder) node;                    
                    DocumentElement de = model.getLeafElementForOffset(deb.m_startOffset);
                    if ( de == null || de == model.getRootElement()) {
                        deb.build(trans, endOff);
                    } else {                        
                        if ( !deb.isEqual(de.getAttributes())) {
                            System.err.println("Attributes changes for " + deb.m_typeName);
                            trans.updateDocumentElementAttribs(de, deb.getAttributeMap());
                        }
                    }
                    m_isLastNode = true;
                }

                public Object startNodeArray(int startOff) {
                    //System.err.println("Array started");
                    return null;
                }

                public void arrayElement(Object node, String value, int startOff, int endOff) {
                    if ( value != null) {
                        //TODO throw exception when FXDContainer library gets updated
                        try {
                            trans.addDocumentElement(value, FXDFileModel.FXD_ARRAY_ELEM, NO_ATTRS, startOff, endOff);
                        } catch (BadLocationException ex) {
                            Exceptions.printStackTrace(ex);
                        } catch (DocumentModelTransactionCancelledException ex) {
                            Exceptions.printStackTrace(ex);
                        }
                    }
                }

                public void endNodeArray(Object node, int endOff) {
                    //NodeArrayBuilder nab = (NodeArrayBuilder) node; 
                    m_isLastNode = false;
                }
                
            }, nodesStartPos);
            
            try {
                fxdParser.parseObject();
                reportDeletedElements(trans, model.getRootElement());
            System.err.println("Model update done ...");
            } catch( DocumentModelException e) {
                throw e;
            } catch( DocumentModelTransactionCancelledException e) {
                throw e;
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }
    
    protected void reportDeletedElements(final DocumentModelModificationTransaction trans, DocumentElement de) throws DocumentModelTransactionCancelledException {
        assert de != null;
        if ( de.getStartOffset() == de.getEndOffset()) {
            trans.removeDocumentElement(de, true);
        } else {
            for (int i = de.getElementCount()-1; i >= 0; i--) {
                reportDeletedElements(trans, (DocumentElement) de.getElement(i));
            }
        }
    }
}
