/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.source;
       
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import javax.swing.JEditorPane;
import javax.swing.SwingUtilities;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.EditorUI;
import org.netbeans.editor.StatusBar;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.editor.NbEditorUtilities;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.editor.structure.api.DocumentModel;
import org.netbeans.modules.editor.structure.api.DocumentModel.DocumentChange;
import org.netbeans.modules.editor.structure.api.DocumentModel.DocumentModelModificationTransaction;
import org.netbeans.modules.editor.structure.api.DocumentModel.DocumentModelTransactionCancelledException;
import org.netbeans.modules.editor.structure.api.DocumentModelException;
import org.netbeans.modules.editor.structure.spi.DocumentModelProvider;

import org.netbeans.modules.javafx.fxd.composer.misc.CharSeqReader;
import org.netbeans.modules.javafx.fxd.composer.model.FXDFileModel;
import org.openide.cookies.EditorCookie;
import org.openide.loaders.DataObject;
import org.openide.util.Exceptions;

import com.sun.javafx.tools.fxd.container.scene.fxd.*;

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
            if ( !m_attributes.containsKey(name)) {
                m_attributes.put(name, value);
            }
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
    
    public static final String PROP_PARSE_ERROR = "parse-error"; // NOI18N
    
    /*
    private static FXDNode s_root = null;    
    public static synchronized FXDNode getRoot() {
        return s_root;
    }*/
    
    public synchronized void updateModel(final DocumentModelModificationTransaction trans, final DocumentModel model, final DocumentChange[] changes) throws DocumentModelException, DocumentModelTransactionCancelledException {
        //DocumentModelUtils.dumpElementStructure( model.getRootElement());
        
        BaseDocument doc = (BaseDocument) model.getDocument();
        final DataObject dObj = NbEditorUtilities.getDataObject(doc);
        
        DocumentElement rootDE = model.getRootElement();
        if ( rootDE.getElementCount() == 1) {
            DocumentElement childDE = rootDE.getElement(0);
            if ( FXDFileModel.isError(childDE)) {
                trans.removeDocumentElement(rootDE.getElement(0), true);
            }            
        }

        Reader docReader = new CharSeqReader(doc.getText());
        final StringBuilder statMsg = new StringBuilder(" "); // NOI18N
                    
        try {
            FXDParser fxdParser = new FXDParser(docReader, new ContentHandler() {
                private boolean m_isLastNode = true;

                public Object startNode(String typeName, int startOff) {
                    return new NodeBuilder(typeName, startOff);
                }

                public void attribute(Object node, String name, String value, int startOff, int endOff) throws FXDException {
                    NodeBuilder deb = (NodeBuilder) node;
                    if ( value == null) {
                        try {
                            if ( m_isLastNode) {
                                //System.err.println(String.format("Adding attribute %s <%d, %d>", name, startOff, endOff));
                                trans.addDocumentElement(name, FXDFileModel.FXD_ATTRIBUTE, NO_ATTRS, startOff, endOff);
                            } else {
                                //System.err.println(String.format("Adding array attribute %s <%d, %d>", name, startOff, endOff));
                                trans.addDocumentElement(name, FXDFileModel.FXD_ATTRIBUTE_ARRAY, NO_ATTRS, startOff, endOff);
                            }
                       } catch( Exception e) {
                           throw new FXDException(e);
                       }
                    }
                    deb.addAttribute( name, value, startOff, endOff);
                }

                public void endNode(Object node, int endOff) throws FXDException {
                    //System.err.println("Node ended");
                    NodeBuilder deb = (NodeBuilder) node;
                    DocumentElement de = model.getLeafElementForOffset(deb.m_startOffset);
                    try {
                        if ( de != model.getRootElement() && FXDFileModel.FXD_NODE.equals(de.getType())) {
                            if ( !deb.isEqual(de.getAttributes())) {
                                //System.err.println("Attributes changes for " + deb.m_typeName);
                                trans.updateDocumentElementAttribs(de, deb.getAttributeMap());
                            }
                        } else {
                            deb.build(trans, endOff);
                        }
                    } catch( Exception e) {
                        throw new FXDException(e);
                    }
                    m_isLastNode = true;
                }

                public Object startNodeArray(int startOff) {
                    //System.err.println("Array started");
                    return null;
                }

                public void arrayElement(Object node, String value, int startOff, int endOff) throws FXDException {
                    if ( value != null) {
                        try {
                            trans.addDocumentElement(value, FXDFileModel.FXD_ARRAY_ELEM, NO_ATTRS, startOff, endOff);
                        } catch (Exception ex) {
                            throw new FXDException( ex);
                        }
                    }
                }

                public void endNodeArray(Object node, int endOff) {
                    //NodeArrayBuilder nab = (NodeArrayBuilder) node;
                    m_isLastNode = false;
                }
            });

            showStatusText(dObj, " Parsing text..."); // NOI18N

            fxdParser.parseObject();
            reportDeletedElements(trans, model.getRootElement());
            doc.putProperty( PROP_PARSE_ERROR, null);
        } catch( DocumentModelTransactionCancelledException e) {
            //s_root = null;
            throw e;
        } catch (Exception ex) {
            if ( ex instanceof FXDSyntaxErrorException) {
                statMsg.append( "Syntax error: "); //NOI18N
            } else {
                statMsg.append( "Unknown error: "); //NOI18N
            }
            String msg = ex.getLocalizedMessage();
            statMsg.append(msg);
            doc.putProperty( PROP_PARSE_ERROR, msg);
            cleanModel(trans, model);
            try {
                trans.addDocumentElement("Invalid FXD syntax", FXDFileModel.FXD_ERROR, NO_ATTRS, 0, doc.getLength()); // NOI18N
            } catch (BadLocationException ex1) {
                Exceptions.printStackTrace(ex1);
            }
        } finally {
            showStatusText(dObj, statMsg.toString());
        }
    }

    protected static void showStatusText( DataObject dObj, final String msg) {
        final EditorCookie ec = dObj.getCookie( EditorCookie.class);
        if ( ec != null) {
            SwingUtilities.invokeLater( new Runnable() {
                public void run() {
                    final JEditorPane [] panes = ec.getOpenedPanes();
                    if ( panes != null && panes.length > 0 && panes[0] != null) {
                        EditorUI eui = Utilities.getEditorUI(panes[0]);
                        StatusBar sb = eui == null ? null : eui.getStatusBar();
                        if (sb != null) {
                            sb.setText(SourceEditorWrapper.CELL_ERROR, msg);
                        }
                    }
                }
            });
        }
    }
    
    protected void cleanModel( final DocumentModelModificationTransaction trans, DocumentModel model) throws DocumentModelTransactionCancelledException {
        DocumentElement root = model.getRootElement();
        for ( int i = root.getElementCount() - 1; i >= 0; i--) {
            trans.removeDocumentElement( root.getElement(i), true);
        }
    }
    
    protected void reportDeletedElements(final DocumentModelModificationTransaction trans, DocumentElement de) throws DocumentModelTransactionCancelledException {
        assert de != null;
        if ( de.getStartOffset() == de.getEndOffset()) {
            trans.removeDocumentElement(de, true);
        } else {
            for (int i = de.getElementCount()-1; i >= 0; i--) {
                reportDeletedElements(trans, de.getElement(i));
            }
        }
    }
}
