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

package org.netbeans.modules.javafx.fxd.composer.source;
       
import com.sun.javafx.tools.fxd.FXDReference;
import com.sun.javafx.tools.fxd.container.scene.fxd.ContentHandler;
import com.sun.javafx.tools.fxd.container.scene.fxd.FXDException;
import com.sun.javafx.tools.fxd.container.scene.fxd.FXDParser;
import com.sun.javafx.tools.fxd.container.scene.fxd.FXDSyntaxErrorException;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.netbeans.modules.javafx.fxd.composer.editor.SyntaxErrorsHighlightingTask;

/**
 *
 * @author Pavel Benes
 */
public final class FXDDocumentModelProvider implements DocumentModelProvider {
        
    private static final Map<String,String> NO_ATTRS = new HashMap<String,String>(1);
    
    private static final class NodeBuilder {
        private final String             m_typeName;
        private final int                m_startOffset;
        /** map with node's attributes */
        private final Map<String,String> m_attributes = new HashMap<String, String>();
        /** map with child attributes that have Node or List with Nodes as value.
         * Is used to check if child nodes should be removed from the model. 
         * The only possible way because ContentHandler doesn't give relations between nodes.
         */
        //private final Map<String, Object> m_childNodes = new HashMap<String, Object>();
        private final Map<String, NodeData> m_childAttrNodes = new HashMap<String, NodeData>();
        private final Map<String, List<NodeData>> m_childAttrArrayNodes =
                new HashMap<String, List<NodeData>>();
        
        private NodeBuilder( String typeName, int startOff) {
            m_typeName = typeName;
            m_startOffset = startOff;
        }

        /**
         * creates new object with node's name and start offset.
         * Separate object is used not to cary extra data, like
         * Maps with links to attributes or children
         * @return
         */
        public NodeData exportNodeData() {
            return new NodeData() {

                public String getName() {
                    return m_typeName;
                }

                public int getStartOffset() {
                    return m_startOffset;
                }
            };
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

        /**
         * checks if DocumentElement type and name are equal to current node
         * @param de
         * @return
         */
        public boolean isEqual(final DocumentElement de) {
            assert de != null;
            return FXDFileModel.FXD_NODE.equals(de.getType())
                    && m_typeName.equals(de.getName());
        }

        /**
         * Checks if given AttributeSet describes the same list of attributes as
         * current node contain.
         * @param attrs
         * @return
         */
        public boolean areAttributesEqual(final AttributeSet attrs) {
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

        @Override
        public String toString() {
            return "NodeBuilder [name="+m_typeName+" startOff="+m_startOffset+"]";
        }

        /**
         * returns map with attributes that contain Node as value.
         * @return Map with pairs (name,value).
         */
        protected Map<String, NodeData> getChildAttrNodes(){
            return m_childAttrNodes;
        }

        /**
         * returns map with attributes that contain List of Nodes as value.
         * @return Map with pairs (name,value).
         */
        protected Map<String, List<NodeData>> getChildAttrArrayNodes(){
            return m_childAttrArrayNodes;
        }

        /**
         * adds Node or List with nodes to the map with child Nodes.
         * Do not add attributes that has primitive value or array of primitives as value.
         * @param name - name of the attribute that has given Node or
         * List of Nodes as value.
         * @param value Node or List of Nodes
         */
        protected void addNodeToChildrenMap(String name, NodeData value) {
            if (!m_childAttrNodes.containsKey(name)) {
                m_childAttrNodes.put(name, value);
            }
        }

        protected void addNodeToChildrenMap(String name, List<NodeData> value) {
            if (!m_childAttrArrayNodes.containsKey(name)) {
                m_childAttrArrayNodes.put(name, value);
            }
        }

        protected boolean hasNodeInAttrValue(String nodeName) {
            NodeData attrNode = getChildAttrNodes().get(nodeName);
            if ((attrNode == null)) {
                return false;
            }
            return true;
        }

        protected List<NodeData> getNodesNamesFromAttrValue(String nodeName) {
            List<NodeData> nodesList = getChildAttrArrayNodes().get(nodeName);
            if (nodesList != null) {
                return nodesList;
            } else {
                return new ArrayList<NodeData>();
            }
        }

    }

    private static interface NodeData {
        String getName();
        int getStartOffset();
    }

    /**
     * is used as key to store one selected error to be shown in e.g. status line
     */
    public static final String PROP_PARSE_ERROR_MSG = "parse-error-msg"; // NOI18N
    /**
     * Is used as key to store list of all appeared errors.
     */
    public static final String PROP_PARSE_ERROR_LIST = "parse-error-list"; // NOI18N
    
    /*
    private static FXDNode s_root = null;    
    public static synchronized FXDNode getRoot() {
        return s_root;
    }*/
    
    public synchronized void updateModel(final DocumentModelModificationTransaction trans, 
            final DocumentModel model, final DocumentChange[] changes)
            throws DocumentModelException, DocumentModelTransactionCancelledException {
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
                    
        FXDParser fxdParser = null;
        try {
            fxdParser = new FXDParser(docReader, new ContentHandler() {
                /** is last processed element was node or not (then array) */
                private boolean m_isLastNode = true;
                /** last processed element.
                 * possible values are: NodeData (if attribute value was Node)
                 * or List<NodeData> ( if attribute value was array of nodes) */
                private Object  m_lastElem = null;

                public Object startNode(String typeName, int startOff, boolean isExtension) {
                    return new NodeBuilder(typeName, startOff);
                }

                public void attribute(Object node, String name, String value,
                        int startOff, int endOff, boolean isMeta) throws FXDException {
                    NodeBuilder deb = (NodeBuilder) node;
                    if ( value == null) {
                        try {
                            if ( m_isLastNode) {
                                //System.err.println(String.format("Adding attribute %s <%d, %d>", name, startOff, endOff));
                                trans.addDocumentElement(name, FXDFileModel.FXD_ATTRIBUTE, NO_ATTRS, startOff, endOff);
                                deb.addNodeToChildrenMap(name, (NodeData)m_lastElem);
                            } else {
                                //System.err.println(String.format("Adding array attribute %s <%d, %d>", name, startOff, endOff));
                                trans.addDocumentElement(name, FXDFileModel.FXD_ATTRIBUTE_ARRAY, NO_ATTRS, startOff, endOff);
                                deb.addNodeToChildrenMap(name, (List<NodeData>)m_lastElem);
                            }
                            m_lastElem = null;
                       } catch( Exception e) {
                           throw new FXDException(e);
                       }
                    }
                    // TODO parse value. This will allow to handle functions and references
                    // TODO handle meta separately
                    deb.addAttribute( name, value, startOff, endOff);
                }

                public void endNode(Object node, int endOff) throws FXDException {
                    //System.err.println("Node ended");
                    NodeBuilder deb = (NodeBuilder) node;
                    DocumentElement de = model.getLeafElementForOffset(deb.m_startOffset);
                    try {
                        if ( de != model.getRootElement() && deb.isEqual(de)) {
                            if ( !deb.areAttributesEqual(de.getAttributes())) {
                                //System.err.println("Attributes changes for " + deb.m_typeName);
                                trans.updateDocumentElementAttribs(de, deb.getAttributeMap());
                            }
                            synchRemovedChildNodes(deb, de);
                        } else {
                            deb.build(trans, endOff);
                        }
                    } catch( Exception e) {
                        throw new FXDException(e);
                    }
                    m_isLastNode = true;
                    // here
                    m_lastElem = deb.exportNodeData();
                }

                private void synchRemovedChildNodes(NodeBuilder deb, DocumentElement de)
                        throws DocumentModelTransactionCancelledException {
                    List<DocumentElement> deChildren = de.getChildren();
                    for (DocumentElement deChild : deChildren) {
                        if (FXDFileModel.FXD_ATTRIBUTE_ARRAY.equals(deChild.getType())) {
                            synchArrayOfChildNodes(deb, deChild);
                        } else if (FXDFileModel.FXD_ATTRIBUTE.equals(deChild.getType())) {
                            if (!deb.hasNodeInAttrValue(deChild.getName())) {
                                trans.removeDocumentElement(deChild, true);
                            }
                        }
                    }
                }

                private void synchArrayOfChildNodes(NodeBuilder deb, DocumentElement de)
                        throws DocumentModelTransactionCancelledException {
                    List<NodeData> nodeList = deb.getNodesNamesFromAttrValue(de.getName());
                    for (DocumentElement child : de.getChildren()) {
                        if (isNodeOrArrayElem(child)) {
                            if (!isDEInList(nodeList, child)) {
                                trans.removeDocumentElement(child, true);
                            }
                        }
                    }
                }

                private boolean isNodeOrArrayElem(DocumentElement elem) {
                    return FXDFileModel.FXD_NODE.equals(elem.getType())
                            || FXDFileModel.FXD_ARRAY_ELEM.equals(elem.getType());
                }

                private boolean isDEInList(List<NodeData> list, DocumentElement de){
                    for (NodeData nodeData : list){
                        if (nodeData.getName().equals(de.getName())
                                && nodeData.getStartOffset() == de.getStartOffset()){
                            return true;
                        }
                    }
                    return false;
                }

                public Object startNodeArray(int startOff) {
                    //System.err.println("Array started");
                    return new ArrayList<String>();
                }

                public void arrayElement(Object array, String value, int startOff, int endOff) throws FXDException {
                    if ( value != null) {
                        try {
                            trans.addDocumentElement(value, FXDFileModel.FXD_ARRAY_ELEM, NO_ATTRS, startOff, endOff);
                        } catch (Exception ex) {
                            throw new FXDException( ex);
                        }
                    } else {
                        ((List<NodeData>)array).add((NodeData)m_lastElem);
                        m_lastElem = null;
                    }
                }

                public void endNodeArray(Object array, int endOff) {
                    m_lastElem = array;
                    m_isLastNode = false;
                }

                public void parsingStarted(FXDParser parser) {
                    // do nothing. we do not need parser onstance
                }

                public FXDReference createReference(String str) throws FXDException {
                    // TODO: should implement?
                    // implementation is necessary if we parse attribute or array element value.
                    // But we use them as they come.
                    return null;
                }

                public boolean stopOnError() {
                    return false;
                }

            });

            showStatusText(dObj, " Parsing text..."); // NOI18N

            fxdParser.parseObject();
            reportDeletedElements(trans, model.getRootElement());
            doc.putProperty( PROP_PARSE_ERROR_MSG, null);
            cleanParserErrors(doc);
        } catch( DocumentModelTransactionCancelledException e) {
            //s_root = null;
            throw e;
        } catch (Exception ex) {
            processErrorMessage(doc, ex, statMsg);
            /* TODO: is it correct not to clean model on error?
            cleanModel(trans, model);
            try {
                trans.addDocumentElement("Invalid FXD syntax", FXDFileModel.FXD_ERROR, NO_ATTRS, 0, doc.getLength()); // NOI18N
            } catch (BadLocationException ex1) {
                Exceptions.printStackTrace(ex1);
            }
             */
        } finally {
            if (fxdParser != null && !fxdParser.getErrors().isEmpty()){
                processErrorMessage(doc, fxdParser.getErrors().get(0), statMsg);
                addParserErrors(doc, fxdParser.getErrors());
            }
            showStatusText(dObj, statMsg.toString());
        }
    }


    public static void cleanParserErrors(BaseDocument doc){
        doc.putProperty(PROP_PARSE_ERROR_LIST, null);
    }
    
    public static void addParserErrors(BaseDocument doc, List<FXDSyntaxErrorException> errors) {
        List<FXDSyntaxErrorException> old = (List<FXDSyntaxErrorException>)doc.getProperty(PROP_PARSE_ERROR_LIST);
        List<FXDSyntaxErrorException> merged = null;
        if (errors != null){
            if (old != null){
                // todo: merge
                merged = errors;
            } else {
                merged = errors;
            }
        }
        doc.putProperty(PROP_PARSE_ERROR_LIST, merged);
    }

    public static void addParserErrors(BaseDocument doc, FXDSyntaxErrorException error){
        addParserErrors(doc, Arrays.asList(error));
    }

    private void processErrorMessage(BaseDocument doc, Exception ex, StringBuilder statMsg) {
        if (ex instanceof FXDSyntaxErrorException) {
            FXDSyntaxErrorException syntaxError = (FXDSyntaxErrorException) ex;
            statMsg.append("Syntax error: "); //NOI18N
            String msg = syntaxError.getLocalizedMessage();

            try {
                int ErrRow = SyntaxErrorsHighlightingTask.getRow(syntaxError, (BaseDocument) doc);
                int ErrPosition = SyntaxErrorsHighlightingTask.getPosition(syntaxError, (BaseDocument) doc);
                msg += " at [" + ErrRow + "," + ErrPosition + "]"; //NOI18N
            } catch (BadLocationException bex) {
            }

            statMsg.append(msg);
            doc.putProperty(PROP_PARSE_ERROR_MSG, msg);
        } else {
            statMsg.append("Unknown error: "); //NOI18N
            String msg = ex.getLocalizedMessage();
            statMsg.append(msg);
            doc.putProperty(PROP_PARSE_ERROR_MSG, msg);
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
