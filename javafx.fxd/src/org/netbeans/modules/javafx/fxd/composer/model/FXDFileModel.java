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

package org.netbeans.modules.javafx.fxd.composer.model;

import com.sun.javafx.tools.fxd.FXDObjectElement;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import javax.swing.SwingUtilities;
import javax.swing.text.AttributeSet;
import org.netbeans.editor.BaseDocument;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.editor.structure.api.DocumentModel;
import org.netbeans.modules.editor.structure.api.DocumentModelException;
import org.netbeans.modules.editor.structure.api.DocumentModelStateListener;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import com.sun.javafx.tools.fxd.FXDRootElement;
import com.sun.javafx.tools.fxd.container.ContainerEntry;
import java.io.IOException;
import javax.swing.text.Document;
import org.netbeans.modules.javafx.fxd.composer.misc.FXDComposerUtils;
import org.netbeans.modules.javafx.fxd.composer.source.FXDDocumentModelProvider;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZEditorSupport;
/**
 *
 * @author Pavel Benes
 */
public final class FXDFileModel implements DocumentModelStateListener {   
    public static final String  DOCUMENT_ROOT_ELEMENT_TYPE = "ROOT_ELEMENT";  //NOI18N
    private static final String PROP_DOCUMENT_ENTRY        = "$ENTRY";
    
    //TODO use intern and direct compare instead of equals
    public static final String FXD_DOC             = "doc";          //NOI18N
    public static final String FXD_HEADER          = "header";       //NOI18N
    public static final String FXD_NODE            = "node";         //NOI18N
    public static final String FXD_ATTRIBUTE       = "attr";         //NOI18N
    public static final String FXD_ATTRIBUTE_ARRAY = "attr-array";   //NOI18N
    public static final String FXD_ARRAY_ELEM      = "array-elem";   //NOI18N
    public static final String FXD_ERROR           = "error";        //NOI18N
    
    private final    FXZArchive     m_archive;
    private final    String         m_entryName;
    private final    DocumentModel  m_docModel;
    private final    Object         m_lock = new Object();
    private volatile boolean        m_sourceChanged = false;
    private volatile boolean        m_updateInProgress = false;
    
    public interface ElementVisitor {
        public boolean visitElement( String elemType, String elemName, AttributeSet attrs) throws Exception;
    }
    
    public interface ElementAttrVisitor {
        public boolean visitAttribute( String attrName, String attrValue);
    }
        
    public FXDFileModel( FXZArchive archive, String entryName) throws IOException, DocumentModelException {
        assert archive != null;
        m_archive   = archive;
        m_entryName = entryName;
        m_docModel = getDocumentModel( archive.getDataObject(), entryName);
        assert FXDComposerUtils.safeEquals( createIdPrefix(entryName),
                getEntryPrefixForDocument(m_docModel.getDocument()));
        m_docModel.addDocumentModelStateListener(this);
    }
          
    BaseDocument getDocument() {
        return (BaseDocument) m_docModel.getDocument();
    }
    
    public DocumentModel getDocumentModel() {
        return m_docModel;
    }

    public boolean isError() {
        DocumentElement root = m_docModel.getRootElement();
        if ( root != null && root.getElementCount() == 1) {
            if ( isError(root.getElement(0))) {
                return true;
            }
        }
        return false;
    }
    
    public String getErrorMsg() {
        return (String) m_docModel.getDocument().getProperty( FXDDocumentModelProvider.PROP_PARSE_ERROR);
    }
    
    public synchronized void updateModel() {
        assert SwingUtilities.isEventDispatchThread() == false : "Model update cannot be called in AWT thread.";  //NOI18N

        synchronized (m_lock) {
            if (m_sourceChanged) {
                //System.err.println("Forcing model update"); //NOI18N
                m_docModel.forceUpdate();
            } else if (!m_updateInProgress) {
                //System.err.println("Model already up to date."); //NOI18N
                return;
            }

            while (m_sourceChanged || m_updateInProgress) {
                //System.err.println("Waiting for model update..."); //NOI18N
                try {
                    m_lock.wait();
                    //System.err.println("Wait ended."); //NOI18N
                } catch (InterruptedException ex) {
                }
            }
        }
    }
    
    public void readLock() {
        m_docModel.readLock();
    }
    
    public void readUnlock() {
        m_docModel.readUnlock();
    }
    
    DocumentElement getElementById( String id) {
        assert id != null;
        assert m_docModel != null;
        return findElement( m_docModel.getRootElement(), id);
    }

    synchronized FXDRootElement getRootNode() {
        //TODO Use better construction
        if (m_docModel != null) {
            DocumentElement elem = m_docModel.getRootElement().getElement(0);
            if (elem != null) {
                return (FXDRootElement) DocumentElementWrapper.wrap(ContainerEntry.create(m_archive, m_entryName), elem);
            }
        }
        return null;
    }
        
    protected DocumentElement findElement( final DocumentElement de, final String id) {
        //TODO Do not use the String id, use number instead and use fact that the sequence is monotonuous
        // TODO: we have leaved existing ids to have working references
        DocumentElement result = null;

        if (id.equals(getElementId(de))) {
            return de;
        } else {
            List<DocumentElement> children = de.getChildren();
            for (DocumentElement cde : children) {
                if ((result = findElement(cde, id)) != null) {
                    return result;
                }
            }
        }
        return result;
    }
            
    public static String getElementId(final DocumentElement de) {
        AttributeSet attrs = de.getAttributes();
        if ( attrs.isDefined(FXDObjectElement.ATTR_NAME_ID)){
            String id = (String) attrs.getAttribute(FXDObjectElement.ATTR_NAME_ID);
            if (id.charAt(0) == '"'){
                return id.substring(1, id.length() - 1);
            }
            return id;
        } else if ( de.getStartOffset() < de.getEndOffset()) {
            String entryName = getEntryPrefixForDocument( de.getDocument());
            return entryName.concat( String.valueOf(de.getStartOffset()));
        } else {
            //TODO
            System.err.println("Deleted element found: " + de);  //NOI18N
            //SceneManager.log(Level.SEVERE, "Deleted element found: " + de); //NOI18N
            return null;
        }
    }
    
    public static boolean isSignificant(final DocumentElement de) {
        String id = (String) de.getAttributes().getAttribute( FXDObjectElement.ATTR_NAME_ID);
        return id != null && id.length() > 0;
    }
        
    public static boolean isError( final DocumentElement de) {
        return de != null && FXD_ERROR.equals(de.getType());
    }

    public static String getEntryPrefixForDocument( Document doc) {
        return (String) doc.getProperty(PROP_DOCUMENT_ENTRY);
    }

    public static void setEntryPrefixForDocument( Document doc, String entryName) {
        doc.putProperty( PROP_DOCUMENT_ENTRY, entryName);
    }

    public static BaseDocument getDocument( FXZDataObject dObj, String entryName) throws IOException {
        FXZEditorSupport supp = dObj.getEditorSupport( entryName);
        if(supp == null) {
            throw new IllegalArgumentException("No editor support found on DataObject" + dObj.getName()); // NOI18N
        } else {
            return (BaseDocument) supp.openDocument(false);
        }
    }
    
    public static DocumentModel getDocumentModel(final FXZDataObject dObj, String entryName) throws IOException, DocumentModelException {
        return DocumentModel.getDocumentModel(getDocument(dObj, entryName));
    }

    public static String createIdPrefix( String entryName) {
        entryName = FXDComposerUtils.removeEntryExtension(entryName);
        entryName += "$";
        return entryName;
    }
        
    /**
     * Convenience helper method.
     */
    public static List<DocumentElement> getParentChain(DocumentElement elem) {
        List<DocumentElement> list = new ArrayList<DocumentElement>();
        while (elem != null) {
            list.add(elem);
            elem = elem.getParentElement();
        }
        return list;
    }   
        
    private static final int MAX_ATTR_VALUE_LENGTH = 50;
    
    public static void visitAttributes( final DocumentElement de, final ElementAttrVisitor visitor, boolean shortValues) {
        assert de != null;
    
        AttributeSet attrs = de.getAttributes();
        Enumeration attrNames = attrs.getAttributeNames();
        while(attrNames.hasMoreElements()) {
            String name = (String)attrNames.nextElement();
            String value = (String)attrs.getAttribute(name);
            
            if (value != null) {
                if ( shortValues && value.length() > MAX_ATTR_VALUE_LENGTH) {
                    value = value.substring(0, MAX_ATTR_VALUE_LENGTH-3) + "...";   //NOI18N
                }
                if (!visitor.visitAttribute(name, value)) {
                    return;
                }
            }
        }
    }    
    
    public void visitElements( ElementVisitor visitor) throws Exception {
        visitElements( m_docModel.getRootElement(), visitor);
    }
    
    public boolean visitElements( DocumentElement de, ElementVisitor visitor) throws Exception {
        visitor.visitElement( de.getType(), de.getName(), de.getAttributes());
        List<DocumentElement> children = de.getChildren();
        for (DocumentElement cde : children) {
            if (!visitElements(cde, visitor)) {
                return false;
            }
        }
        return true;
    }
        
    public static boolean isEqual( final AttributeSet attrs, final Map<String,String> map) {
        int attrNum = attrs.getAttributeCount();
        
        if ( attrNum == map.size()) {
            Enumeration attrNames = attrs.getAttributeNames();
            
            while( attrNames.hasMoreElements()) {
                String attrName   = (String) attrNames.nextElement();  
       //         if ( !JSONObject.ATTR_NAME_ID.equals(attrName)) {
                    Object attrValue1 = attrs.getAttribute(attrName);
                    String attrValue2 = map.get(attrName);

                    if ( !isEqual( attrValue1, attrValue2)) {
                        System.err.println( String.format( "ATTR_CHANGED: %s, '%s' != '%s'", //NOI18N
                                attrName, attrValue1, attrValue2));  
                        return false;
                    }
       //         }
            }
            return true;
        } else {
            return false;
        }
    }
    
    public static boolean isEqual(final Object obj1, final Object obj2) {
        if ( obj1 == obj2) {
            return true;
        }
        
        if ( obj1 == null || obj2 == null) {
            return false;
        }
        
        return obj1.equals(obj2);
    }

    public void sourceChanged() {
        synchronized (m_lock) {
            //System.err.println("Document source changed."); //NOI18N
            m_sourceChanged = true;
            m_archive.incrementChangeTicker(false);            
            m_lock.notifyAll();
        }
    }

    public void scanningStarted() {
        synchronized (m_lock) {
            //System.err.println("Document scanning started."); //NOI18N
            //getSceneManager().setBusyState(MODEL_UPDATE_TOKEN, true);
            m_updateInProgress = true;
            m_sourceChanged = false;
            m_lock.notifyAll();
        }
    }

    public void updateStarted() {
    }

    public void updateFinished() {
        synchronized (m_lock) {
            //System.err.println("Model update finished."); //NOI18N
            m_updateInProgress = false;
            m_lock.notifyAll();
            /*
            try {
                FXDNode rootNode = FXDDocumentModelProvider.getRoot();
                DocumentElement rootDE = m_docModel.getRootElement();
                if ( rootNode != null && rootDE != null && rootDE.getElementCount() == 1) {  
                    System.err.println("Checking models ...");
                    checkEqual( DocumentElementWrapper.wrap( rootDE.getElement(0), false, false), rootNode);
                }
            } catch(Exception e) {
                e.printStackTrace();
            }
             */
//            getSceneManager().setBusyState(MODEL_UPDATE_TOKEN, false);
        }
    }

    /*
    private static boolean checkEqual( final com.sun.javafx.tools.fxd.FXDElement elem1, final com.sun.javafx.tools.fxd.FXDElement elem2) {
        if ( elem1.getKind() != elem2.getKind()) {
            System.err.println( String.format("Kind mismatch: %s - %s", elem1.toString(), elem2.toString()));
            return false;
        }
        
        if ( elem1 instanceof FXDNode) {
            FXDNode node1 = (FXDNode) elem1;
            FXDNode node2 = (FXDNode) elem2;
            
            int nodeAttrNum = node1.getAttrNum();
            if ( node2.getAttrNum() != nodeAttrNum) {
                System.err.println( String.format("Attribute count mismatch: %s - %s", node1.toString(), node2.toString()));
                return false;
            }            
            
            Enumeration<String> attrNames = node1.getAttrNames();
            while ( attrNames.hasMoreElements()) {
                String attrName = attrNames.nextElement();

                Object v1 = node1.getAttrValue(attrName);
                Object v2 = node2.getAttrValue(attrName);
                
                if ( v1 == null || v2 == null) {
                    System.err.println( String.format("Missing attribute %s", attrName));
                    return false;
                }
                
                if ( v1 instanceof com.sun.javafx.tools.fxd.FXDElement) {
                    if ( !(v2 instanceof com.sun.javafx.tools.fxd.FXDElement)) {
                        System.err.println( String.format("Different type for attribute %s", attrName));
                        return false;
                    }
                } else {
                    if ( !v1.equals(v2)) {
                        System.err.println( String.format("Attribute %s value mismatch '%s' x '%s' \n: %s - %s", attrName, v1, v2, node1.toString(), node2.toString()));
                        return false;
                    }
                }
            }
        }
            
        Enumeration<com.sun.javafx.tools.fxd.FXDElement> children1 = elem1.children();
        Enumeration<com.sun.javafx.tools.fxd.FXDElement> children2 = elem2.children();

        while( children1.hasMoreElements()) {
            if ( !children2.hasMoreElements()) {
                System.err.println( String.format("Children mismatch %s - %s", elem1.toString(), elem2.toString()));
                return false;
            }

            if ( !checkEqual( children1.nextElement(), children2.nextElement())) {
                return false;
            }
        }
        if ( children2.hasMoreElements()) {
            System.err.println( String.format("Children mismatch %s - %s", elem1.toString(), elem2.toString()));
            return false;
        }
        return true;
    }
    
    private static void collectChildren( final DocumentElement de, List<DocumentElement> childrenList) {
        for ( int i = 0; i < de.getElementCount(); i++) {
            DocumentElement child = de.getElement(i);
            if ( FXDFileModel.FXD_NODE.equals( child.getType())) {
                childrenList.add( child);
            } else {
                collectChildren(child, childrenList);
            }
        }
    }
    private static void collectChildren( final com.sun.javafx.tools.fxd.FXDElement elem, List<FXDNode> childrenList) {
        Enumeration<com.sun.javafx.tools.fxd.FXDElement> children = elem.children();
        
        while( children.hasMoreElements()) {
            com.sun.javafx.tools.fxd.FXDElement child = children.nextElement();
            if ( child instanceof FXDNode) {
                childrenList.add( (FXDNode) child);
            } else {
                collectChildren(child, childrenList);
            }
        }
    }
     */
}
