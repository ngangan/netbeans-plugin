/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.model;

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
import com.sun.javafx.tools.fxd.FXDNode;
import java.io.IOException;
import org.openide.cookies.EditorCookie;
/**
 *
 * @author Pavel Benes
 */
public class FXDFileModel implements DocumentModelStateListener {   
    public static final String DOCUMENT_ROOT_ELEMENT_TYPE = "ROOT_ELEMENT";  //NOI18N
    
    //TODO use intern and direct compare instead of equals
    public static final String FXD_DOC             = "doc";          //NOI18N
    public static final String FXD_HEADER          = "header";       //NOI18N
    public static final String FXD_NODE            = "node";         //NOI18N
    public static final String FXD_ATTRIBUTE       = "attr";         //NOI18N
    public static final String FXD_ATTRIBUTE_ARRAY = "attr-array";   //NOI18N
    public static final String FXD_ARRAY_ELEM      = "array-elem";   //NOI18N
    
    private final    FXZArchive     m_archive;
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
        
    public FXDFileModel( FXZArchive archive) throws IOException, DocumentModelException {
        assert archive != null;
        m_archive  = archive;
        m_docModel = getDocumentModel( archive.getDataObject());
        m_docModel.addDocumentModelStateListener(this);
        //m_docModel.getDocument().addDocumentListener(this);
        //System.err.println("File model created."); //NOI18N
    }
          
    BaseDocument getDocument() {
        return (BaseDocument) m_docModel.getDocument();
    }
    
    public DocumentModel getDocumentModel() {
        return m_docModel;
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

    public synchronized FXDNode getRootNode() {
        //TODO Use better construction
        return (FXDNode) DocumentElementWrapper.wrap( m_docModel.getRootElement().getElement(0), true);
    }    
        
    protected DocumentElement findElement( final DocumentElement de, final String id) {
        //TODO Do not use the String id, use number instead and use fact that the sequence is monotonuous
        DocumentElement result = null;
        
        if ( id.equals(getIdAttribute(de))) {
            return de;
        } else {
            int childNum = de.getElementCount();
            for (int i = 0; i < childNum; i++) {
                if ( (result=findElement(de.getElement(i), id)) != null) {
                    return result;
                }
            }
        }
        return result;
    }
        
    /**
     * Convenience helper method.
     */
    public static String getIdAttribute(final DocumentElement de) {
        return String.valueOf(de.getStartOffset());
    }    
    
    public static String getElementId(final DocumentElement de) {
        if ( de.getStartOffset() < de.getEndOffset()) {
            return FXDFileModel.getIdAttribute(de);
        } else {
            //TODO
            System.err.println("Deleted element found: " + de);  //NOI18N
            //SceneManager.log(Level.SEVERE, "Deleted element found: " + de); //NOI18N
            return null;
        }
    }
    
    public static boolean isSignificant(final DocumentElement de) {
        String id = (String) de.getAttributes().getAttribute( FXDNode.ATTR_NAME_ID);
        return id != null && id.length() > 0;
    }
        
    public static BaseDocument getDocument( final FXZDataObject dObj) throws IOException {
        EditorCookie ec = (EditorCookie)dObj.getCookie(EditorCookie.class);
        if(ec == null) {
            throw new IllegalArgumentException("The DataObject " + dObj.getName() + "(class=" + dObj.getClass().getName() + ") has no EditorCookie!?");
        } else {
            return (BaseDocument)ec.openDocument();
        }
    }
    
    public static DocumentModel getDocumentModel(final FXZDataObject dObj) throws IOException, DocumentModelException {
        return DocumentModel.getDocumentModel(getDocument(dObj));
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
    
    private boolean visitElements( DocumentElement de, ElementVisitor visitor) throws Exception {
        visitor.visitElement( de.getType(), de.getName(), de.getAttributes());
        int num = de.getElementCount();
        for ( int i = 0; i < num; i++) {
            if (!visitElements( de.getElement(i), visitor)) {
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
//            getSceneManager().setBusyState(MODEL_UPDATE_TOKEN, false);
        }
    }
}
