/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.model;

import javafx.fxd.FXDNode;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.AttributeSet;
import org.netbeans.editor.BaseDocument;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.editor.structure.api.DocumentModel;
import org.netbeans.modules.editor.structure.api.DocumentModelStateListener;
import org.netbeans.modules.javafx.fxd.dataloader.FXDDataObject;

/**
 *
 * @author Pavel Benes
 */
public class FXDFileModel implements DocumentListener, DocumentModelStateListener {   
    public static final String DOCUMENT_ROOT_ELEMENT_TYPE = "ROOT_ELEMENT";
    
//    private static final String PROP_FILE_MODEL = "prop-fxd-file-model";
    
    public static final String FXD_DOC       = "doc";
    public static final String FXD_HEADER    = "header";
    public static final String FXD_NODE      = "node";
    public static final String FXD_ATTRIBUTE = "attr";
    
    private final    FXZArchive     m_archive;
    private final    DocumentModel  m_docModel;
    private volatile boolean        m_changed = false;
        
    public interface ElementAttrVisitor {
        public boolean visitAttribute( String attrName, String attrValue);
    }
        
    public FXDFileModel( FXZArchive archive, DocumentModel docModel) {
        assert archive != null;
        assert docModel != null;
        m_archive  = archive;
        m_docModel = docModel;
        m_docModel.getDocument().addDocumentListener(this);
        System.err.println("File model created.");
    }
          
    BaseDocument getDocument() {
        return (BaseDocument) m_docModel.getDocument();
    }
    
    public DocumentModel getDocumentModel() {
        return m_docModel;
    }

    public synchronized void updateModel() {
        assert SwingUtilities.isEventDispatchThread() == false : "Model update cannot be called in AWT thread.";
        if ( m_changed) {
            System.err.println("Forcing model update");
            m_docModel.forceUpdate();
        }
    }
    
    DocumentElement getElementById( String id) {
        assert id != null;
        assert m_docModel != null;
        return findElement( m_docModel.getRootElement(), id);
    }

    public synchronized FXDNode getRootNode() {
        //TODO Use better construction
        return (FXDNode) DocumentElementWrapper.wrap( m_docModel.getRootElement().getElement(0));
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
        
    public void insertUpdate(DocumentEvent e) {
        documentChanged(e);
    }

    public void removeUpdate(DocumentEvent e) {
        documentChanged(e);
    }

    public void changedUpdate(DocumentEvent e) {
        documentChanged(e);
    }
    
    protected void documentChanged( DocumentEvent e) {
        System.err.println("Document changed.");
        m_archive.incrementChangeTicker();
        m_changed = true;
    }
    
    /**
     * Convenience helper method.
     */
    public static String getIdAttribute(DocumentElement de) {
        return String.valueOf(de.getStartOffset());
    }    
        
    public static DocumentModel getDocumentModel( final FXDDataObject dObj) {
        FXDFileModel fm = dObj.getDataModel().getFXDContainer().getFileModel();
        return fm != null ? fm.getDocumentModel() : null;
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
    
//    public static FXDFileModel get( final DocumentModel docModel) {
//        FXDFileModel fileModel = (FXDFileModel) docModel.getDocument().getProperty(PROP_FILE_MODEL);
//        assert fileModel != null;
//        return fileModel;
//    }
    
    private static final int MAX_ATTR_VALUE_LENGTH = 50;
    
    public static void visitAttributes( final DocumentElement de, final ElementAttrVisitor visitor) {
        assert de != null;
    
        AttributeSet attrs = de.getAttributes();
        Enumeration attrNames = attrs.getAttributeNames();
        while(attrNames.hasMoreElements()) {
            String name = (String)attrNames.nextElement();
            String value = (String)attrs.getAttribute(name);
            
            if (value != null) {
//                if ( FXDNode.ATTR_NAME_ID.equals(name) &&
//                     value.startsWith(JSONObject.INJECTED_ID_PREFIX)) {
//                     continue;
//                }
//                if ( JSONObject.ATTR_NODE_CLASS.equals(name)) {
//                    continue;
//                }
                
                if ( value.length() > MAX_ATTR_VALUE_LENGTH) {
                    value = value.substring(0, MAX_ATTR_VALUE_LENGTH-3) + "...";
                }
                if (!visitor.visitAttribute(name, value)) {
                    return;
                }
            }
        }
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
                        System.err.println( String.format( "ATTR_CHANGED: %s, '%s' != '%s'", attrName, attrValue1, attrValue2));
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
    }

    public void scanningStarted() {
    }

    public void updateStarted() {
    }

    public void updateFinished() {
        System.err.println("Model update finished.");
        m_changed = false;
    }
}
