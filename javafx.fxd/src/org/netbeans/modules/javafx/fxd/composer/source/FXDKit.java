/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.source;

import java.util.Map;
import javax.swing.Action;
import javax.swing.text.TextAction;
import org.netbeans.modules.editor.NbEditorKit;

/**
 *
 * @author Pavel Benes
 */
public class FXDKit extends NbEditorKit implements org.openide.util.HelpCtx.Provider {

    /** Serial Version UID */
    private static final long serialVersionUID =1L;
    
    /** Default FXD Mime Type. */
    public static final String MIME_TYPE = "text/x-fxd"; // NOI18N

    
    // hack to be settings browseable //??? more info needed
    public static Map settings;
    
    
    public org.openide.util.HelpCtx getHelpCtx() {
        return new org.openide.util.HelpCtx(FXDKit.class);
    }
    
    /** Create new instance of syntax coloring parser */
    /*
    @Override
    public Syntax createSyntax(Document doc) {
        return new XMLDefaultSyntax();
    }*/
  
    /*
    @Override
    public Document createDefaultDocument() {
        //Document doc = new FXDEditorDocument(getContentType());
        //return doc;
        return super.createDefaultDocument();
    }*/


    /** Create syntax support */
    /*
    @Override
    public SyntaxSupport createSyntaxSupport(BaseDocument doc) {
        return new XMLSyntaxSupport(doc);
    }*/
    
    /*
    @Override
    public void install(JEditorPane c) {
        super.install(c);
        if (Boolean.getBoolean("netbeans.experimental.xml.nodeselectors")) {  // NOI18N
            new NodeSelector(c);
        }
    }*/

    // hack to be settings browseable //??? more info needed    
    public static void setMap(Map map) {
        settings = map;
    }

    // hack to be settings browseable //??? more info needed        
    public Map getMap() {
        return settings;
    }

    //??? +xml handling
    public @Override String getContentType() {
        return MIME_TYPE;
    }

    /**
     * Provide XML related actions.
     */
    protected @Override Action[] createActions() {
        Action[] actions = new Action[] {
        };
        return TextAction.augmentList(super.createActions(), actions);
    }    
    
    /*
    public class FXDEditorDocument extends NbEditorDocument {
        public FXDEditorDocument(Class kitClass) {
            super(kitClass);
        }

        public FXDEditorDocument(String mimeType) {
            super(mimeType);
        }
        
        public @Override boolean addLayer(DrawLayer layer, int visibility) {
            //filter out the syntax layer adding
            if(!(layer instanceof DrawLayerFactory.SyntaxLayer)) {
                return super.addLayer(layer, visibility);
            } else {
                return false;
            }
        }
    }
   */ 
}
