/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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
 * 
 * Contributor(s):
 * 
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.fxd.dataloader.fxz;

import java.io.IOException;
import org.netbeans.core.api.multiview.MultiViewHandler;
import org.netbeans.core.api.multiview.MultiViewPerspective;
import org.netbeans.core.api.multiview.MultiViews;
import org.netbeans.modules.javafx.fxd.composer.model.FXDComposerController;
import org.netbeans.modules.javafx.fxd.composer.model.FXDComposerModel;
import org.netbeans.modules.javafx.fxd.dataloader.FXDZDataObject;
import org.openide.cookies.SaveCookie;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObjectExistsException;
import org.openide.nodes.Node;
import org.openide.nodes.CookieSet;
import org.openide.nodes.Node.Cookie;
import org.openide.util.Exceptions;
import org.openide.util.Lookup;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;
import org.openide.util.lookup.ProxyLookup;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */

public final class FXZDataObject extends FXDZDataObject implements Lookup.Provider {     
    private static final long  serialVersionUID = 1L;
    
    public static final String FXZ_EXT = "fxz";  //NOI18N

    public static final int    VISUAL_VIEW_INDEX  = 0;
    public static final int    TEXT_VIEW_INDEX    = 1;
    public static final int    ARCHIVE_VIEW_INDEX = 2;
    
    InstanceContent                                  m_ic;
    private transient volatile Lookup                m_lookup;
    private transient          FXZEditorSupport      m_edSup = null;
    private transient          FXDComposerModel      m_model = null;
    private transient          FXDComposerController m_controller = null;
    private transient          int                   m_defaultViewIndex;
            
    public FXZDataObject(FileObject pf, FXZDataLoader loader) throws DataObjectExistsException, IOException {
        super(pf, loader);
        getCookieSet().add(FXZEditorSupport.class, new CookieSet.Factory() {
            public <T extends Cookie> T createCookie(Class<T> klass) {
                return klass.cast(getEditorSupport());
            }
        });
        
        m_ic = new InstanceContent();
        m_lookup = new ProxyLookup(getCookieSet().getLookup(), new AbstractLookup(m_ic));
        m_defaultViewIndex = VISUAL_VIEW_INDEX;
//        SceneManager.log(Level.INFO, "SVGDataObject created for " + pf.getPath()); //NOI18N
    }
            
    public void notifyEditorSupportModified () {
        getEditorSupport().notifyModified ();
    }    
    
    //TODO better name
    public synchronized void init() {
        getController().init();
    }
    
    private synchronized FXZEditorSupport getEditorSupport() {
        if(m_edSup == null) {
            m_edSup = new FXZEditorSupport(this);
        }
        return m_edSup;
    }
    
    public synchronized void reset() {
        m_ic     = new InstanceContent();
        m_lookup = new ProxyLookup(getCookieSet().getLookup(), new AbstractLookup(m_ic));

        m_defaultViewIndex = VISUAL_VIEW_INDEX;
        if ( m_edSup != null) {
            m_edSup.resetMVTC();
        }
        m_model = null;
        if ( m_controller != null) {
            m_controller.close();
            m_controller = null;
        }
    }

    public void setDefaultView( int viewIndex) {
        m_defaultViewIndex = viewIndex;
    }
    
    public int getDefaultView() {
        return m_defaultViewIndex;
    }
    
    public void selectView( int index) {
        MultiViewHandler handler = MultiViews.findMultiViewHandler( getMVTC());
        if (handler != null) {
            MultiViewPerspective perspective =  handler.getPerspectives()[index];        
            handler.requestActive(perspective);
        } else {
            setDefaultView(index);
        }
    }
    
    public synchronized FXDComposerModel getDataModel() {
        if (m_model == null) {
            try {
                //System.err.println("Creating the DataModel");
                m_model = new FXDComposerModel(this);
                //SceneManager.log(Level.INFO, "SceneManager created for " + getPrimaryFile().getPath()); //NOI18N
            } catch (Exception ex) {
                Exceptions.printStackTrace(ex);
            } 
            //SceneManager.log(Level.INFO, "SceneManager created for " + getPrimaryFile().getPath()); //NOI18N
        }
        return m_model;        
    }
    
    public synchronized FXDComposerController getController() {
        if ( m_controller == null) {
            //System.err.println("Creating the Controller");
            m_controller = new FXDComposerController(this);
        }
        return m_controller;
    }
    
    public InstanceContent getLookupContent() {
        return m_ic;
    }
    
    public TopComponent getMVTC() {
        //TODO Check if the view callback is not a better option
        return m_edSup.getMVTC();
    }
    
    @Override
    protected Node createNodeDelegate() {
        return new FXZDataNode(this);
    }
    
    @Override
    public Lookup getLookup() {
        return m_lookup;
    }
    
   public void addSaveCookie(SaveCookie cookie){
        getCookieSet().add(cookie);
     }

    public void removeSaveCookie(){
        Node.Cookie cookie = getCookie(SaveCookie.class);
        if (cookie!=null) getCookieSet().remove(cookie);
    }    
}
