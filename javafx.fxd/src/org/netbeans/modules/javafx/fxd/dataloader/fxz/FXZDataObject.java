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
import org.netbeans.modules.javafx.fxd.composer.model.FXDComposerController;
import org.netbeans.modules.javafx.fxd.composer.model.FXDComposerModel;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObjectExistsException;
import org.openide.loaders.MultiDataObject;
import org.openide.nodes.Node;
import org.openide.util.Exceptions;
import org.openide.util.Lookup;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */

public final class FXZDataObject extends MultiDataObject implements Lookup.Provider {     
    private static final long  serialVersionUID = 1L;
    
    public static final String FXZ_EXT = "fxz";  //NOI18N

    public static final int    VISUAL_VIEW_INDEX  = 0;
    public static final int    TEXT_VIEW_INDEX    = 1;
    public static final int    ARCHIVE_VIEW_INDEX = 2;
    
    final InstanceContent m_ic;
    private final     AbstractLookup        m_lookup;
    private final     FXZEditorSupport      m_edSup;
    private transient FXDComposerModel      m_model = null;
    private transient FXDComposerController m_controller = null;
    private transient int                   m_defaultViewIndex = VISUAL_VIEW_INDEX;
            
    public FXZDataObject(FileObject pf, FXZDataLoader loader) throws DataObjectExistsException, IOException {
        super(pf, loader);
        m_ic = new InstanceContent();
        m_lookup = new AbstractLookup(m_ic);
        m_ic.add( m_edSup = new FXZEditorSupport(this));
        m_ic.add(this);           
//        SceneManager.log(Level.INFO, "SVGDataObject created for " + pf.getPath()); //NOI18N
    }
    
    public void setDefaultView( int viewIndex) {
        m_defaultViewIndex = viewIndex;
    }
    
    public int getDefaultView() {
        return m_defaultViewIndex;
    }
    
    public synchronized FXDComposerModel getDataModel() {
        if (m_model == null) {
            try {
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
            m_controller = new FXDComposerController(this);
        }
        return m_controller;
    }
    
    public InstanceContent getLookupContent() {
        return m_ic;
    }
    
    public TopComponent getMTVC() {
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
    
    @Override
    public Node.Cookie getCookie(Class type) {
        Object o = m_lookup.lookup(type);
        return o instanceof Node.Cookie ? (Node.Cookie) o : null;
    }
    
    /*
    public static boolean isFXZArchive( final FileObject fo) {
        System.err.println("FO: " + fo);
        File file = FileUtil.toFile(fo);
        System.err.println("File: " + file);
        if ( file.exists() && file.isFile()) {
            if ( FXZ_EXT.equalsIgnoreCase(fo.getExt())) {
                return true;
            }
        }
        return false;
    }*/
}
