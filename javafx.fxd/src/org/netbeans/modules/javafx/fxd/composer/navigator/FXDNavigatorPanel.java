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
package org.netbeans.modules.javafx.fxd.composer.navigator;

import java.util.Collection;
import javax.swing.JComponent;
import org.netbeans.modules.javafx.fxd.dataloader.FXDZDataObject;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.netbeans.spi.navigator.NavigatorPanel;
import org.openide.util.Lookup;
import org.openide.util.LookupEvent;
import org.openide.util.LookupListener;
import org.openide.util.NbBundle;

/** An implementation of NavigatorPanel for FXD navigator.
 *
 * @author Pavel Benes 
 * @version 1.0
 */
public class FXDNavigatorPanel implements NavigatorPanel {    
    private FXDNavigatorContent m_navigator = FXDNavigatorContent.getDefault();    
    private Lookup.Result       m_dataObjectSelection;
    
    private final LookupListener dataObjectListener = new LookupListener() {
        public void resultChanged(LookupEvent ev) {
            try {
                navigate(m_dataObjectSelection.allInstances());
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    };
        
    /** public no arg constructor needed for system to instantiate the provider. */
    public FXDNavigatorPanel() {
    }
    
    public String getDisplayHint() {
        return NbBundle.getMessage(FXDNavigatorPanel.class, "FXD_files_navigator");  //NOI18N
    }
    
    public String getDisplayName() {
        return NbBundle.getMessage(FXDNavigatorPanel.class, "FXD_View");  //NOI18N
    }
    
    public JComponent getComponent() {
        return m_navigator;
    }
    
    public Lookup getLookup() {
        return null;
    }    
    
    public void panelActivated(Lookup context) {
        m_dataObjectSelection = context.lookup(new Lookup.Template<FXDZDataObject>(FXDZDataObject.class));
        m_dataObjectSelection.addLookupListener(dataObjectListener);
        m_dataObjectSelection.allItems();
        dataObjectListener.resultChanged(null);
    }
    
    public void panelDeactivated() {
        m_dataObjectSelection.removeLookupListener(dataObjectListener);
        m_dataObjectSelection = null;
        m_navigator.release(); //hide the UI
    }

    @SuppressWarnings("fallthrough")
    public void navigate(Collection selectedFiles) throws Exception {
        switch( selectedFiles.size()) {
            default:
                System.err.println("Multiple selection not allowed; using first node ..."); //NOI18N
            case 1:
                final FXDZDataObject dObj = (FXDZDataObject) selectedFiles.iterator().next();
                m_navigator.navigate(dObj);        
                break;
            case 0:
                m_navigator.navigate(null);
                break;
        }
    }    
}
