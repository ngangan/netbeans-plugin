/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Oracle and/or its affiliates. All rights reserved.
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

package org.netbeans.modules.visage.navigation;

import java.awt.BorderLayout;
import java.io.Serializable;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.netbeans.api.visage.editor.ElementJavadoc;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;

/**
 * Top component which displays something.
 * 
 * @author Sandip V. Chitale (Sandip.Chitale@Sun.Com)
 * @author Anton Chechel - visage modifications
 */
public class JavafxdocTopComponent extends TopComponent {

    private static final Logger LOGGER = Logger.getLogger(JavafxdocTopComponent.class.getName());
    
    private static JavafxdocTopComponent instance;
    /** path to the icon used by the component and its open action */
    public static final String ICON_PATH = "org/netbeans/modules/visage/navigation/resources/javadoc_action.png"; // NOI18N
    
    private static final String PREFERRED_ID = "JavafxdocTopComponent"; // NOI18N
    
    private DocumentationScrollPane documentationPane;
    
    private JavafxdocTopComponent() {
        setName(NbBundle.getMessage(JavafxdocTopComponent.class, "CTL_JavafxdocTopComponent")); // NOI18N
        setToolTipText(NbBundle.getMessage(JavafxdocTopComponent.class, "HINT_JavafxdocTopComponent")); // NOI18N
        setIcon(Utilities.loadImage(ICON_PATH, true));
        
        documentationPane = new DocumentationScrollPane( false );
        setLayout(new BorderLayout());
        add( documentationPane, BorderLayout.CENTER );
    }
    
    void setJavadoc( ElementJavadoc doc ){    
        documentationPane.setData( doc );
    }
    
    public static boolean shouldUpdate() {
        if ( instance == null ) {
            return false;
        }
        else  {
            return instance.isShowing();
        }
    }

    /**
     * Gets default instance. Do not use directly: reserved for *.settings files only,
     * i.e. deserialization routines; otherwise you could get a non-deserialized instance.
     * To obtain the singleton instance, use {@link findInstance}.
     */
    public static synchronized JavafxdocTopComponent getDefault() {
        if (instance == null) {
            instance = new JavafxdocTopComponent();
        }
        return instance;
    }
    
    /**
     * Obtain the JavafxdocTopComponent instance. Never call {@link #getDefault} directly!
     */
    public static synchronized JavafxdocTopComponent findInstance() {
        TopComponent win = WindowManager.getDefault().findTopComponent(PREFERRED_ID);
        if (win == null) {
            LOGGER.log(Level.WARNING, 
                       "Cannot find MyWindow component. It will not be located properly in the window system."); // NOI18N
            return getDefault();
        }
        if (win instanceof JavafxdocTopComponent) {
            return (JavafxdocTopComponent)win;
        }
        LOGGER.log(Level./* Shut up! Logged dozens of times in every session. */FINE,
                "There seem to be multiple components with the '" + PREFERRED_ID + // NOI18N
                "' ID. That is a potential source of errors and unexpected behavior."); // NOI18N
        return getDefault();
    }
   
    @Override
    public int getPersistenceType() {
        return TopComponent.PERSISTENCE_ALWAYS;
    }

    @Override
    protected void componentActivated() {
        super.componentActivated();
        documentationPane.getViewport().getView().requestFocusInWindow();
    }
    
    @Override
    public void componentOpened() {
        documentationPane.getViewport().getView().requestFocusInWindow();
    }

    @Override
    protected void componentShowing() {
        super.componentShowing();
        CaretListeningFactory.runAgain();
    }
    
    @Override
    public void componentClosed() {
    }
    
    /** replaces this in object stream */
    @Override
    public Object writeReplace() {
        return new ResolvableHelper();
    }
    
    @Override
    protected String preferredID() {
        return PREFERRED_ID;
    }
    
    final static class ResolvableHelper implements Serializable {
        private static final long serialVersionUID = 1L;
        public Object readResolve() {
            return JavafxdocTopComponent.getDefault();
        }
    }
    
}
