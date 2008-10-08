/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 2008 Sun Microsystems, Inc. All rights reserved.
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

package org.netbeans.modules.javafx.preview;

import java.awt.Dimension;
import java.awt.Point;
import java.beans.PropertyChangeEvent;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import org.netbeans.api.project.Project;
import org.netbeans.modules.javafx.editor.JavaFXDocument;
import org.netbeans.modules.javafx.project.JavaFXProject;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import java.beans.PropertyChangeListener;
import javax.swing.text.Document;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.modules.editor.NbEditorUtilities;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;

class NBSidePreviewServer extends UnicastRemoteObject implements NBSideServerFace, PropertyChangeListener {
    private PreviewSideServerFace previewSideServer = null;
    private JavaFXDocument document = null;
    
    NBSidePreviewServer(JavaFXDocument document) throws RemoteException {
        super();
        this.document = document;
        Project project = getProject(document);
        PropertyEvaluator evaluator =((JavaFXProject)project).evaluator();
        evaluator.addPropertyChangeListener(this);
    }

    public void setPreviewSideServerFace(PreviewSideServerFace previewSideServer) {
        this.previewSideServer = previewSideServer;
    }

    public void previewWindowClosed() throws RemoteException {
        document.enableExecution(false);
    }
    
    public void setPreviewPlacement(Point previewLocation, Dimension previewSize) throws RemoteException {
        document.setPreviewPlacement(previewLocation, previewSize);
    }

    public void propertyChange(PropertyChangeEvent evt) {
        if (evt.getPropertyName().contentEquals("platform.active")) {                             // NOI18N
            if (previewSideServer != null) {
                try {
                    previewSideServer.notifyClassPathChanged();
                } catch (RemoteException ex) {
                    Exceptions.printStackTrace(ex);
                }
            }
        }
    }
    
    public static Project getProject(Document doc){
        return getProject(NbEditorUtilities.getFileObject(doc));
    }

    public static Project getProject(FileObject fileObject){
        return FileOwnerQuery.getOwner(fileObject);
    }
}