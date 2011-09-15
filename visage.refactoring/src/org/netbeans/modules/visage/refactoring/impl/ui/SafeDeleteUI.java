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

package org.netbeans.modules.visage.refactoring.impl.ui;

import java.io.IOException;
import java.util.Collection;
import java.util.ResourceBundle;
import javax.swing.event.ChangeListener;
import org.netbeans.api.fileinfo.NonRecursiveFolder;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.queries.VisibilityQuery;
import org.netbeans.modules.refactoring.api.AbstractRefactoring;
import org.netbeans.modules.refactoring.api.Problem;
import org.netbeans.modules.refactoring.api.SafeDeleteRefactoring;
import org.netbeans.modules.refactoring.spi.ui.CustomRefactoringPanel;
import org.netbeans.modules.refactoring.spi.ui.RefactoringUI;
import org.netbeans.modules.refactoring.spi.ui.RefactoringUIBypass;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;
import org.openide.util.Exceptions;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

/**
 * A CustomRefactoringUI subclass that represents Safe Delete
 * @author Bharath Ravikumar
 */
public class SafeDeleteUI implements RefactoringUI, RefactoringUIBypass{
    
    private final SafeDeleteRefactoring refactoring;
    
    private Collection<? extends FileObject> elementsToDelete;
    
    private SafeDeletePanel panel;
    
    private boolean regulardelete = false;
    /**
     * Creates a new instance of SafeDeleteUI
     * @param selectedElements An array of selected Elements that need to be 
     * safely deleted
     */
    public SafeDeleteUI(SafeDeleteRefactoring refactoring, boolean regulardelete) {
        this.refactoring = refactoring;
        this.elementsToDelete = refactoring.getRefactoringSource().lookupAll(FileObject.class);
        this.regulardelete = regulardelete;
    }

    /**
     * Creates a new instance of SafeDeleteUI
     * @param selectedElements An array of selected Elements that need to be 
     * safely deleted
     */
    public SafeDeleteUI(SafeDeleteRefactoring refactoring) {
        this(refactoring, false);
    }
    
    /**
     * Delegates to the fastCheckParameters of the underlying
     * refactoring
     * @return Returns the result of fastCheckParameters of the
     * underlying refactoring
     */
    public org.netbeans.modules.refactoring.api.Problem checkParameters() {
        refactoring.setCheckInComments(panel.isSearchInComments());
        return refactoring.fastCheckParameters();
    }
    
    public String getDescription() {
        //TODO: Check bounds here. Might throw an OutofBoundsException otherwise.
//        if (elementsToDelete[0] instanceof JavaClass) {
//            return getString("DSC_SafeDelClasses", elementsToDelete);// NOI18N
//        } else {
//            if (elementsToDelete[0] instanceof ExecutableElement) {
//                if (elementsToDelete.length > 1) 
//                    return getString("DSC_SafeDelMethods");// NOI18N
//                else 
//                    return getString("DSC_SafeDelMethod", elementsToDelete[0]);// NOI18N
//            }
//            
//        }
//        if(elementsToDelete[0] instanceof Resource){
//                return NbBundle.getMessage(SafeDeleteUI.class, "DSC_SafeDel", 
//                        ((Resource)elementsToDelete[0]).getName()); // NOI18N
//        }
        NonRecursiveFolder folder = refactoring.getRefactoringSource().lookup(NonRecursiveFolder.class);
        if (folder != null) {
            return NbBundle.getMessage(SafeDeleteUI.class, "DSC_SafeDelPkg", folder); // NOI18N
        }
        
        return NbBundle.getMessage(SafeDeleteUI.class, "DSC_SafeDel", elementsToDelete); // NOI18N
    }
    
    public org.openide.util.HelpCtx getHelpCtx() {
        
        return new HelpCtx(SafeDeleteUI.class.getName());
    }
    
    public String getName() {
        
        return NbBundle.getMessage(SafeDeleteUI.class, "LBL_SafeDel"); // NOI18N
    }
    
    public CustomRefactoringPanel getPanel(ChangeListener parent) {
        //TODO:Do you want to just use Arrays.asList?
        if(panel == null)
            panel = new SafeDeletePanel(refactoring, regulardelete, parent);
        return panel;
    }
    
    public AbstractRefactoring getRefactoring() {
        
        return refactoring;
    }
    
    public boolean hasParameters() {
        
        return true;
    }
    /**
     * Returns false, since this refactoring is not a query.
     * @return false
     */
    public boolean isQuery() {
        return false;
    }
    
    public Problem setParameters() {
        refactoring.setCheckInComments(panel.isSearchInComments());
        return refactoring.checkParameters();
    }
    
    //Helper methods------------------
    
    public boolean isRefactoringBypassRequired() {
        return panel.isRegularDelete();
    }

    public void doRefactoringBypass() throws IOException {
        // #172199
        FileUtil.runAtomicAction(new FileSystem.AtomicAction() {
            public void run() throws IOException {
                for (FileObject file:getRefactoring().getRefactoringSource().lookupAll(FileObject.class)) {
                    DataObject.find(file).delete();
                }
                NonRecursiveFolder f = (NonRecursiveFolder) getRefactoring().getRefactoringSource().lookup(NonRecursiveFolder.class);
                if (f!=null) {
                    deletePackage(f.getFolder());
                }
            }
        });
    }
    
    private void deletePackage(FileObject source) {
        FileObject root = ClassPath.getClassPath(source, ClassPath.SOURCE).findOwnerRoot(source);

        DataFolder dataFolder = DataFolder.findFolder(source);

        FileObject parent = dataFolder.getPrimaryFile().getParent();
        // First; delete all files except packages

        try {
            DataObject ch[] = dataFolder.getChildren();
            boolean empty = true;
            for (int i = 0; ch != null && i < ch.length; i++) {
                if (!ch[i].getPrimaryFile().isFolder()) {
                    ch[i].delete();
                }
                else if (empty && VisibilityQuery.getDefault().isVisible(ch[i].getPrimaryFile())) {
                    // 156529: hidden folders should be considered as empty content
                    empty = false;
                }
            }

            // If empty delete itself
            if ( empty ) {
                dataFolder.delete();
            }

            // Second; delete empty super packages
            while (!parent.equals(root) && parent.getChildren().length == 0) {
                FileObject newParent = parent.getParent();
                parent.delete();
                parent = newParent;
            }
        } catch (IOException ex) {
            Exceptions.printStackTrace(ex);
        }

    }
}
