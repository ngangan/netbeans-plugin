/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.impl.plugins.elements;

import org.netbeans.modules.javafx.refactoring.impl.plugins.BaseRefactoringElementImplementation;
import org.netbeans.modules.refactoring.api.RefactoringSession;
import org.openide.filesystems.FileObject;
import org.openide.util.NbBundle;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
abstract public class UpdatePackageDeclarationElement extends BaseRefactoringElementImplementation {
    private String oldPkgName, newPkgName;

    public UpdatePackageDeclarationElement(String oldPkgName, String newPkgName, FileObject srcFO, RefactoringSession session, boolean shouldBackup) {
        super(srcFO, session, shouldBackup);
        this.oldPkgName = oldPkgName;
        this.newPkgName = newPkgName;
    }

    public UpdatePackageDeclarationElement(String oldPkgName, String newPkgName, FileObject srcFO, RefactoringSession session) {
        super(srcFO, session);
        this.oldPkgName = oldPkgName;
        this.newPkgName = newPkgName;
    }

    final protected String getNewPkgName() {
        return newPkgName != null ? newPkgName : ""; // NOI18N
    }

    final protected String getOldPkgName() {
        return oldPkgName != null ? oldPkgName : ""; // NOI18N
    }

    final protected boolean isOldDefault() {
        return oldPkgName == null || oldPkgName.length() == 0;
    }

    final protected boolean isNewDefault() {
        return newPkgName == null || newPkgName.length() == 0;
    }

    @Override
    protected String getRefactoringText() {
        if (isOldDefault()) {
            return NbBundle.getMessage(UpdatePackageDeclarationElement.class, "LBL_RemovePackage", oldPkgName); // NOI18N
        } else if (isNewDefault()) {
            return NbBundle.getMessage(UpdatePackageDeclarationElement.class, "LBL_AddPackage", newPkgName); // NOI18N
        } else {
            return NbBundle.getMessage(UpdatePackageDeclarationElement.class, "LBL_RenamePackage", oldPkgName, newPkgName); // NOI18N
        }
    }


}
