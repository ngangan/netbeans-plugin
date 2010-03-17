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
abstract public class RenameOccurencesElement extends BaseRefactoringElementImplementation {
    private String oldName, newName;

    public RenameOccurencesElement(String oldName, String newName, FileObject srcFO, RefactoringSession session) {
        super(srcFO, session);
        this.oldName = oldName;
        this.newName = newName;
    }

    public RenameOccurencesElement(String oldName, String newName, FileObject srcFO, RefactoringSession session, boolean shouldBackup) {
        super(srcFO, session, shouldBackup);
        this.oldName = oldName;
        this.newName = newName;
    }

    @Override
    final protected String getRefactoringText() {
        return NbBundle.getMessage(RenameOccurencesElement.class, "LBL_RenameOccurences", oldName, newName); // NOI18N
    }

    final protected String getOldName() {
        return oldName;
    }

    final protected String getNewName() {
        return newName;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final RenameOccurencesElement other = (RenameOccurencesElement) obj;
        if ((this.oldName == null) ? (other.oldName != null) : !this.oldName.equals(other.oldName)) {
            return false;
        }
        if ((this.newName == null) ? (other.newName != null) : !this.newName.equals(other.newName)) {
            return false;
        }
        if ((getSourceFO() == null) ? (other.getSourceFO() != null) : !this.getSourceFO().equals(other.getSourceFO())) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 5;
        hash = 89 * hash + (this.oldName != null ? this.oldName.hashCode() : 0);
        hash = 89 * hash + (this.newName != null ? this.newName.hashCode() : 0);
        hash = 89 * hash + (this.getSourceFO() != null ? this.getSourceFO().hashCode() : 0);
        return hash;
    }
}
