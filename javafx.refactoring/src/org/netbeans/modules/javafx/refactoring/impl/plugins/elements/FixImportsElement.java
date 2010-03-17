/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.impl.plugins.elements;

import java.util.Set;
import org.netbeans.modules.javafx.refactoring.impl.plugins.BaseRefactoringElementImplementation;
import org.netbeans.modules.javafx.refactoring.transformations.Transformation;
import org.netbeans.modules.refactoring.api.RefactoringSession;
import org.openide.filesystems.FileObject;
import org.openide.util.NbBundle;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
abstract public class FixImportsElement extends BaseRefactoringElementImplementation {

    public FixImportsElement(FileObject srcFO, RefactoringSession session, boolean shouldBackup) {
        super(srcFO, session, shouldBackup);
    }

    public FixImportsElement(FileObject srcFO, RefactoringSession session) {
        super(srcFO, session);
    }

    @Override
    final protected String getRefactoringText() {
        return NbBundle.getMessage(FixImportsElement.class, "LBL_FixImports"); // NOI18N
    }
}
