/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Sun Microsystems, Inc. All rights reserved.
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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
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

package org.netbeans.modules.javafx.refactoring.impl.plugins.elements;

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
        return NbBundle.getMessage(RenameOccurencesElement.class, "LBL_ReplaceOccurences", oldName, newName); // NOI18N
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
        if ((getParentFile() == null) ? (other.getParentFile() != null) : !this.getParentFile().equals(other.getParentFile())) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 5;
        hash = 89 * hash + (this.oldName != null ? this.oldName.hashCode() : 0);
        hash = 89 * hash + (this.newName != null ? this.newName.hashCode() : 0);
        hash = 89 * hash + (this.getParentFile() != null ? this.getParentFile().hashCode() : 0);
        return hash;
    }
}
