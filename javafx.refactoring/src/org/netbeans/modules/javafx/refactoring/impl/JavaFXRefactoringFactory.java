/*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 *  Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 * 
 *  The contents of this file are subject to the terms of either the GNU
 *  General Public License Version 2 only ("GPL") or the Common
 *  Development and Distribution License("CDDL") (collectively, the
 *  "License"). You may not use this file except in compliance with the
 *  License. You can obtain a copy of the License at
 *  http://www.netbeans.org/cddl-gplv2.html
 *  or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 *  specific language governing permissions and limitations under the
 *  License.  When distributing the software, include this License Header
 *  Notice in each file and include the License file at
 *  nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Sun in the GPL Version 2 section of the License file that
 *  accompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 * 
 *  Contributor(s):
 * 
 *  Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.refactoring.impl;

import org.netbeans.modules.javafx.refactoring.impl.plugins.WhereUsedQueryPlugin;
import org.netbeans.modules.javafx.refactoring.impl.plugins.RenamePackagePlugin;
import org.netbeans.modules.javafx.refactoring.impl.plugins.RenameRefactoringPlugin;
import org.netbeans.api.fileinfo.NonRecursiveFolder;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.impl.plugins.CopyRefactoringPlugin;
import org.netbeans.modules.javafx.refactoring.impl.plugins.MoveRefactoringPlugin;
import org.netbeans.modules.refactoring.api.AbstractRefactoring;
import org.netbeans.modules.refactoring.api.MoveRefactoring;
import org.netbeans.modules.refactoring.api.MultipleCopyRefactoring;
import org.netbeans.modules.refactoring.api.RenameRefactoring;
import org.netbeans.modules.refactoring.api.SingleCopyRefactoring;
import org.netbeans.modules.refactoring.api.WhereUsedQuery;
import org.netbeans.modules.refactoring.spi.RefactoringPlugin;
import org.netbeans.modules.refactoring.spi.RefactoringPluginFactory;
import org.openide.filesystems.FileObject;
import org.openide.util.Lookup;
import org.openide.util.lookup.ServiceProvider;

/**
 *
 * @author Jaroslav Bachorik
 */
@ServiceProvider(service=RefactoringPluginFactory.class)
public class JavaFXRefactoringFactory implements RefactoringPluginFactory {

    public RefactoringPlugin createInstance(AbstractRefactoring refactoring) {
        // disable javafx refactoring for NB6.8 Beta
//        if (!Boolean.getBoolean("javafx.refactoring")) return null;

        Lookup look = refactoring.getRefactoringSource();
        FileObject file = look.lookup(FileObject.class);
        NonRecursiveFolder folder = look.lookup(NonRecursiveFolder.class);
        ElementLocation location = look.lookup(ElementLocation.class);

        if (refactoring instanceof WhereUsedQuery) {
            if (location == null ) return null;
            return new WhereUsedQueryPlugin((WhereUsedQuery)refactoring);
        }

        if (refactoring instanceof RenameRefactoring) {
            if ((location !=null && location.getStartPosition() != 0) || (location == null && ((file!=null) && SourceUtils.isJavaFXFile(file)))) {
                //rename javafx file, class, method etc..
                return new RenameRefactoringPlugin((RenameRefactoring)refactoring);
            } else if (file!=null && SourceUtils.isOnSourceClasspath(file) && file.isFolder()) {
                //rename folder
                return new RenamePackagePlugin((RenameRefactoring)refactoring);
            } else if (folder!=null && SourceUtils.isOnSourceClasspath(folder.getFolder())) {
                //rename package
                return new RenamePackagePlugin((RenameRefactoring)refactoring);
            }
        }

        if (refactoring instanceof MoveRefactoring) {
            if (checkMove(refactoring.getRefactoringSource())) {
                return new MoveRefactoringPlugin((MoveRefactoring) refactoring);
            }
        }

        if (refactoring instanceof SingleCopyRefactoring  || refactoring instanceof MultipleCopyRefactoring) {
            if (checkCopy(refactoring.getRefactoringSource())) {
                return new CopyRefactoringPlugin(refactoring);
            }
        }

        return null;
    }

    private boolean checkMove(Lookup refactoringSource) {
        for (FileObject f:refactoringSource.lookupAll(FileObject.class)) {
            if (SourceUtils.isJavaFXFile(f)) {
                return true;
            }
            if (f.isFolder()) {
                return true;
            }
        }
        return false;
    }

    private boolean checkCopy(Lookup object) {
        FileObject f=object.lookup(FileObject.class);
        if (f!=null && SourceUtils.isJavaFXFile(f))
            return true;
        return false;
    }

}
