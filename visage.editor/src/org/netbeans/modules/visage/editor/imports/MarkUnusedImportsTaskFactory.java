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
 * Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */
package org.netbeans.modules.visage.editor.imports;

import org.netbeans.api.visage.source.CancellableTask;
import org.netbeans.api.visage.source.CompilationInfo;
import org.netbeans.api.visage.source.VisageSource;
import org.netbeans.api.visage.source.support.EditorAwareVisageSourceTaskFactory;
import org.openide.filesystems.FileObject;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @todo documentation
 */
public class MarkUnusedImportsTaskFactory extends EditorAwareVisageSourceTaskFactory {

    public MarkUnusedImportsTaskFactory() {
        super(VisageSource.Phase.ANALYZED, VisageSource.Priority.LOW);
    }

    /**
     * <p>
     * Create task for a given file. This task will be registered into the {@link org.netbeans.api.visage.source.VisageSource}
     * parsing harness.
     * </p><p>
     * Please note that this method should run as quickly as possible.
     * </p>
     * @param file for which file the task should be created.
     * @return created {@link org.netbeans.api.visage.source.CancellableTask}  for a given file.
     */
    protected CancellableTask<CompilationInfo> createTask(FileObject file) {
        return new MarkUnusedImportsTask(file);
    }
}
