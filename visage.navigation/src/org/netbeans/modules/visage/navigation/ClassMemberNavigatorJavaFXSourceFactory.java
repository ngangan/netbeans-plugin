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

import java.util.Collections;
import java.util.List;
import org.netbeans.api.visage.source.CancellableTask;
import org.netbeans.api.visage.source.CompilationInfo;
import org.netbeans.api.visage.source.VisageSource.Phase;
import org.netbeans.api.visage.source.VisageSource.Priority;
import org.netbeans.api.visage.source.support.LookupBasedVisageSourceTaskFactory;
import org.openide.filesystems.FileObject;
import org.openide.util.Lookup;

/**
 *
 * @author Jan Lahoda, Petr Hrebejk
 * @author Anton Chechel - visage modifications
 */
public class ClassMemberNavigatorVisageSourceFactory extends LookupBasedVisageSourceTaskFactory {
    
    private static ClassMemberNavigatorVisageSourceFactory instance;

    private static final CancellableTask<CompilationInfo> EMPTY_TASK = new CancellableTask<CompilationInfo>() {
        public void cancel() {}
        public void run(CompilationInfo parameter) throws Exception {}
    };
    
    private ClassMemberPanelUI ui;
    
    static {
        instance = Lookup.getDefault().lookup(ClassMemberNavigatorVisageSourceFactory.class);
    }

    static ClassMemberNavigatorVisageSourceFactory getInstance() {
        return instance;
    }

    public ClassMemberNavigatorVisageSourceFactory() {
//        super(Phase.ELEMENTS_RESOLVED, Priority.LOW, "text/x-java", "application/x-class-file");
        super(Phase.ANALYZED, Priority.LOW, "text/x-fx"); // NOI18N
    }

    @Override
    protected CancellableTask<CompilationInfo> createTask(FileObject file) {
        return ui == null ? EMPTY_TASK : ui.getTask();
    }

    @Override
    public List<FileObject> getFileObjects() {
        List<FileObject> result = super.getFileObjects();
        if (result.size() == 1) {
            return result;
        }
        return Collections.emptyList();
    }

    public synchronized void setLookup(Lookup l, ClassMemberPanelUI ui) {
        this.ui = ui;
        super.setLookup(l);
    }

    @Override
    protected void lookupContentChanged() {
        if (ui != null) {
            ui.showWaitNode(); // Creating new task (file changed)
        }
    }
}
