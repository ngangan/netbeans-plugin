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
package org.netbeans.modules.javafx.source.tasklist;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.WeakHashMap;
import javax.tools.Diagnostic;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.spi.tasklist.FileTaskScanner;
import org.netbeans.spi.tasklist.Task;
import org.openide.filesystems.FileChangeAdapter;
import org.openide.filesystems.FileEvent;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;
import org.openide.util.NbBundle;

/**
 *
 * @author karol 
 */
//FIXME This line must be added to the org.netbeans.modules.tasklist.filter.TypesFilter to make this scanner works on default "enabledProviders.add("org.netbeans.modules.javafx.source.tasklist.JavaFXTaskListProvider"); //NOI18N"
//TODO It'd be better to use PushTaskScanner then FileTaskScanner
public final class JavaFXTaskListProviderNotUsed extends FileTaskScanner {

    private static final String TASK_LIST_NAME = NbBundle.getMessage(JavaFXTaskListProviderNotUsed.class, "LABEL_TL_JAVAFX_ISSUES");//NOI18N
    //private Callback callback;
    private WeakHashMap<FileObject, List<Task>> tasksMap;
    private WeakHashMap<FileObject, CompilationController> controllersMap;

    public JavaFXTaskListProviderNotUsed() {
        super(TASK_LIST_NAME, TASK_LIST_NAME, null); //NOI18N
    }

    @Override
    public List<? extends Task> scan(final FileObject fileObject) {
        if (!fileObject.getExt().equals("fx")) { //NOI18N
            return Collections.EMPTY_LIST;
        }

        final FileChangeAdapter fileChangeAdapter = new FileChangeAdapter() {

            @Override
            public void fileChanged(FileEvent fe) {
                if (controllersMap.get(fileObject) != null) {
                    updateTasks(controllersMap.get(fileObject), fileObject);
                }
                super.fileChanged(fe);
            }

            @Override
            public void fileDeleted(FileEvent fe) {
                fileObject.removeFileChangeListener(this);
                controllersMap.remove(fileObject);
                tasksMap.remove(fileObject);
                super.fileDeleted(fe);
            }
        };

        //CompilationController compilationController = controllersMap.get(fileObject);
        if (controllersMap.get(fileObject) == null) {
            // FIXME No guarantees that current thread is in AWT! Tasks for in tasksMap may not be properly updated after scan() method is finished
            JavaFXSource jfxs = JavaFXSource.forFileObject(fileObject);
            try {
                jfxs.runUserActionTask(new org.netbeans.api.javafx.source.Task<CompilationController>() {

                    public void run(CompilationController compilationController) throws Exception {
                        controllersMap.put(fileObject, compilationController);
                        fileObject.addFileChangeListener(fileChangeAdapter);
                    }
                }, true);
            } catch (IOException ex) {
                Exceptions.printStackTrace(ex);
            }
        }
        if (controllersMap.get(fileObject) == null) {
            return Collections.EMPTY_LIST;
        }

        //updateTasks(compilationController, fileObject);

        return tasksMap.get(fileObject);
    }

    public static JavaFXTaskListProviderNotUsed create() {
        return new JavaFXTaskListProviderNotUsed();
    }

    @Override
    public void attach(Callback callback) {
        //this.callback = callback;
        if (callback == null) {
            return;
        }
        if (tasksMap == null) {
            tasksMap = new WeakHashMap<FileObject, List<Task>>();
        }
        if (controllersMap == null) {
            controllersMap = new WeakHashMap<FileObject, CompilationController>();
        }
    }

    private void updateTasks(CompilationController compilationController, FileObject fileObject) {
        if (compilationController == null) {
            return;
        }
        if (tasksMap.get(fileObject) == null) {
            tasksMap.put(fileObject, new ArrayList<Task>());
        }
        List<Task> localTasks = new ArrayList<Task>();
        for (Diagnostic diagnostic : compilationController.getDiagnostics()) {
            if (diagnostic.getKind() == Diagnostic.Kind.ERROR) {
                Task task = Task.create(fileObject, "nb-tasklist-error", diagnostic.getMessage(Locale.getDefault()), (int) diagnostic.getLineNumber()); //NOI18N
                localTasks.add(task);
            }
        }
        boolean refresh = false;
        if (localTasks.size() != tasksMap.get(fileObject).size() || !localTasks.containsAll(tasksMap.get(fileObject))) {
            refresh = true;
        }
        if (refresh) {
            tasksMap.put(fileObject, localTasks);
            //callback.refresh(fileObject);
        }
    }
}


