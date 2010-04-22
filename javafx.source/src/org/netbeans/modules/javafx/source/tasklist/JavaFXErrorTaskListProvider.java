/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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
package org.netbeans.modules.javafx.source.tasklist;

import com.sun.tools.mjavac.util.JCDiagnostic;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Logger;
import javax.tools.Diagnostic;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.modules.parsing.api.indexing.IndexingManager;
import org.netbeans.modules.parsing.spi.indexing.ErrorsCache;
import org.netbeans.spi.tasklist.PushTaskScanner;
import org.netbeans.spi.tasklist.Task;
import org.netbeans.spi.tasklist.TaskScanningScope;
import org.openide.filesystems.FileChangeAdapter;
import org.openide.filesystems.FileChangeListener;
import org.openide.filesystems.FileEvent;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.URLMapper;
import org.openide.util.Exceptions;
import org.openide.util.NbBundle;
import org.openide.util.RequestProcessor;

/**
 *
 * @author Karol Harezlak
 */
// NEVER CHANGE NAME OF THIS CLASS (JavaFXErrorTaskListProvider)! binded with tasklist.ui/src/org/netbeans/modules/tasklist/filter/TypesFilter.java
public class JavaFXErrorTaskListProvider extends PushTaskScanner {

    private static final Logger LOG = Logger.getAnonymousLogger(); //NOI18N
    private static final String TASK_LIST_NAME = NbBundle.getMessage(JavaFXErrorTaskListProvider.class, "LABEL_TL_JAVAFX_ISSUES"); //NOI18N
    private static final String FX_EXT = "fx"; //NOI18N
    private final Map<FileObject, FileChangeListener> projectDirs;
    private final Map<FileObject, RequestProcessor.Task> waitingTasks;
    private final Map<FileObject, Future<Void>> scannerTasks;
    private AtomicBoolean active = new AtomicBoolean();
    private RequestProcessor.Task majorTask;
    private RequestProcessor processor;

    private JavaFXErrorTaskListProvider() {
        super(TASK_LIST_NAME, TASK_LIST_NAME, TASK_LIST_NAME);
        projectDirs = Collections.synchronizedMap(new HashMap<FileObject, FileChangeListener>());
        waitingTasks = Collections.synchronizedMap(new HashMap<FileObject, RequestProcessor.Task>());
        scannerTasks = Collections.synchronizedMap(new HashMap<FileObject, Future<Void>>());
    }

    public static final PushTaskScanner create() {
        return new JavaFXErrorTaskListProvider();
    }

    @Override
    public void setScope(TaskScanningScope taskScanningScope, final Callback callback) {
        //LOG.info("(1)Callback:  " + callback); //NOI18N
        if (callback == null) {
            active.set(false);
            if (majorTask != null) {
                majorTask.cancel();
                //LOG.info("(2)Major task has been CANCELED " + majorTask.hashCode()); //NOI18N
                majorTask = null;
            }
            for (FileObject fileObject : projectDirs.keySet()) {
                fileObject.removeRecursiveListener(projectDirs.get(fileObject));
            }
            for (RequestProcessor.Task task : waitingTasks.values()) {
                task.cancel();
                //LOG.info("(3)Waiting task has been CANCELED " + task); //NOI18N
            }
            for (Future<Void> future : scannerTasks.values()) {
                future.cancel(true);
                //LOG.info("(4)Scanner Task has been CANCELED " + future); //NOI18N
            }
            projectDirs.clear();
            waitingTasks.clear();
            scannerTasks.clear();
            if (processor != null) {
                processor.stop();
                processor.shutdown();
            }
            return;
        }
        active.set(true);
        processor = new RequestProcessor("Error Task List Processor"); //NOI18N
        Iterator<FileObject> iterator = taskScanningScope.iterator();
        Collection<FileObject> fileObjects = new HashSet<FileObject>();
        while (iterator.hasNext()) {
            FileObject fileObject = iterator.next();
            if (!fileObject.getExt().equals(FX_EXT)) {
                continue;
            }
            callback.setTasks(fileObject, Collections.EMPTY_LIST);
            fileObjects.add(fileObject);
            final FileObject projectDir = FileOwnerQuery.getOwner(fileObject).getProjectDirectory();

            if (!projectDirs.keySet().contains(projectDir)) {
                FileChangeListener listener = new FileChangeAdapter() {

                    @Override
                    public void fileDataCreated(FileEvent fe) {
                        if (fe.getFile().getExt().equals(FX_EXT) && !active.get()) {
                            //LOG.info("(6)File created: " + fe.getFile().getName());  //NOI18N
                            //LOG.info("(7)Update task created from fileDataCreated"); //NOI18N
                            updateTask(fe.getFile(), callback, 4000);
                        }
                        super.fileDataCreated(fe);
                    }

                    @Override
                    public void fileFolderCreated(FileEvent fe) {
                        if (!active.get()) {
                            return;
                        }
                        for (FileObject child : fe.getFile().getChildren()) {
                            if (child.getExt().equals(FX_EXT)) {
                                //LOG.info("(8)Folder created and update: " + fe.getFile().getName());  //NOI18N
                                //LOG.info("(9)Update task created from fileFolderCreated"); //NOI18N
                                updateTask(child, callback, 4000);
                            }
                        }
                        super.fileFolderCreated(fe);
                    }

                    @Override
                    public synchronized void fileChanged(FileEvent fe) {
                        if (fe.getFile().getExt().equals(FX_EXT)) {
                            if (!active.get()) {
                                return;
                            }
                            //LOG.info("(10)File changed: " + fe.getFile().getName());  //NOI18N
                            //callback.setTasks(fe.getFile(), Collections.EMPTY_LIST);
                            //LOG.info("(11)Update task created from fileChanged"); //NOI18N
                            updateTask(fe.getFile(), callback, 4000);
                        }
                        super.fileChanged(fe);
                    }

                    @Override
                    public synchronized void fileDeleted(FileEvent fe) {
                        if (!active.get()) {
                            return;
                        }
                        if (fe.getFile() == projectDir) {
                            fe.getFile().removeRecursiveListener(this);
                        } else if (fe.getFile().getExt().equals(FX_EXT)) {
                            //LOG.info("(12)File removed: " + fe.getFile().getName());  //NOI18N
                            cancelRemoveTasks(fe.getFile());
                            callback.setTasks(fe.getFile(), Collections.EMPTY_LIST);
                        }
                        super.fileDeleted(fe);
                    }
                };
                projectDirs.put(projectDir, listener);
                projectDir.addRecursiveListener(listener);
            }
        }
        if (!fileObjects.isEmpty()) {
            updateTasks(fileObjects, callback, 5000);
        }
    }

    private synchronized void createTask(FileObject fileObject, Callback callback) {
        if (!fileObject.isValid() || !ErrorsCache.isInError(fileObject, false)) {
            callback.setTasks(fileObject, Collections.EMPTY_LIST);
            return;
        }
        JavaFXSource jfxs = JavaFXSource.forFileObject(fileObject);
        if (jfxs == null) {
            return;
        }
        try {
            //LOG.info("CreateTask Thread " + Thread.currentThread().getId());
            scannerTasks.put(fileObject, jfxs.runWhenScanFinished(new ScannerTask(callback), true));
            scannerTasks.remove(fileObject);
            //LOG.info("(15)Scanning task created, file: " + fileObject); //NOI18N
        } catch (IOException ex) {
            Exceptions.printStackTrace(ex);
            return;
        }
    }

    private void cancelRemoveTasks(FileObject fileObject) {
        RequestProcessor.Task waitingTask = waitingTasks.get(fileObject);
        if (waitingTask != null) {
            waitingTask.cancel();
            //LOG.info("(16)Waiting task has been CANCELED and REMOVED, file " + fileObject); //NOI18N
        }
        waitingTasks.remove(fileObject);
        Future<Void> future = scannerTasks.get(fileObject);
        if (future != null) {
            future.cancel(true);
            //LOG.info("(17)Task Scaner has been CANCELED and REMOVED " + future + " for file " + fileObject); //NOI18N
        }
        scannerTasks.remove(fileObject);
    }

    private void updateTask(final FileObject fileObject, final Callback callback, final int delay) {
        cancelRemoveTasks(fileObject);
        RequestProcessor.Task task = processor.create(new Runnable() {

            public void run() {
                if (!active.get() || !fileObject.isValid() || !ErrorsCache.isInError(fileObject, false)) {
                    callback.setTasks(fileObject, Collections.EMPTY_LIST);
                    return;
                }
                if (IndexingManager.getDefault().isIndexing()) {
                    final RequestProcessor.Task task = waitingTasks.get(fileObject);
                    if (task != null) {
                        task.schedule(delay);
                    }
                    //LOG.info("(18)Task " + fileObject.getName() + " has to wait " + delay + " ms"); //NOI18N
                } else {
                    callback.started();
                    createTask(fileObject, callback);
                    callback.finished();
                }
            }
        });
        waitingTasks.put(fileObject, task);
        task.setPriority(Thread.MIN_PRIORITY);
        task.schedule(delay);
    }

    private void updateTasks(final Collection<FileObject> fileObjects, final Callback callback, final int delay) {
        majorTask = processor.create(new Runnable() {

            public void run() {
                if (!active.get()) {
                    return;
                }
                if (IndexingManager.getDefault().isIndexing()) {
                    if (majorTask != null) {
                        majorTask.schedule(delay);
                        //LOG.info("Major task " + majorTask.hashCode() + " has to wait " + delay + " ms"); //NOI18N
                    } else {
                        return;
                    }
                } else {
                    callback.started();
                    for (FileObject fileObject : fileObjects) {
                        if (majorTask == null) {
                            return;
                        }
                        createTask(fileObject, callback);
                    }
                    callback.finished();
                    majorTask = null;
                }
            }
        });
        if (majorTask == null) {
            return;
        }
        majorTask.setPriority(Thread.MIN_PRIORITY);
        majorTask.schedule(delay);
        //LOG.info("Major task has been CREATED " + majorTask.hashCode()); //NOI18N
    }

    private class ScannerTask implements org.netbeans.api.javafx.source.Task<CompilationController> {

        private final Callback callback;

        public ScannerTask(Callback callback) {
            this.callback = callback;
        }

        public void run(final CompilationController compilationController) throws Exception {
            if (!compilationController.isErrors() || !active.get()) {
                return;
            }
            //LOG.info("(19)Scanning " + compilationController.getFileObject().getName()); //NOI18N
            refreshTasks(compilationController);
        }

        private void refreshTasks(CompilationController compilationController) {
            List<Diagnostic> diagnostics = compilationController.getDiagnostics();
            List<Task> tasks = new ArrayList<Task>();
            FileObject fileObject = compilationController.getFileObject();
            for (Diagnostic diagnostic : diagnostics) {
                if (diagnostic == null || diagnostic.getKind() != Diagnostic.Kind.ERROR) {
                    continue;
                }
                JCDiagnostic jcd = ((JCDiagnostic) diagnostic);

                FileObject sourceErrorFileObject = null;
                try {
                    if (jcd.getSource() == null || jcd.getSource().toUri() == null) {
                        continue;
                    }
                    sourceErrorFileObject = URLMapper.findFileObject(jcd.getSource().toUri().toURL());
                } catch (MalformedURLException ex) {
                    ex.printStackTrace();
                }
                if (sourceErrorFileObject == null || sourceErrorFileObject != compilationController.getFileObject()) {
                    continue;
                }
                Task task = Task.create(fileObject, "nb-tasklist-error", diagnostic.getMessage(Locale.getDefault()), (int) diagnostic.getLineNumber()); //NOI18N
                tasks.add(task);
                //LOG.info("TASK -  FILE: " + compilationController.getFileObject().getName() + " SOURCE " +sourceErrorFileObject.getName());
            }
            callback.setTasks(fileObject, tasks);
            //tasks.clear();
            waitingTasks.remove(compilationController.getFileObject());
            //LOG.info("Scanning - Thread " + Thread.currentThread().getId());
            //LOG.info("(20)Scanning task is FINISHED " + compilationController.getFileObject()); //NOI18N
        }
    }
}
