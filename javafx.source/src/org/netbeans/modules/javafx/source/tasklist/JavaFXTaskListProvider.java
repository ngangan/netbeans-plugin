/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.source.tasklist;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import javax.tools.Diagnostic;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.spi.tasklist.PushTaskScanner;
import org.netbeans.spi.tasklist.Task;
import org.netbeans.spi.tasklist.TaskScanningScope;
import org.openide.filesystems.FileChangeAdapter;
import org.openide.filesystems.FileEvent;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;
import org.openide.util.NbBundle;
import org.openide.util.WeakSet;

/**
 *
 * @author Karol Harezlak
 */
public class JavaFXTaskListProvider extends PushTaskScanner {

    private static final String TASK_LIST_NAME = NbBundle.getMessage(JavaFXTaskListProvider.class, "LABEL_TL_JAVAFX_ISSUES");//NOI18N
    private static final String FX_EXT = "fx"; //NOI18N
    
    private WeakSet<FileObject> projectDirs = new WeakSet<FileObject>();

    private JavaFXTaskListProvider() {
        super(TASK_LIST_NAME, TASK_LIST_NAME, TASK_LIST_NAME);
    }

    public static final PushTaskScanner create() {
        return new JavaFXTaskListProvider();
    }

    @Override
    public void setScope(TaskScanningScope taskScanningScope, final Callback callback) {

        if (callback == null) {
            return;
        }

        Iterator<FileObject> iterator = taskScanningScope.iterator();

        while (iterator.hasNext()) {
            FileObject fileObject = iterator.next();

            if (!fileObject.getExt().equals(FX_EXT)) {
                continue;
            }

            final FileObject projectDir = FileOwnerQuery.getOwner(fileObject).getProjectDirectory();

            if (!projectDirs.contains(projectDir)) {
                projectDir.addRecursiveListener(new FileChangeAdapter() {

                    @Override
                    public void fileDataCreated(FileEvent fe) {
                        if (fe.getFile().getExt().equals(FX_EXT)) {
                            updateTasks(fe.getFile(), callback);
                        }

                        super.fileDataCreated(fe);
                    }

                    @Override
                    public void fileChanged(FileEvent fe) {
                        if (fe.getFile().getExt().equals(FX_EXT)) {
                            updateTasks(fe.getFile(), callback);
                        }

                        super.fileChanged(fe);
                    }

                    @Override
                    public void fileDeleted(FileEvent fe) {
                        if (fe.getFile() == projectDir) {
                            fe.getFile().removeRecursiveListener(this);
                        } else if (fe.getFile().getExt().equals(FX_EXT)) {
                            callback.setTasks(fe.getFile(), Collections.EMPTY_LIST);
                        }

                        super.fileDeleted(fe);
                    }
                });
                
                projectDirs.add(projectDir);
            }

            updateTasks(fileObject, callback);
        }

    }

    private void updateTasks(final FileObject fileObject, final Callback callback) {
        JavaFXSource jfxs = JavaFXSource.forFileObject(fileObject);
        if (jfxs == null) {
            return;
        }
        try {
            jfxs.runWhenScanFinished(new ScannerTask(callback), true);
        } catch (IOException ex) {
            Exceptions.printStackTrace(ex);
        }
    }

    private static class ScannerTask implements org.netbeans.api.javafx.source.Task<CompilationController> {

        private final Callback callback;

        public ScannerTask(Callback callback) {
            this.callback = callback;
        }

        public void run(final CompilationController compilationController) throws Exception {
            callback.setTasks(getTasks(compilationController));
        }

        private List<Task> getTasks(CompilationController compilationController) {
            List<Task> tasks = new ArrayList<Task>();
            for (Diagnostic diagnostic : compilationController.getDiagnostics()) {
                if (diagnostic.getKind() == Diagnostic.Kind.ERROR) {
                    Task task = Task.create(compilationController.getFileObject(), "nb-tasklist-error", diagnostic.getMessage(Locale.getDefault()), (int) diagnostic.getLineNumber()); //NOI18N
                    tasks.add(task);
                }
            }

            return tasks;
        }
    }
}
