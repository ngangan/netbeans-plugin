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

package org.netbeans.api.javafx.source;

import java.net.URL;
import java.util.HashSet;
import java.util.Set;
import org.netbeans.modules.javafx.source.classpath.SourceFileObject;
import com.sun.javafx.api.tree.UnitTree;
import com.sun.tools.javac.parser.DocCommentScanner;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javafx.api.JavafxcTaskImpl;
import com.sun.tools.javafx.api.JavafxcTool;
import com.sun.tools.javafxdoc.JavafxdocEnter;
import com.sun.tools.javafxdoc.Messager;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.tools.DiagnosticListener;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.modules.javafx.source.JavadocEnv;
import org.netbeans.modules.javafx.source.tasklist.FXErrorAnnotator;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStateInvalidException;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;
import org.openide.util.Exceptions;
import org.openide.filesystems.FileChangeAdapter;
import org.openide.filesystems.FileChangeListener;
import org.openide.filesystems.FileEvent;
import org.openide.filesystems.FileRenameEvent;
import org.openide.util.RequestProcessor;
import org.openide.util.WeakListeners;
import org.netbeans.modules.javafx.source.scheduler.DocListener;
import org.netbeans.modules.javafx.source.scheduler.DataObjectListener;
import org.netbeans.modules.javafx.source.scheduler.CompilationJob;
import org.netbeans.modules.javafx.source.scheduler.EditorRegistryListener;
import org.netbeans.modules.javafx.source.scheduler.Request;


/**
 * A class representing JavaFX source.
 * 
 * @author nenik
 * @author David Strupl
 */
public final class JavaFXSource {

    public static enum Phase {
        MODIFIED,
        PARSED,
        ANALYZED,
        UP_TO_DATE,
        CODE_GENERATED;
        
        public boolean lessThan(Phase p) {
            return compareTo(p) < 0;
        }
    };
    public static enum Priority {
        MAX,
        HIGH,
        ABOVE_NORMAL,
        NORMAL,
        BELOW_NORMAL,
        LOW,
        MIN
    };

    private static final PrintWriter DEV_NULL = new PrintWriter(new DevNullWriter(), false);

    // flags:
    public static final int INVALID = 1;
    public static final int CHANGE_EXPECTED = INVALID<<1;
    public static final int RESCHEDULE_FINISHED_TASKS = CHANGE_EXPECTED<<1;
    public static final int UPDATE_INDEX = RESCHEDULE_FINISHED_TASKS<<1;
    public static final int IS_CLASS_FILE = UPDATE_INDEX<<1;
    
    private static Map<FileObject, Reference<JavaFXSource>> file2Source = new WeakHashMap<FileObject, Reference<JavaFXSource>>();
    private static final RequestProcessor RP = new RequestProcessor ("JavaFXSource-event-collector",1);       //NOI18N
    static final Logger LOGGER = Logger.getLogger(JavaFXSource.class.getName());
    
    private static final int REPARSE_DELAY = 500;
    private static final Pattern excludedTasks;
    private static final Pattern includedTasks;

    // all the following should be private:
    public int flags = 0;
    public volatile boolean k24;
//    public CompilationController currentInfo;
    public CompilationInfoImpl currentInfo;

    public final Collection<? extends FileObject> files;
    public final int reparseDelay;
    
    private final ClasspathInfo cpInfo;

    private final AtomicReference<Request> rst = new AtomicReference<Request> ();
    private final FileChangeListener fileChangeListener;
    
    private DocListener listener;
    private DataObjectListener dataObjectListener;
    
    public final RequestProcessor.Task resetTask = RP.create(new Runnable() {
        public void run() {
            resetStateImpl();
        }
    });
    
    static {
        // Start listening on the editor registry:
        EditorRegistryListener.singleton.toString();
        // Start the factories ...
        JavaFXSourceTaskFactoryManager.register();
//        Init the maps
//        phase2Message.put (Phase.PARSED,"Parsed");                              //NOI18N
//        phase2Message.put (Phase.ELEMENTS_RESOLVED,"Signatures Attributed");    //NOI18N
//        phase2Message.put (Phase.RESOLVED, "Attributed");                       //NOI18N
        
        //Initialize the excludedTasks
        Pattern _excludedTasks = null;
        try {
            String excludedValue= System.getProperty("org.netbeans.api.java.source.JavaFXSource.excludedTasks");      //NOI18N
            if (excludedValue != null) {
                _excludedTasks = Pattern.compile(excludedValue);
            }
        } catch (PatternSyntaxException e) {
            e.printStackTrace();
        }
        excludedTasks = _excludedTasks;
        Pattern _includedTasks = null;
        try {
            String includedValue= System.getProperty("org.netbeans.api.java.source.JavaFXSource.includedTasks");      //NOI18N
            if (includedValue != null) {
                _includedTasks = Pattern.compile(includedValue);
            }
        } catch (PatternSyntaxException e) {
            e.printStackTrace();
        }
        includedTasks = _includedTasks;
    }  

    JavafxcTaskImpl createJavafxcTask(DiagnosticListener<JavaFileObject> diagnosticListener) {
        JavafxcTool tool = JavafxcTool.create();
        JavaFileManager fileManager = cpInfo.getFileManager(tool);
        JavaFileObject jfo = (JavaFileObject) SourceFileObject.create(files.iterator().next(), null); // XXX

        if (LOGGER.isLoggable(Level.FINEST)) {
            try {
                String sourceTxt = jfo.getCharContent(true).toString();
                LOGGER.finest("\n======================================================\n"); // NOI18N
                LOGGER.finest(sourceTxt);
                LOGGER.finest("\n------------------------------------------------------\n"); // NOI18N
            } catch (IOException ex) {
                LOGGER.log(Level.FINEST, "Cannot get file content.", ex); // NOI18N
            }
        }

        List<String> options = new ArrayList<String>();
        //options.add("-Xjcov"); //NOI18N, Make the compiler store end positions
        options.add("-XDdisableStringFolding"); //NOI18N
        
        JavafxcTaskImpl task = (JavafxcTaskImpl)tool.getTask(null, fileManager, diagnosticListener, options, Collections.singleton(jfo));
        Context context = task.getContext();
//        JavafxdocClassReader.preRegister(context);
        Messager.preRegister(context, null, DEV_NULL, DEV_NULL, DEV_NULL);
        JavafxdocEnter.preRegister(context);
        JavadocEnv.preRegister(context, cpInfo);
        DocCommentScanner.Factory.preRegister(context);
        
        return task;
  }

    public Phase moveToPhase(Phase phase, CompilationInfoImpl cc, boolean cancellable) throws IOException {
        FileObject file = getFileObject();
        
        if (cc.phase.lessThan(Phase.PARSED) && !phase.lessThan(Phase.PARSED)) {
            if (cancellable && CompilationJob.currentRequest.isCanceled()) {
                //Keep the currentPhase unchanged, it may happen that an userActionTask
                //runnig after the phace completion task may still use it.
                return cc.phase;
            }
            if (LOGGER.isLoggable(Level.FINE)) LOGGER.fine("Starting to parse " + file.getNameExt()); // NOI18N
            long start = System.currentTimeMillis();
            Iterable<? extends UnitTree> trees = null;
            try {
                trees = cc.getJavafxcTask().parse();
            } catch (RuntimeException parserError) {
                LOGGER.log(Level.FINE, "Error in parser", parserError); // NOI18N
                return cc.phase;
            }
//                new JavaFileObject[] {currentInfo.jfo});
            Iterator<? extends UnitTree> it = trees.iterator();
            assert it.hasNext();
            UnitTree unit = it.next();
            cc.setCompilationUnit(unit);
            assert !it.hasNext();
            cc.setPhase(Phase.PARSED);

            long end = System.currentTimeMillis();
            Logger.getLogger("TIMER").log(Level.FINE, "Compilation Unit", new Object[] {file, unit}); // log the instance // NOI18N
            Logger.getLogger("TIMER").log(Level.FINE, "Parsed", new Object[] {file, end-start}); // NOI18N
            if (LOGGER.isLoggable(Level.FINE)) LOGGER.fine("Finished parsing " + file.getNameExt()); // NOI18N
        }

        if (cc.phase == Phase.PARSED && !phase.lessThan(Phase.ANALYZED)) {
            if (cancellable && CompilationJob.currentRequest.isCanceled()) {
                return Phase.MODIFIED;
            }
            if (LOGGER.isLoggable(Level.FINE)) LOGGER.fine("Starting to analyze " + file.getNameExt()); // NOI18N
            long start = System.currentTimeMillis();
            try {
                cc.getJavafxcTask().analyze();
            } catch (RuntimeException analyzerError) {
                LOGGER.log(Level.FINE, "Error in analyzer", analyzerError); // NOI18N
                return cc.phase;
            } catch (ThreadDeath td) {
                throw td;
            } catch (Throwable assErr) {
                LOGGER.log(Level.FINE, "Error in analyzer", assErr); // NOI18N
                return cc.phase;
            }
            cc.setPhase(Phase.ANALYZED);

            //update error annotations in projects tree

            Set<URL> urls = new HashSet<URL>();

            try {
                FileObject f = file;
                urls.add(f.getURL());
                Project proj = FileOwnerQuery.getOwner(file);
                if (proj != null) {
                    do {
                        f = f.getParent();
                        if (f != null) {
                            urls.add(f.getURL());
                        } else {
                            break;
                        }
                    } while (proj.getProjectDirectory() != f);
                }
            } catch (FileStateInvalidException ex) {
                Exceptions.printStackTrace(ex);
            }
            FXErrorAnnotator.getAnnotator().updateInError(urls);

            long end = System.currentTimeMillis();
            Logger.getLogger("TIMER").log(Level.FINE, "Analyzed", new Object[] {file, end-start}); // NOI18N
            if (LOGGER.isLoggable(Level.FINE)) LOGGER.fine("Finished to analyze " + file.getNameExt()); // NOI18N
        }
        
        if (cc.phase == Phase.ANALYZED && !phase.lessThan(Phase.UP_TO_DATE)) {
            cc.setPhase(Phase.UP_TO_DATE);
        }
        
        if (cc.phase == Phase.UP_TO_DATE && !phase.lessThan(Phase.CODE_GENERATED)) {
            if (cancellable && CompilationJob.currentRequest.isCanceled()) {
                return Phase.MODIFIED;
            }
            if (!cc.isErrors()) {
                long start = System.currentTimeMillis();
                Iterable <? extends JavaFileObject> bytes = null;
                try {
                    bytes = cc.getJavafxcTask().generate();
                } catch (RuntimeException generateError) {
                    LOGGER.log(Level.FINE, "Error in generate", generateError); // NOI18N
                    return cc.phase;
                }
                cc.setClassBytes(bytes);
                cc.setPhase(Phase.CODE_GENERATED);
                long end = System.currentTimeMillis();
                Logger.getLogger("TIMER").log(Level.FINE, "Analyzed", new Object[] {file, end-start}); // NOI18N
            } else {
                cc.setClassBytes(null);
                cc.setPhase(Phase.CODE_GENERATED);
            }
            
        }
        
        return phase;
    }
    
    public FileObject getFileObject() {
        return files.iterator().next();
    }
    
    private JavaFXSource(ClasspathInfo cpInfo, Collection<? extends FileObject> files) throws IOException {
        this.cpInfo = cpInfo;
        this.files = Collections.unmodifiableList(new ArrayList<FileObject>(files));   //Create a defensive copy, prevent modification
        
        this.reparseDelay = REPARSE_DELAY;
        this.fileChangeListener = new FileChangeListenerImpl ();
        boolean multipleSources = this.files.size() > 1, filterAssigned = false;
        for (Iterator<? extends FileObject> it = this.files.iterator(); it.hasNext();) {
            FileObject file = it.next();
            try {
                Logger.getLogger("TIMER").log(Level.FINE, "JavaFXSource", // NOI18N
                    new Object[] {file, this});
                if (!multipleSources) {
                    file.addFileChangeListener(FileUtil.weakFileChangeListener(this.fileChangeListener,file));
                    assignDocumentListener(DataObject.find(file));
                    dataObjectListener = new DataObjectListener(file,this);                                        
                }
            } catch (DataObjectNotFoundException donf) {
                if (multipleSources) {
                    LOGGER.warning("Ignoring non existent file: " + FileUtil.getFileDisplayName(file));     //NOI18N
                    it.remove();
                }
                else {
                    throw donf;
                }
            }
        }
        this.cpInfo.addChangeListener(WeakListeners.change(listener, this.cpInfo));
        if (LOGGER.isLoggable(Level.FINE)) {
            LOGGER.fine("Created JavaFXSource for " + files); // NOI18N
        }
    }
    
    
    /**
     * Returns a {@link JavaFXSource} instance associated with given
     * {@link org.openide.filesystems.FileObject}.
     * It returns null if the file doesn't represent JavaFX source file.
     * 
     * @param fileObject for which the {@link JavaFXSource} should be found/created.
     * @return {@link JavaFXSource} or null
     * @throws {@link IllegalArgumentException} if fileObject is null
     */
    public static JavaFXSource forFileObject(FileObject fileObject) throws IllegalArgumentException {
        if (fileObject == null) {
            throw new IllegalArgumentException ("fileObject == null");  //NOI18N
        }
        if (!fileObject.isValid()) {
            return null;
        }

        try {
            if (   fileObject.getFileSystem().isDefault()
                && fileObject.getAttribute("javax.script.ScriptEngine") != null // NOI18N
                && fileObject.getAttribute("template") == Boolean.TRUE) { // NOI18N
                return null;
            }
            DataObject od = DataObject.find(fileObject);
            
            EditorCookie ec = od.getLookup().lookup(EditorCookie.class);           
        } catch (FileStateInvalidException ex) {
            LOGGER.log(Level.FINE, null, ex);
            return null;
        } catch (DataObjectNotFoundException ex) {
            LOGGER.log(Level.FINE, null, ex);
            return null;
        }
        
        Reference<JavaFXSource> ref = file2Source.get(fileObject);
        JavaFXSource source = ref != null ? ref.get() : null;
        if (source == null) {
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.fine("Not found in cache: " + fileObject); // NOI18N
            }

            if (!"text/x-fx".equals(FileUtil.getMIMEType(fileObject)) && !"fx".equals(fileObject.getExt())) {  //NOI18N
                return null;
            }
            source = create(ClasspathInfo.create(fileObject), Collections.singletonList(fileObject));
            file2Source.put(fileObject, new WeakReference<JavaFXSource>(source));
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.fine("Put into the cache: " + fileObject); // NOI18N
            }
        }
        return source;
    }

    public static JavaFXSource create(final ClasspathInfo cpInfo, final Collection<? extends FileObject> files) throws IllegalArgumentException {
        try {
            return new JavaFXSource(cpInfo, files);
        } catch (DataObjectNotFoundException donf) {
            Logger.getLogger("global").warning("Ignoring non existent file: " + FileUtil.getFileDisplayName(donf.getFileObject()));     //NOI18N
        } catch (IOException ex) {            
            Exceptions.printStackTrace(ex);
        }        
        return null;
    }

    public void runUserActionTask( final Task<? super CompilationController> task, final boolean shared) throws IOException {
        if (task == null) {
            throw new IllegalArgumentException ("Task cannot be null");     //NOI18N
        }

        // XXX: check access an threading
        
        if (this.files.size()<=1) {                        
            // XXX: cancel pending tasks


            // XXX: validity check, reuse
            CompilationInfoImpl currentInfo = null;
            boolean jsInvalid;
            synchronized (this) {                        
                jsInvalid = this.currentInfo == null || (this.flags & INVALID)!=0;
                currentInfo = this.currentInfo;
                if (!shared) {
                    this.flags|=INVALID;
                }                        
            }                    
            if (jsInvalid) {
                currentInfo = createCurrentInfo(this, null);
            }
            if (shared) {
                synchronized (this) {                        
                    if (this.currentInfo == null || (this.flags & INVALID) != 0) {
                        this.currentInfo = currentInfo;
                        this.flags&=~INVALID;
                    } else {
                        currentInfo = this.currentInfo;
                    }
                }
            }
            assert currentInfo != null;
//                    if (shared) {
//                        if (!infoStack.isEmpty()) {
//                            currentInfo = infoStack.peek();
//                        }
//                    } else {
//                        infoStack.push (currentInfo);
//                    }
            try {
                final CompilationController clientController = new CompilationController (currentInfo);
                try {
                    task.run (clientController);
                } catch (Exception ex) {
                  // XXX better handling
                  Exceptions.printStackTrace(ex);
                } finally {
                    if (shared) {
                        clientController.invalidate();
                    }
                }
            } finally {
//                if (!shared) {
//                    infoStack.pop ();
//                }
            }
        }
    }

    public static CompilationInfoImpl createCurrentInfo (final JavaFXSource js, final String javafxc) throws IOException {
        CompilationInfoImpl impl = new CompilationInfoImpl(js);
//        CompilationController info = new CompilationController(impl);//js, binding, javac);
        return impl;
    }

    /** Adds a task to given compilation phase. The tasks will run sequentially by
     * priority after given phase is reached.
     * @see CancellableTask for information about implementation requirements 
     * @task The task to run.
     * @phase In which phase should the task run
     * @priority Priority of the task.
     */
    void addPhaseCompletionTask( CancellableTask<CompilationInfo> task, Phase phase, Priority priority ) throws IOException {
        if (task == null) {
            throw new IllegalArgumentException ("Task cannot be null");     //NOI18N
        }
        if (phase == null || phase == Phase.MODIFIED) { 
            throw new IllegalArgumentException (String.format("The %s is not a legal value of phase",phase));   //NOI18N
        }
        if (priority == null) {
            throw new IllegalArgumentException ("The priority cannot be null");    //NOI18N
        }
        final String taskClassName = task.getClass().getName();
        if (excludedTasks != null && excludedTasks.matcher(taskClassName).matches()) {
            if (includedTasks == null || !includedTasks.matcher(taskClassName).matches())
            return;
        }        
        handleAddRequest (new Request (task, this, phase, priority, true));
    }
    
    /** Removes the task from the phase queue.
     * @task The task to remove.
     */
    void removePhaseCompletionTask( CancellableTask<CompilationInfo> task ) {
        final String taskClassName = task.getClass().getName();
        if (excludedTasks != null && excludedTasks.matcher(taskClassName).matches()) {
            if (includedTasks == null || !includedTasks.matcher(taskClassName).matches()) {
                return;
            }
        }
        synchronized (CompilationJob.INTERNAL_LOCK) {
            CompilationJob.toRemove.add (task);
            Collection<Request> rqs = CompilationJob.finishedRequests.get(this);
            if (rqs != null) {
                for (Iterator<Request> it = rqs.iterator(); it.hasNext(); ) {
                    Request rq = it.next();
                    if (rq.task == task) {
                        it.remove();
                    }
                }
            }
        }
    }
    
    /**Rerun the task in case it was already run. Does nothing if the task was not already run.
     *
     * @task to reschedule
     */
    void rescheduleTask(CancellableTask<CompilationInfo> task) {
        synchronized (CompilationJob.INTERNAL_LOCK) {
            Request request = CompilationJob.currentRequest.getTaskToCancel (task);
            if ( request == null) {                
out:            for (Iterator<Collection<Request>> it = CompilationJob.finishedRequests.values().iterator(); it.hasNext();) {
                    Collection<Request> cr = it.next ();
                    for (Iterator<Request> it2 = cr.iterator(); it2.hasNext();) {
                        Request fr = it2.next();
                        if (task == fr.task) {
                            it2.remove();
                            CompilationJob.requests.add(fr);
                            if (cr.size()==0) {
                                it.remove();
                            }
                            break out;
                        }
                    }
                }
            }
            else {
                CompilationJob.currentRequest.cancelCompleted(request);
            }
        }        
    }
    
    /**
     * Not synchronized, only sets the atomic state and clears the listeners
     *
     */
    private void resetStateImpl() {
        if (!k24) {
            Request r = rst.getAndSet(null);
            CompilationJob.currentRequest.cancelCompleted(r);
            synchronized (CompilationJob.INTERNAL_LOCK) {
                boolean reschedule, updateIndex;
                synchronized (this) {
                    reschedule = (this.flags & RESCHEDULE_FINISHED_TASKS) != 0;
                    updateIndex = (this.flags & UPDATE_INDEX) != 0;
                    this.flags&=~(RESCHEDULE_FINISHED_TASKS|CHANGE_EXPECTED|UPDATE_INDEX);
                }            
                Collection<Request> cr;            
                if (reschedule) {                
                    if ((cr=CompilationJob.finishedRequests.remove(this)) != null && cr.size()>0)  {
                        CompilationJob.requests.addAll(cr);
                    }
                }
                if ((cr=CompilationJob.waitingRequests.remove(this)) != null && cr.size()>0)  {
                    CompilationJob.requests.addAll(cr);
                }
            }          
        }
    }

    public void resetState(boolean invalidate, boolean updateIndex) {
        boolean invalid;
        synchronized (this) {
            invalid = (this.flags & INVALID) != 0;
            this.flags|=CHANGE_EXPECTED;
            if (invalidate) {
                this.flags|=(INVALID|RESCHEDULE_FINISHED_TASKS);
                if (this.currentInfo != null) {
//                    this.currentInfo.setChangedMethod (changedMethod);
                }
            }
            if (updateIndex) {
                this.flags|=UPDATE_INDEX;
            }            
        }
        Request r = CompilationJob.currentRequest.getTaskToCancel (invalidate);
        if (r != null) {
            r.task.cancel();
            Request oldR = rst.getAndSet(r);
            assert oldR == null;
        }
        if (!k24) {
            resetTask.schedule(reparseDelay);
        }
    }
    
    public void assignDocumentListener(final DataObject od) throws IOException {
        EditorCookie.Observable ec = od.getCookie(EditorCookie.Observable.class);            
        if (ec != null) {
            listener = new DocListener (ec,this);
        } else {
            LOGGER.log(Level.WARNING,String.format("File: %s has no EditorCookie.Observable", FileUtil.getFileDisplayName (od.getPrimaryFile())));      //NOI18N
        }
    }
  
    public Document getDocument() {
        if ((listener == null) || (listener.getDocument() == null)) {
            return null;
        }
        return listener.getDocument();
    }
    
    public TokenHierarchy getTokenHierarchy() {
        if (listener == null) {
            return null;
        }
        Document doc = listener.getDocument();
        if (doc == null) {
            try {
                DataObject od = DataObject.find(getFileObject());
                EditorCookie ec = od.getLookup().lookup(EditorCookie.class);
                doc = ec.openDocument();
            } catch (IOException dnfe) {
                return null;
            }
        }
        TokenHierarchy th = TokenHierarchy.get(doc);
        return th;
    }
    
    String getText() {
        if ((listener == null) || (listener.getDocument() == null)) {
            return "";
        }
        try {
            return listener.getDocument().getText(0, listener.getDocument().getLength());
        } catch (BadLocationException ex) {
            Exceptions.printStackTrace(ex);
        }
        return "";
    }
    
    public ClasspathInfo getCpInfo() {
        return cpInfo;
    }
    
    private static void handleAddRequest (final Request nr) {
        assert nr != null;
        //Issue #102073 - removed running task which is readded is not performed
        synchronized (CompilationJob.INTERNAL_LOCK) {            
            CompilationJob.toRemove.remove(nr.task);
            CompilationJob.requests.add (nr);
        }
        Request request = CompilationJob.currentRequest.getTaskToCancel(nr.priority);
        try {
            if (request != null) {
                request.task.cancel();
            }
        } finally {
            CompilationJob.currentRequest.cancelCompleted(request);
        }
    }
    
        /**
     * Performs the given task when the scan finished. When no background scan is running
     * it performs the given task synchronously. When the background scan is active it queues
     * the given task and returns, the task is performed when the background scan completes by
     * the thread doing the background scan.
     * @param task to be performed
     * @param shared if true the java compiler may be reused by other {@link org.netbeans.api.java.source.CancellableTasks},
     * the value false may have negative impact on the IDE performance.
     * @return {@link Future} which can be used to find out the sate of the task {@link Future#isDone} or {@link Future#isCancelled}.
     * The caller may cancel the task using {@link Future#cancel} or wait until the task is performed {@link Future#get}.
     * @throws IOException encapsulating the exception thrown by {@link CancellableTasks#run}
     * @since 0.12
     */
    public Future<Void> runWhenScanFinished (final Task<CompilationController> task, final boolean shared) throws IOException {
        return CompilationJob.runWhenScanFinished(this, task, shared);
    }

    /**
     * Returns a {@link JavaSource} instance associated to the given {@link javax.swing.Document},
     * it returns null if the {@link Document} is not
     * associated with data type providing the {@link JavaSource}.
     * @param doc {@link Document} for which the {@link JavaSource} should be found/created.
     * @return {@link JavaSource} or null
     * @throws {@link IllegalArgumentException} if doc is null
     */
    public static JavaFXSource forDocument(Document doc) throws IllegalArgumentException {
        if (doc == null) {
            throw new IllegalArgumentException ("doc == null");  //NOI18N
        }
        Reference<?> ref = (Reference<?>) doc.getProperty(JavaFXSource.class);
        JavaFXSource js = ref != null ? (JavaFXSource) ref.get() : null;
        if (js == null) {
            Object source = doc.getProperty(Document.StreamDescriptionProperty);
            
            if (source instanceof DataObject) {
                DataObject dObj = (DataObject) source;
                if (dObj != null) {
                    js = forFileObject(dObj.getPrimaryFile());
                }
            }
        }
        return js;
    }
    
    private class FileChangeListenerImpl extends FileChangeAdapter {                
        
        public @Override void fileChanged(final FileEvent fe) {
            resetState(true, false);
        }        

        public @Override void fileRenamed(FileRenameEvent fe) {
            resetState(true, false);
        }        
    }

    private static final class DevNullWriter extends Writer {
        public void write(char[] cbuf, int off, int len) throws IOException {
        }
        public void flush() throws IOException {
        }
        public void close() throws IOException {
        }
    }
    
}