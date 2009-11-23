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

package org.netbeans.modules.javafx.source.parsing;

import com.sun.javafx.api.tree.UnitTree;
import com.sun.tools.mjavac.parser.DocCommentScanner;
import com.sun.tools.mjavac.util.Context;
import com.sun.tools.javafx.api.JavafxcTaskImpl;
import com.sun.tools.javafx.api.JavafxcTool;
import com.sun.tools.javafxdoc.JavafxdocEnter;
import com.sun.tools.javafxdoc.JavafxdocMemberEnter;
import com.sun.tools.javafxdoc.Messager;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.event.ChangeListener;
import javax.tools.Diagnostic;
import javax.tools.DiagnosticListener;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationPhase;
import org.netbeans.modules.javafx.source.ApiSourcePackageAccessor;
import org.netbeans.modules.javafx.source.JavadocEnv;
import org.netbeans.modules.javafx.source.classpath.SourceFileObject;
import org.netbeans.modules.parsing.api.Snapshot;
import org.netbeans.modules.parsing.api.Source;
import org.netbeans.modules.parsing.api.Task;
import org.netbeans.modules.parsing.spi.ParseException;
import org.netbeans.modules.parsing.spi.Parser;
import org.netbeans.modules.parsing.spi.Parser.Result;
import org.netbeans.modules.parsing.spi.SourceModificationEvent;
import org.openide.filesystems.FileObject;
import org.openide.util.ChangeSupport;

/**
 *
 * @author Miloslav Metelka
 */
public final class JavaFXParser extends Parser {

    // -J-Dorg.netbeans.modules.javafx.source.parsing.JavaFXParser.level=FINEST
    private static final Logger LOG = Logger.getLogger(JavaFXParser.class.getName());

    private static final PrintWriter DEV_NULL = new PrintWriter(new DevNullWriter(), false);

    private final AtomicBoolean cancelled = new AtomicBoolean();

    private final ChangeSupport listeners = new ChangeSupport(this);

    private Snapshot snapshot;

    private ClasspathInfo classpathInfo;

    CompilationPhase currentPhase;

    UnitTree compilationUnit;

    Iterable <? extends JavaFileObject> classBytes;

    JavafxcTaskImpl javafxcTaskImpl;

    private DiagnosticListener<JavaFileObject> diagnosticListener;

    private List<Diagnostic> diagnosticsCached;

    private Boolean errorsPresentCached;

    public JavaFXParser() {
    }

    @Override
    public void parse(Snapshot snapshot, Task task, SourceModificationEvent event) {
        this.snapshot = snapshot;
        cancelled.set(false);
        // Init classpath if not inited yet
        classpathInfo = getTaskClasspathInfo(task);
        if (snapshot != null && classpathInfo == null) {
            classpathInfo = ClasspathInfo.create(fileObject());
        }
        if (LOG.isLoggable(Level.FINE)) {
            LOG.fine("Parsing snapshot: " + snapshot + ", classpathInfo=" + classpathInfo + "\n"); // NOI18N
        }
        currentPhase = CompilationPhase.MODIFIED;
    }

    @Override
    public Result getResult(Task task) throws ParseException {
        assert (task != null);
        JavaFXParserResultImpl impl = new JavaFXParserResultImpl(this);
        if (snapshot != null) {
            ClasspathInfo taskClasspathInfo = getTaskClasspathInfo(task);
            if (taskClasspathInfo != null && !taskClasspathInfo.equals(classpathInfo)) {
                // Force init of reparse
                parse(snapshot, task, null);
            }
            CompilationPhase requestedPhase = CompilationPhase.ANALYZED;
            if (task instanceof JavaFXSourceCancelableTask) {
                JavaFXSourceCancelableTask cTask = (JavaFXSourceCancelableTask) task;
                requestedPhase = cTask.getPhase();
            }
            if (currentPhase.lessThan(requestedPhase)) {
                try {
                    if (LOG.isLoggable(Level.FINE)) {
                        LOG.fine("getResult() moves to phase: " + requestedPhase);
                    }
                    toPhase(requestedPhase);
                } catch (IOException e) {
                    throw new ParseException("Parser error", e);
                }
            }
        }
        return ApiSourcePackageAccessor.get().createResult(impl);
    }

    public void cancel() {
        cancelled.set(true);
    }

    private ClasspathInfo getTaskClasspathInfo(Task task) {
        ClasspathInfo taskClasspathInfo;
        if (task instanceof JavaFXParserTask) {
            JavaFXParserTask parserTask = (JavaFXParserTask) task;
            taskClasspathInfo = parserTask.classpathInfo();
        } else if (task instanceof LegacyUserTask) {
            LegacyUserTask legacyTask = (LegacyUserTask) task;
            taskClasspathInfo = legacyTask.classpathInfo();
        } else if (task instanceof JavaFXSourceCancelableTask) {
            JavaFXSourceCancelableTask sourceTask = (JavaFXSourceCancelableTask) task;
            taskClasspathInfo = sourceTask.classpathInfo();
        } else {
            taskClasspathInfo = null;
        }
        return taskClasspathInfo;
    }

    public void addChangeListener(ChangeListener changeListener) {
        listeners.addChangeListener(changeListener);
    }

    public void removeChangeListener(ChangeListener changeListener) {
        listeners.removeChangeListener(changeListener);

    }

    public Source source() {
        return snapshot().getSource();
    }

    public Snapshot snapshot() {
        return snapshot;
    }

    private FileObject fileObject() {
        return source().getFileObject();
    }

    public ClasspathInfo classpathInfo() {
        return classpathInfo;
    }

    public Context context() {
        return javafxcTaskImpl.getContext();
    }

    public List<Diagnostic> getDiagnostics() {
        if (diagnosticsCached == null) {
            final JavaFXParser.DiagnosticListenerImpl dli = (JavaFXParser.DiagnosticListenerImpl)
                    context().get(DiagnosticListener.class);
            final TreeMap<Integer, Diagnostic> errorsMap = dli.errors;
            Collection<Diagnostic> errors = errorsMap.values();
            diagnosticsCached = new ArrayList<Diagnostic>(errors);
        }
        return diagnosticsCached;
    }

    public boolean isErrors() {
        if (errorsPresentCached == null) {
            for (Diagnostic diag: getDiagnostics()) {
                if (diag.getKind() == Diagnostic.Kind.ERROR) {
                    errorsPresentCached = true;
                    break;
                }
            }
            if (errorsPresentCached == null) {
                errorsPresentCached = false;
            }
        }
        return errorsPresentCached;
    }

    synchronized CompilationPhase toPhase(CompilationPhase phase) throws IOException {
        if (currentPhase.lessThan(CompilationPhase.PARSED) && !phase.lessThan(CompilationPhase.PARSED)) {
            if (cancelled.get()) {
                //Keep the currentPhase unchanged, it may happen that an userActionTask
                //runnig after the phace completion task may still use it.
                return currentPhase;
            }

            if (LOG.isLoggable(Level.FINE)) LOG.fine("Starting to parse " + fileObject().getNameExt()); // NOI18N
            long start = System.currentTimeMillis();
            Iterable<? extends UnitTree> trees = null;
            diagnosticsCached = null;
            errorsPresentCached = null;
            javafxcTaskImpl = createJavafxcTaskImpl();
            try {
                trees = javafxcTaskImpl.parse();
            } catch (RuntimeException parserError) {
                LOG.log(Level.FINE, "Error in parser", parserError); // NOI18N
                return currentPhase;
            }
//                new JavaFileObject[] {currentInfo.jfo});
            Iterator<? extends UnitTree> it = trees.iterator();
            assert it.hasNext();
            compilationUnit = it.next();
            assert !it.hasNext();
            currentPhase = CompilationPhase.PARSED;

            long end = System.currentTimeMillis();
            Logger.getLogger("TIMER").log(Level.FINE, "Compilation Unit", new Object[] {fileObject(), compilationUnit}); // log the instance // NOI18N
            Logger.getLogger("TIMER").log(Level.FINE, "Parsed", new Object[] {fileObject(), end-start}); // NOI18N
            if (LOG.isLoggable(Level.FINE)) LOG.fine("Finished parsing " + fileObject().getNameExt()); // NOI18N
        }

        if (cancelled.get()) {
            return CompilationPhase.MODIFIED;
        }

        if (currentPhase == CompilationPhase.PARSED && !phase.lessThan(CompilationPhase.ANALYZED)) {
            if (LOG.isLoggable(Level.FINE)) LOG.fine("Starting to analyze " + fileObject().getNameExt()); // NOI18N
            long start = System.currentTimeMillis();
            diagnosticsCached = null;
            errorsPresentCached = null;
            try {
                javafxcTaskImpl.analyze();
            } catch (RuntimeException analyzerError) {
                LOG.log(Level.FINE, "Error in analyzer", analyzerError); // NOI18N
                return currentPhase;
            } catch (ThreadDeath td) {
                throw td;
            } catch (Throwable assErr) {
                LOG.log(Level.FINE, "Error in analyzer", assErr); // NOI18N
                return currentPhase;
            }
            currentPhase = CompilationPhase.ANALYZED;

            long end = System.currentTimeMillis();
            Logger.getLogger("TIMER").log(Level.FINE, "Analyzed", new Object[] {fileObject(), end-start}); // NOI18N
            if (LOG.isLoggable(Level.FINE)) LOG.fine("Finished to analyze " + fileObject().getNameExt()); // NOI18N
        }

        if (currentPhase == CompilationPhase.ANALYZED && !phase.lessThan(CompilationPhase.UP_TO_DATE)) {
            currentPhase = CompilationPhase.UP_TO_DATE;
        }

        if (currentPhase == CompilationPhase.UP_TO_DATE && !phase.lessThan(CompilationPhase.CODE_GENERATED)) {
            if (!isErrors()) {
                long start = System.currentTimeMillis();
                Iterable <? extends JavaFileObject> bytes = null;
                try {
                    bytes = javafxcTaskImpl.generate();
                } catch (RuntimeException generateError) {
                    LOG.log(Level.FINE, "Error in generate", generateError); // NOI18N
                    return currentPhase;
                }
                classBytes = bytes;
                long end = System.currentTimeMillis();
                Logger.getLogger("TIMER").log(Level.FINE, "Analyzed", new Object[] {fileObject(), end-start}); // NOI18N
            } else {
                classBytes = null;;
            }
            currentPhase = CompilationPhase.CODE_GENERATED;
        }

        return phase;
    }

    private JavafxcTaskImpl createJavafxcTaskImpl() {
        JavafxcTool tool = JavafxcTool.create();
        FileObject fileObject = fileObject();
        JavaFileManager fileManager = ApiSourcePackageAccessor.get().getFileManager(classpathInfo(), tool);
        JavaFileObject jfo = (JavaFileObject) SourceFileObject.create(fileObject, null); // XXX

        if (LOG.isLoggable(Level.FINEST)) {
            try {
                String sourceTxt = jfo.getCharContent(true).toString();
                LOG.finest("\n======================================================\n"); // NOI18N
                LOG.finest(sourceTxt);
                LOG.finest("\n------------------------------------------------------\n"); // NOI18N
            } catch (IOException ex) {
                LOG.log(Level.FINEST, "Cannot get file content.", ex); // NOI18N
            }
        }

        List<String> options = new ArrayList<String>();
        //options.add("-Xjcov"); //NOI18N, Make the compiler store end positions
        options.add("-XDdisableStringFolding"); //NOI18N

        // required for code formatting (and completion I believe), see JFXC-3528
        options.add("-XDpreserveTrees"); //NOI18N

        diagnosticListener = new DiagnosticListenerImpl();
        JavafxcTaskImpl task = (JavafxcTaskImpl)tool.getTask(null, fileManager, diagnosticListener, options, Collections.singleton(jfo));
        Context context = task.getContext();
//        JavafxdocClassReader.preRegister(context);
        Messager.preRegister(context, null, DEV_NULL, DEV_NULL, DEV_NULL);
        JavafxdocEnter.preRegister(context);
        JavafxdocMemberEnter.preRegister(context);
        JavadocEnv.preRegister(context, classpathInfo());
        DocCommentScanner.Factory.preRegister(context);
        return task;
    }

    private static final class DevNullWriter extends Writer {
        public void write(char[] cbuf, int off, int len) throws IOException {
        }
        public void flush() throws IOException {
        }
        public void close() throws IOException {
        }
    }

    static class DiagnosticListenerImpl implements DiagnosticListener<JavaFileObject> {

        final TreeMap<Integer,Diagnostic> errors;

        public DiagnosticListenerImpl() {
            this.errors = new TreeMap<Integer,Diagnostic>();
        }

        public void report(Diagnostic<? extends JavaFileObject> message) {
            LOG.fine("Error at [" + message.getLineNumber() + ":" + message.getColumnNumber() + "]/" + message.getEndPosition() + " - " + message.getMessage(null)); // NOI18N
            errors.put((int)message.getPosition(),message);
        }
    }
}
