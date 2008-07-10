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

package org.netbeans.modules.javafx.preview;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import org.netbeans.modules.javafx.editor.*;
import java.util.Map;
import java.util.Set;
import javax.swing.JComponent;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Collections;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ui.OpenProjects;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.modules.editor.NbEditorUtilities;
import org.openide.filesystems.FileObject;
import org.netbeans.modules.javafx.project.JavaFXProject;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;

/**
 *
 * @author answer
 */

public class JavaFXModel {
    
    private static Map<FXDocument, JavaFXRuntimeInfo> comps = Collections.synchronizedMap(new HashMap<FXDocument, JavaFXRuntimeInfo>());
    private static Set<FXDocument> documents = Collections.synchronizedSet(new HashSet<FXDocument>());
    
    private static final long PREVIEW_SHOW_DELAY = 1000;
    private static final long PREVIEW_CHECK_DELAY = 200;
    private static ChangeThread changeThread = null;
    private static long lastVisitTime = 0;
    private static Map <Project, Map <String, byte[]>> projectsClassBytes = null;
    private static Map <Project, ACThread> projectsAppContextsThreads = null;
    private static Object projectsClassBytesLock = new Object();
    
    static{
        initFX();
    }
    
    static class ProjectListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent evt) {
            if (evt.getPropertyName().contentEquals(OpenProjects.PROPERTY_OPEN_PROJECTS)) {
                Project projects[] = OpenProjects.getDefault().getOpenProjects();
                ArrayList<Project> projectArray = new ArrayList<Project>();
                Collections.addAll(projectArray, projects);
                synchronized (projectsClassBytesLock) {
                    ArrayList<Project> removedProjects = new ArrayList<Project>();
                    for (Project project : projectsClassBytes.keySet()) {
                        if (!projectArray.contains(project)) {
                            removedProjects.add(project);
                        }
                    }
                    for (Project project : removedProjects) {
                        projectsClassBytes.remove(project);
                        destroyAC(project);
                    }
                }
            }
        }
    }
    
    static class ClassPathListener implements PropertyChangeListener {
        private Project project = null;
        public ClassPathListener(Project project) {
            this.project = project;
        }
        public void propertyChange(PropertyChangeEvent evt) {
            if (evt.getPropertyName().contentEquals("platform.active")) {                             // NOI18N
                synchronized (projectsClassBytesLock) {
                    projectsClassBytes.remove(project);
                    destroyAC(project);
                    if (project instanceof JavaFXProject) {
                        PropertyEvaluator evaluator =((JavaFXProject)project).evaluator();
                        evaluator.removePropertyChangeListener(this);
                    }
                }
            }
        }
    }
    
    static public void addClassBytes(Project project, Map<String, byte[]> classBytes) {
        synchronized (projectsClassBytesLock) {
            Map <String, byte[]> classBytesForProj = projectsClassBytes.get(project);
            if (classBytesForProj == null) {
                classBytesForProj = new HashMap <String, byte[]>();
                if (project instanceof JavaFXProject) {
                    PropertyEvaluator evaluator =((JavaFXProject)project).evaluator();
                    evaluator.addPropertyChangeListener(new ClassPathListener(project));
                }
            }
            if (classBytes != null)
                classBytesForProj.putAll(classBytes);
            projectsClassBytes.put(project,classBytesForProj);
        }
    }
    
    static public void putClassBytes(Project project, Map<String, byte[]> classBytes) {
        synchronized (projectsClassBytesLock) {
            Map <String, byte[]> classBytesForProj = projectsClassBytes.get(project);
            if (classBytesForProj == null) {
                classBytesForProj = new HashMap <String, byte[]>();
                if (project instanceof JavaFXProject) {
                    PropertyEvaluator evaluator =((JavaFXProject)project).evaluator();
                    evaluator.addPropertyChangeListener(new ClassPathListener(project));
                }
            }
            else 
                classBytesForProj.clear();
            if (classBytes != null)
                classBytesForProj.putAll(classBytes);
            projectsClassBytes.put(project,classBytesForProj);
        }
    }
    
    static public Map<String, byte[]> getClassBytes(Project project) {
        synchronized (projectsClassBytesLock) {
            Map<String, byte[]> classBytes = projectsClassBytes.get(project);
            if (classBytes != null)
                return classBytes;
            else
                return new HashMap<String, byte[]>();
        }
    }
    
    public static FXDocument getNextDocument() {
        FXDocument result = documents.iterator().next();
        documents.remove(result);
        return(result);
    }
    
    public static boolean hasMoreDocuments() {
        return(documents.iterator().hasNext());
    }
    
    public static void sourceChanged(FXDocument doc){
        CodeManager.cut(doc);
        lastVisitTime = System.currentTimeMillis();
        previewReq(doc, true);
    }

    public static void previewReq(FXDocument doc, boolean requiredNew){
        comps.get(doc).sourceChanged();
        changeThread.setDocument(doc);
    }
    
    public static void sourceDependencyChanged(FXDocument doc){
        comps.get(doc).sourceDependencyChanged();
        sourceChanged(doc);
    }
    
    public static void projectChanged(Project project){
        if (project != null){
            for(JavaFXRuntimeInfo ri: comps.values()){
                if (getProject(ri.getFileObject()) == project){
                    sourceDependencyChanged(ri.getDocument());
                }
            }
        }
    }
    
    public static void fireDependenciesChange(FXDocument doc){
        for(JavaFXRuntimeInfo ri: comps.values()){
            ri.fireDependenciesUpdate(doc);
        }
    }
    
    public static void showPreview(FXDocument document, boolean requiredNew) {
        if (document != null && document.executionAllowed()){
            JComponent resultComponent = getResultComponent(document);
            if ((requiredNew) || (resultComponent == null)){
                
                //boolean isAdded = documents.add(document);
                //if (isAdded){
                    renderPreview(document);
                //}
            }else{
                document.renderPreview(resultComponent);
            }
        }
    }

    public static void addDocument(FXDocument doc){
        synchronized(comps){
            JavaFXRuntimeInfo ri = new JavaFXRuntimeInfo(doc);
            comps.put(doc, ri);
        }
    }

    public static void setResultComponent(FXDocument doc, JComponent comp){
        comps.get(doc).setResultComponent(comp);
    }

    public static JComponent getResultComponent(FXDocument doc){
        return comps.get(doc).getResultComponent();
    }
    
    public static Reader getPreviewWidgetSource() {
        ClassLoader loader = JavaFXModel.class.getClassLoader();
        return new InputStreamReader(loader.getResourceAsStream("org/netbeans/modules/javafx/model/impl/JavaFXWidget.fx"));
    }
    
    private static void initFX(){
        if (changeThread == null){
            changeThread = new ChangeThread();
            new Thread(new ChangeThread()).start();
        }
        projectsClassBytes = new HashMap<Project, Map<String, byte[]>>();
        projectsAppContextsThreads = new HashMap<Project, ACThread>();
        OpenProjects.getDefault().addPropertyChangeListener(new ProjectListener());
    }
    
    static PreviewThread tPreview = null;
    
    synchronized private static void renderPreview(final FXDocument doc) {
        try {
            if (tPreview != null)
                tPreview.join();
            tPreview = new PreviewThread(doc);
            tPreview.start();
        }catch(Exception e){
            e.printStackTrace();
        }
    }
    
    static class ACThread extends Thread {
        private volatile Runnable request = null;
        private Object lock1 = new Object();
        private Object lock2 = new Object();
        private volatile boolean skipLock1 = false;
        private volatile boolean skipLock2 = false;
        Object ac = null;
        
        public ACThread(ThreadGroup tg) {
            super(tg, "SACT"); // NOI18N
        }
        
        public void execute(Runnable runnable) throws Exception {
            synchronized (lock1) {
                request = runnable;
                lock1.notifyAll();
                skipLock1 = true;
            }
            synchronized (lock2) {
                try {
                    if (!skipLock2) {
                        lock2.wait(20000);
                        skipLock2 = false;
                    }
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
                if (request != null)
                    throw new Exception();
            }
        }
        
        @Override
        public void run() {
            try {
                Class<?> acc = this.getClass().getClassLoader().loadClass(STK);
                ac = acc.getDeclaredMethod(CNAPC).invoke(null);
            } catch (Exception ex) {
                ex.printStackTrace();
            }
            while (true) {
                synchronized (lock1) {
                    while (request == null) {
                        try {
                            if (!skipLock2) {
                                lock1.wait();
                                skipLock1 = false;
                            }
                        } catch (Exception ex) {
                            ex.printStackTrace();
                        }
                    }
                }
                request.run();
                request = null;
                synchronized (lock2) {
                    lock2.notify();
                    skipLock2 = true;
                }
            }
        }
        
        public void cleanup() {
            if (ac != null) {
                try {
                    Class<?> acc = this.getClass().getClassLoader().loadClass(APC);
                    acc.getDeclaredMethod(DSP).invoke(ac);
                } catch (Throwable er) {
                    er.printStackTrace();
                }
            }
        }
    }
    
    private static int instanceCounter = 0;
    private static ACThread createAC(Project project) {
        ACThread thread = new ACThread(new ThreadGroup("SACG" + instanceCounter++));
        projectsAppContextsThreads.put(project, thread);
        thread.start();
        try {
            Thread.sleep(500);
        } catch (InterruptedException ex) {
            ex.printStackTrace();
        }
        return thread;
    }
    
    public static void destroyAC(Project project) {
        if (projectsAppContextsThreads.containsKey(project))
            projectsAppContextsThreads.remove(project).cleanup();
    }
    
    public static void runInAC(Project project, Runnable runnable) throws Exception {
        ACThread thread = projectsAppContextsThreads.get(project);
        if (thread == null) thread = createAC(project);
        thread.execute(runnable);
    }
            
    public static Project getProject(FXDocument doc){
        return getProject(NbEditorUtilities.getFileObject(doc));
    }

    public static Project getProject(FileObject fileObject){
        return FileOwnerQuery.getOwner(fileObject);
    }

    private static class ChangeThread implements Runnable{
        private static FXDocument doc;
        public void run(){
            while(true){
                if (doc != null && doc.executionAllowed() && (getResultComponent(doc) == null) && (System.currentTimeMillis() - lastVisitTime > PREVIEW_SHOW_DELAY)){
                    FXDocument document = doc;
                    doc = null;
                    showPreview(document, false);
                }
                try{
                    Thread.sleep(PREVIEW_CHECK_DELAY);
                }catch(InterruptedException e){}
            }
        }
        public void setDocument(FXDocument doc){
            ChangeThread.doc = doc;
        }
    }
    
    static private String STK = "sun.awt.SunToolkit";                       // NOI18N
    static private String CNAPC = "createNewAppContext";                    // NOI18N
    static private String APC = "sun.awt.AppContext";                       // NOI18N
    static private String DSP = "dispose";                                  // NOI18N
}