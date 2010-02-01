/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
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

package org.netbeans.modules.javafx.refactoring.impl.plugins;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.UnitTree;
import javax.lang.model.element.Element;
import java.io.IOException;
import java.util.*;
import javax.lang.model.element.ElementKind;
import javax.swing.Action;
import org.netbeans.api.fileinfo.NonRecursiveFolder;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.modules.javafx.refactoring.impl.ElementLocation;
import org.netbeans.modules.javafx.refactoring.impl.WhereUsedQueryConstants;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.impl.ui.WhereUsedQueryUI;
import org.netbeans.modules.refactoring.api.*;
import org.netbeans.modules.refactoring.spi.*;
import org.netbeans.modules.refactoring.spi.ui.RefactoringUI;
import org.netbeans.modules.refactoring.spi.ui.UI;

import org.openide.ErrorManager;
import org.openide.filesystems.FileObject;
import org.openide.util.*;
import org.openide.util.lookup.Lookups;


/**
 * The plugin that carries out Safe Delete refactoring.
 * @author Bharath Ravikumar
 * @author Jan Becicka
 * @author Jaroslav Bachorik
 *
 * Copied over from the java implementation
 */
public class SafeDeleteRefactoringPlugin extends ProgressProviderAdapter implements RefactoringPlugin {
    private final ElementLocation location;
    private SafeDeleteRefactoring refactoring;
    private WhereUsedQuery[] whereUsedQueries;
    
    private static final String JAVAFX_EXTENSION = "fx"; // NOI18N
    
    /**
     * Creates the a new instance of the Safe Delete refactoring
     * plugin.
     * @param refactoring The refactoring to be used by this plugin
     */
    public SafeDeleteRefactoringPlugin(SafeDeleteRefactoring refactoring) {
        this.location = refactoring.getRefactoringSource().lookup(ElementLocation.class);
        this.refactoring = refactoring;
    }

    public void cancelRequest() {
        // do nothing
    }

    /**
     * Checks whether the element being refactored is a valid Method/Field/Class
     * @return Problem returns a generic problem message if the check fails
     */
    @Override
    public Problem preCheck() {
        return null;
    }

    /**
     * A No-op for this particular refactoring.
     */
    @Override
    public Problem fastCheckParameters() {
        //Nothing to be done for Safe Delete
        return null;
    }

    private ArrayList<ElementLocation> grips = new ArrayList<ElementLocation>();
    /**
     * Invokes the checkParameters of each of the underlying
     * WhereUsed refactorings and returns a Problem (if any)
     * returned by any of these queries.
     */
    @Override
    public Problem checkParameters() {
        //This class expects too many details from SafeDeleteRefactoring
        //But there's no other go I guess.
        grips.clear();
        for (final FileObject f: lookupJavaFXFileObjects()) {
            JavaFXSource source = JavaFXSource.forFileObject(f);
            if (source == null) {
                continue;
            }
            try {
                source.runUserActionTask(new Task<CompilationController>() {
                    public void cancel() {

                    }
                    public void run(CompilationController co) throws Exception {
                        UnitTree cut = co.getCompilationUnit();
                        for (Tree t: cut.getTypeDecls()) {
                            ElementLocation handle = ElementLocation.forPath(JavaFXTreePath.getPath(cut, t), co);
                            if (!containsHandle(handle, co))
                                grips.add(handle);
                        }
                    }
                }, true);
            } catch (IllegalArgumentException ex) {
                ex.printStackTrace();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }

        grips.addAll(refactoring.getRefactoringSource().lookupAll(ElementLocation.class));

        whereUsedQueries = new WhereUsedQuery[grips.size()];
        for(int i = 0;i <  whereUsedQueries.length; ++i) {
            whereUsedQueries[i] = createQuery(grips.get(i));

            whereUsedQueries[i].putValue(WhereUsedQuery.SEARCH_IN_COMMENTS, refactoring.isCheckInComments());
            if(ElementKind.METHOD == grips.get(i).getElement().getKind()){
                whereUsedQueries[i].putValue(WhereUsedQueryConstants.FIND_OVERRIDING_METHODS,true);
                whereUsedQueries[i].putValue(WhereUsedQueryConstants.FIND_SUBCLASSES, true);
                whereUsedQueries[i].putValue(WhereUsedQueryConstants.FIND_DIRECT_SUBCLASSES, true);
            }
        }

        Problem problemFromUsage = null;
        for(int i = 0;i < whereUsedQueries.length; ++i) {
//          Fix for issue 63050. Doesn't make sense to check usages of a Resource.Ignore it.
//            if(whereUsedQueries[i].getRefactoredObject() instanceof Resource)
//                continue;
            if((problemFromUsage = whereUsedQueries[i].checkParameters()) != null)
                return problemFromUsage;
        }
        return null;
    }
    
    /**
     * For each element to be refactored, the corresponding
     * prepare method of the underlying WhereUsed query is
     * invoked to check for usages. If none is present, a
     * <CODE>SafeDeleteRefactoringElement</CODE> is created
     * with the corresponding element.
     * @param refactoringElements
     * @return
     */
    public Problem prepare(RefactoringElementsBag refactoringElements) {
        RefactoringSession inner = RefactoringSession.create("delete"); // NOI18N
//        Collection<ElementGrip> abstractMethHandles = new ArrayList<ElementGrip>();
        Set<Object> refactoredObjects = new HashSet<Object>();
        Collection<? extends FileObject> files = lookupJavaFXFileObjects();
        NonRecursiveFolder nrf = refactoring.getRefactoringSource().lookup(NonRecursiveFolder.class);
        if (nrf != null) {
            refactoredObjects.addAll(getAffectedFolders(nrf.getFolder()));
        }

        fireProgressListenerStart(AbstractRefactoring.PARAMETERS_CHECK, whereUsedQueries.length + 1);
        for(int i = 0;i < whereUsedQueries.length; ++i) {
            Object refactoredObject = whereUsedQueries[i].getRefactoringSource().lookup(Object.class);
            refactoredObjects.add(refactoredObject);

            whereUsedQueries[i].prepare(inner);
            fireProgressListenerStep();
        }
        Problem problemFromWhereUsed = null;
        for (RefactoringElement refacElem : inner.getRefactoringElements()) {
            ElementLocation eloc = refacElem.getLookup().lookup(ElementLocation.class);
            if (!(refactoredObjects.contains(eloc) || refactoredObjects.contains(eloc.getSourceFile()) || refactoredObjects.contains(eloc.getSourceFile().getParent()))) {
                problemFromWhereUsed = new Problem(false, NbBundle.getMessage(SafeDeleteRefactoringPlugin.class, "ERR_ReferencesFound"), ProblemDetailsFactory.createProblemDetails(new ProblemDetailsImplemen(new WhereUsedQueryUI(eloc!=null?eloc:null, eloc.getCompilationInfo()), inner)));
                break;
            }
        }

        if(problemFromWhereUsed != null){
            fireProgressListenerStop();
            return problemFromWhereUsed;
        }

        fireProgressListenerStop();
        return null;
    }

    private Set<FileObject> getAffectedFolders(FileObject folder) {
        Set<FileObject> objs = new HashSet<FileObject>();
        
        objs.add(folder);
        for(FileObject subFolder : folder.getChildren()) {
            if (subFolder.isFolder()) {
                objs.addAll(getAffectedFolders(subFolder));
            }
        }
        return objs;
    }

    private Collection<? extends FileObject> lookupJavaFXFileObjects() {
        Lookup lkp = refactoring.getRefactoringSource();
        Collection<? extends FileObject> javaFiles = null;
        NonRecursiveFolder folder = lkp.lookup(NonRecursiveFolder.class);
        if (folder != null) {
            javaFiles = getJavaFileObjects(folder.getFolder(), false);
        } else {
            Collection<FileObject> javaFileObjects =  new ArrayList<FileObject>();
            for (FileObject fileObject : lkp.lookupAll(FileObject.class)) {
                if (fileObject.isFolder()) {
                    javaFileObjects.addAll(getJavaFileObjects(fileObject, true));
                }else if (SourceUtils.isRefactorable(fileObject)) {
                    javaFileObjects.add(fileObject);
                }
            }
            javaFiles = javaFileObjects;
        }
        return javaFiles;
    }

     private static Collection<FileObject> getJavaFileObjects(FileObject dirFileObject, boolean isRecursive){
        Collection<FileObject> javaSrcFiles = new ArrayList<FileObject>();
        addSourcesInDir(dirFileObject, isRecursive, javaSrcFiles);
        return javaSrcFiles;
    }

    private static void addSourcesInDir(FileObject dirFileObject, boolean isRecursive, Collection<FileObject> javaSrcFiles) {
        for (FileObject childFileObject : dirFileObject.getChildren()) {
            if (childFileObject.isData() && JAVAFX_EXTENSION.equalsIgnoreCase(childFileObject.getExt())) {
                javaSrcFiles.add(childFileObject);
            }
            else if (childFileObject.isFolder() && isRecursive) {
                addSourcesInDir(childFileObject, true, javaSrcFiles);
            }
        }
    }

    private boolean containsHandle(ElementLocation handle, CompilationInfo info) {
        Element wanted = handle.getElement(info);
        for (ElementLocation current : refactoring.getRefactoringSource().lookupAll(ElementLocation.class)) {
            if (wanted == current.getElement(info)) {
                return true;
            }
        }
        return false;
    }

    private WhereUsedQuery createQuery(ElementLocation loc) {
        WhereUsedQuery q = new WhereUsedQuery(Lookups.singleton(loc));
        for (Object o:refactoring.getContext().lookupAll(Object.class)) {
            q.getContext().add(o);
        }
        q.getContext().add(refactoring);
        q.getContext().add(this);
        return q;
    }

//    private static boolean isPendingDelete(ElementLocation elementLocati, Set<Object> refactoredObjects){
//        ElementGrip parent = elementLocati;
//        if (parent!=null) {
//            do {
//                if (refactoredObjects.contains(parent.getHandle())) {
//                    return true;
//                }
//                parent = parent.getParent();
//            } while (parent!=null);
//        }
//        return false;
//    }

    private class ProblemDetailsImplemen implements ProblemDetailsImplementation {

        private RefactoringUI ui;
        private RefactoringSession rs;

        public ProblemDetailsImplemen(RefactoringUI ui, RefactoringSession rs) {
            this.ui = ui;
            this.rs = rs;
        }

        public void showDetails(Action callback, Cancellable parent) {
            parent.cancel();
            UI.openRefactoringUI(ui, rs, callback);
        }

        public String getDetailsHint() {
            return NbBundle.getMessage(SafeDeleteRefactoringPlugin.class, "LBL_ShowUsages");
        }

    }
}