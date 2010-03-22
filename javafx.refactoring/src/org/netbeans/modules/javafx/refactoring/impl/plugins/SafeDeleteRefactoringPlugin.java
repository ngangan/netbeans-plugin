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

import org.netbeans.modules.javafx.refactoring.impl.plugins.elements.BaseRefactoringElementImplementation;
import java.util.*;
import javax.lang.model.element.ElementKind;
import javax.swing.Action;
import org.netbeans.api.fileinfo.NonRecursiveFolder;
import org.netbeans.api.javafx.source.ClassIndex.SearchKind;
import org.netbeans.api.javafx.source.ClassIndex.SearchScope;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.refactoring.RefactoringSupport;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.impl.ui.WhereUsedQueryUI;
import org.netbeans.modules.javafx.refactoring.repository.ClassModel;
import org.netbeans.modules.javafx.refactoring.repository.ClassModelFactory;
import org.netbeans.modules.javafx.refactoring.repository.ElementDef;
import org.netbeans.modules.javafx.refactoring.repository.Usage;
import org.netbeans.modules.javafx.refactoring.transformations.RemoveTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.Transformation;
import org.netbeans.modules.refactoring.api.*;
import org.netbeans.modules.refactoring.java.api.WhereUsedQueryConstants;
import org.netbeans.modules.refactoring.spi.*;
import org.netbeans.modules.refactoring.spi.ui.RefactoringUI;
import org.netbeans.modules.refactoring.spi.ui.UI;

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
public class SafeDeleteRefactoringPlugin extends JavaFXRefactoringPlugin {
    private SafeDeleteRefactoring refactoring;
    private WhereUsedQuery[] whereUsedQueries;
    
    private static final String JAVAFX_EXTENSION = "fx"; // NOI18N

    /**
     * Creates the a new instance of the Safe Delete refactoring
     * plugin.
     * @param refactoring The refactoring to be used by this plugin
     */
    public SafeDeleteRefactoringPlugin(SafeDeleteRefactoring refactoring) {
        this.refactoring = refactoring;
    }

    /**
     * Checks whether the element being refactored is a valid Method/Field/Class
     * @return Problem returns a generic problem message if the check fails
     */
    @Override
    public Problem preCheck() {
        ElementDef edef = refactoring.getRefactoringSource().lookup(ElementDef.class);
        if (edef != null && // #182465 - don't try to access ElementDef for eg. package deletion
            !edef.isFromSource()) {
            return new Problem(true, NbBundle.getMessage(SafeDeleteRefactoringPlugin.class, "ERR_CannotRefactorLibraryClass", SourceUtils.getEnclosingTypeName(edef)));
        }
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

    private ArrayList<ElementDef> grips = new ArrayList<ElementDef>();
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
            ClassModel cm = RefactoringSupport.classModelFactory(refactoring).classModelFor(f);

            grips.addAll(cm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE, ElementKind.ENUM)));
        }

        grips.addAll(refactoring.getRefactoringSource().lookupAll(ElementDef.class));

        whereUsedQueries = new WhereUsedQuery[grips.size()];
        for(int i = 0;i <  whereUsedQueries.length; ++i) {
            whereUsedQueries[i] = createQuery(grips.get(i));

            whereUsedQueries[i].putValue(WhereUsedQuery.SEARCH_IN_COMMENTS, refactoring.isCheckInComments());
            if(ElementKind.METHOD == grips.get(i).getKind()){
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
     * @param reb
     * @return
     */
    public Problem prepare(RefactoringElementsBag reb) {
        RefactoringSession inner = RefactoringSession.create("delete"); // NOI18N
//        Collection<ElementGrip> abstractMethHandles = new ArrayList<ElementGrip>();
        Set<Object> refactoredObjects = new HashSet<Object>();

        NonRecursiveFolder nrf = refactoring.getRefactoringSource().lookup(NonRecursiveFolder.class);
        if (nrf != null) {
            refactoredObjects.addAll(getAffectedFolders(nrf.getFolder()));
        }

        ElementDef edef = refactoring.getRefactoringSource().lookup(ElementDef.class);

        fireProgressListenerStart(AbstractRefactoring.PARAMETERS_CHECK, whereUsedQueries.length + 1);
        for(int i = 0;i < whereUsedQueries.length; ++i) {
            if (isCancelled()) return null;

            Object refactoredObject = whereUsedQueries[i].getRefactoringSource().lookup(FileObject.class);
            refactoredObjects.add(refactoredObject);

            whereUsedQueries[i].prepare(inner);
            fireProgressListenerStep();
        }
        Problem problemFromWhereUsed = null;
        for (RefactoringElement refacElem : inner.getRefactoringElements()) {
            if (isCancelled()) return null;
            
            Usage usg = refacElem.getLookup().lookup(Usage.class);
            ElementDef refdef = usg.getDef();
            if (refdef.getStartFQN() == usg.getStartPos() && !refdef.overrides(edef)) continue; // don't include the element definition in the usages

            FileObject fo = usg.getFile();

            if (!(refactoredObjects.contains(fo) || refactoredObjects.contains(fo.getParent()))) {
                problemFromWhereUsed = new Problem(false,
                                                   NbBundle.getMessage(SafeDeleteRefactoringPlugin.class, "ERR_ReferencesFound"),
                                                   ProblemDetailsFactory.createProblemDetails(
                                                        new ProblemDetailsImplemen(
                                                            new WhereUsedQueryUI(refdef, RefactoringSupport.classModelFactory(refactoring), fo),
                                                            inner
                                                        )
                                                   )
                );
                break;
            }
        }

        if(problemFromWhereUsed != null){
            fireProgressListenerStop();
            return problemFromWhereUsed;
        }

        if (isCancelled()) return null;
        if (lookupJavaFXFileObjects().isEmpty()) { // element deletion; no associated file
            for(ElementDef refdef : refactoring.getRefactoringSource().lookupAll(ElementDef.class)) {
                if (isCancelled()) return null;
                
                ElementHandle eh = refdef.createHandle();
                ElementHandle teh = new ElementHandle(ElementKind.CLASS, new String[]{eh.getSignatures()[0]});

                Set<FileObject> defFo = RefactoringSupport.classIndex(refactoring).getResources(teh, EnumSet.of(SearchKind.TYPE_DEFS), EnumSet.allOf(SearchScope.class));
                if (defFo.size() == 1) {
                    ClassModel cm = RefactoringSupport.classModelFactory(refactoring).classModelFor(defFo.iterator().next());
                    for(final ElementDef def : cm.getElementDefs(EnumSet.of(refdef.getKind()))) {
                        if (isCancelled()) return null;
                        if (def.equals(refdef)) {
                            reb.add(refactoring, new BaseRefactoringElementImplementation(cm.getSourceFile(), reb.getSession()) {

                                @Override
                                protected Set<Transformation> prepareTransformations(FileObject fo) {
                                    Set<Transformation> t = new HashSet<Transformation>();
                                    t.add(new RemoveTextTransformation(def.getStartPos(), def.getEndPos() - def.getStartPos() + 1));
                                    return t;
                                }

                                @Override
                                protected String getRefactoringText() {
                                    return "Delete declaration of " + def.getName();
                                }
                            });
                            break;
                        }
                    }
                }
            }
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
        Collection<? extends FileObject> javaFXFiles = null;
        NonRecursiveFolder folder = lkp.lookup(NonRecursiveFolder.class);
        if (folder != null) {
            javaFXFiles = getJavaFXFileObjects(folder.getFolder(), false);
        } else {
            Collection<FileObject> javaFXFileObjects =  new ArrayList<FileObject>();
            for (FileObject fileObject : lkp.lookupAll(FileObject.class)) {
                if (fileObject.isFolder()) {
                    javaFXFileObjects.addAll(getJavaFXFileObjects(fileObject, true));
                }else if (SourceUtils.isRefactorable(fileObject)) {
                    javaFXFileObjects.add(fileObject);
                }
            }
            javaFXFiles = javaFXFileObjects;
        }
        return javaFXFiles;
    }

     private static Collection<FileObject> getJavaFXFileObjects(FileObject dirFileObject, boolean isRecursive){
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

//    private boolean containsHandle(ElementLocation handle, CompilationInfo info) {
//        Element wanted = handle.getElement(info);
//        for (ElementLocation current : refactoring.getRefactoringSource().lookupAll(ElementLocation.class)) {
//            if (wanted == current.getElement(info)) {
//                return true;
//            }
//        }
//        return false;
//    }

    private WhereUsedQuery createQuery(ElementDef def) {
        FileObject refFo = getRefFO();
        Lookup lkp = refFo != null ? Lookups.fixed(def, refFo) : Lookups.fixed(def);
        WhereUsedQuery q = new WhereUsedQuery(lkp);
        for (Object o:refactoring.getContext().lookupAll(Object.class)) {
            q.getContext().add(o);
        }
        q.getContext().add(refactoring);
        q.getContext().add(RefactoringSupport.classModelFactory(refactoring));
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

    private FileObject getRefFO() {
        FileObject refFo = refactoring.getRefactoringSource().lookup(FileObject.class);
        if (refFo == null) {
            NonRecursiveFolder nrf = refactoring.getRefactoringSource().lookup(NonRecursiveFolder.class);
            if (nrf != null) {
                refFo = nrf.getFolder();
            }
        }
        if (refFo == null) {
            refFo = refactoring.getContext().lookup(FileObject.class);
        }
        return refFo;
    }

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
