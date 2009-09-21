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
package org.netbeans.modules.javafx.refactoring.impl.plugins;

import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.JavaFXTreeScanner;
import com.sun.javafx.api.tree.Tree.JavaFXKind;
import java.io.IOException;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.JavaFXSourceUtils;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.modules.javafx.refactoring.impl.WhereUsedQueryConstants;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.TreePathHandle;
import org.netbeans.modules.javafx.refactoring.impl.scanners.FindOverridersScanner;
import org.netbeans.modules.javafx.refactoring.impl.scanners.FindSubclassesScanner;
import org.netbeans.modules.javafx.refactoring.impl.scanners.FindUsagesScanner;
import org.netbeans.modules.refactoring.api.Problem;
import org.netbeans.modules.refactoring.api.WhereUsedQuery;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
import org.netbeans.modules.refactoring.spi.RefactoringPlugin;
import org.openide.filesystems.FileObject;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.lookup.Lookups;

/**
 * Actual implementation of Find Usages query search for Ruby
 * 
 * @todo Perform index lookups to determine the set of files to be checked!
 * @todo Scan comments!
 * @todo Do more prechecks of the elements we're trying to find usages for
 * 
 * @author  Jaroslav Bachorik
 */
public class WhereUsedQueryPlugin implements RefactoringPlugin {
    private final WhereUsedQuery refactoring;
    private final TreePathHandle searchHandle;
//    private Set<IndexedClass> subclasses;
    private final String targetName;

    volatile private boolean cancelled = false;

    /** Creates a new instance of WhereUsedQuery */
    public WhereUsedQueryPlugin(WhereUsedQuery refactoring) {
        this.refactoring = refactoring;
        this.searchHandle = refactoring.getRefactoringSource().lookup(TreePathHandle.class);
        targetName = searchHandle.getSimpleName();
    }

    public void cancelRequest() {
        cancelled = true;
    }
    
    @Override
    public Problem preCheck() {
        if (!searchHandle.getFileObject().isValid()) {
            return new Problem(true, NbBundle.getMessage(WhereUsedQueryPlugin.class, "DSC_ElNotAvail")); // NOI18N
        }
        
        return null;
    }
    
    //@Override
    public Problem prepare(final RefactoringElementsBag elements) {
        JavaFXSource jfxs = JavaFXSource.forFileObject(searchHandle.getFileObject());

        try {
            final Set<FileObject> relevantFiles = new HashSet<FileObject>();
            final Set<ElementHandle> relevantHandles = new HashSet<ElementHandle>();

            jfxs.runUserActionTask(new Task<CompilationController>() {

                public void run(CompilationController cc) throws Exception {
                    ClassIndex ci = cc.getClasspathInfo().getClassIndex();
                    Element e = searchHandle.resolveElement(cc);
                    ElementHandle eh = ElementHandle.create(e);

                    if (isFindUsages()) {
                        relevantHandles.add(eh);
                        collectReferences(ElementHandle.create(e), ci, relevantFiles);
                    }
                    if (isFindDirectSubclassesOnly() || isFindSubclasses() || isFindOverridingMethods()) {
                        Stack<ElementHandle> processingStack = new Stack();
                        processingStack.push(eh);
                        while(!processingStack.empty()) {
                            ElementHandle currentHandle = processingStack.pop();
                            for(ElementHandle eh1 : ci.getElements(currentHandle, EnumSet.of(ClassIndex.SearchKind.IMPLEMENTORS), EnumSet.allOf(ClassIndex.SearchScope.class))) {
                                if (!relevantHandles.contains(eh1)) {
                                    if (isFindSubclasses() || isFindOverridingMethods()) {
                                        processingStack.push(eh1);
                                    }
                                    relevantHandles.add(eh1);
                                    if (isFindOverridingMethods()) {
                                        collectReferences(eh1, ci, relevantFiles);
                                    }
                                    if (isFindSubclasses() || isFindDirectSubclassesOnly()) {
                                        relevantFiles.add(JavaFXSourceUtils.getFile(eh1, cc.getClasspathInfo()));
                                    }
                                }
                            }
                        }
                    }
                }
            }, true);

            for(FileObject fo : relevantFiles) {
                if (fo == null) continue; // prevent NPE in case of a corrupted index
                JavaFXSource src = JavaFXSource.forFileObject(fo);
                if (src != null) {
                    src.runUserActionTask(new Task<CompilationController>() {

                        public void run(final CompilationController cc) throws Exception {
                            for(ElementHandle eh : relevantHandles) {
                                if (isFindOverridingMethods()) {
                                    new FindOverridersScanner(refactoring, searchHandle, cc).scan(cc.getCompilationUnit(), elements);
                                }
                                if (isFindUsages()) {
                                    new FindUsagesScanner(refactoring, searchHandle, eh, cc).scan(cc.getCompilationUnit(), elements);
                                }
                                if (isFindDirectSubclassesOnly() || isFindSubclasses()) {
                                    new FindSubclassesScanner(refactoring, searchHandle, eh, cc).scan(cc.getCompilationUnit(), elements);
                                }
                            }
                        }
                    }, true);
                }
            }
        } catch (IOException e) {
            return new Problem(true, e.getLocalizedMessage());
        }
        
        return null;
    }
    
    public Problem fastCheckParameters() {
        if (targetName == null) {
            return new Problem(true, "Cannot determine target name. Please file a bug with detailed information on how to reproduce (preferably including the current source file and the cursor position)");
        }
        if (searchHandle.getKind() == JavaFXKind.METHOD_INVOCATION || searchHandle.getKind() == JavaFXKind.FUNCTION_DEFINITION) {
            return checkParametersForMethod(isFindOverridingMethods(), isFindUsages());
        } 
        return null;
    }
    
    public Problem checkParameters() {
        return null;
    }

    private void collectReferences(ElementHandle handle, ClassIndex ci, Set<FileObject> references) {
        switch (handle.getKind()) {
            case CLASS: {
                references.addAll(ci.getResources(handle, EnumSet.of(ClassIndex.SearchKind.TYPE_REFERENCES), EnumSet.allOf(ClassIndex.SearchScope.class)));
                break;
            }
            case METHOD: {
                references.addAll(ci.getResources(handle, EnumSet.of(ClassIndex.SearchKind.METHOD_REFERENCES), EnumSet.allOf(ClassIndex.SearchScope.class)));
                break;
            }
            case FIELD: {
                references.addAll(ci.getResources(handle, EnumSet.of(ClassIndex.SearchKind.FIELD_REFERENCES), EnumSet.allOf(ClassIndex.SearchScope.class)));
                break;
            }
        }
    }

    private void collectImplementors(ElementHandle handle, ClassIndex ci, Set<FileObject> implementors) {
        switch (handle.getKind()) {
            case CLASS: {
                implementors.addAll(ci.getResources(handle, EnumSet.of(ClassIndex.SearchKind.IMPLEMENTORS), EnumSet.allOf(ClassIndex.SearchScope.class)));
                break;
            }
        }
    }

    private Problem checkParametersForMethod(boolean overriders, boolean usages) {
        if (!(usages || overriders)) {
            return new Problem(true, NbBundle.getMessage(WhereUsedQueryPlugin.class, "MSG_NothingToFind"));
        } else
            return null;
    }
        
    private boolean isFindSubclasses() {
        return refactoring.getBooleanValue(WhereUsedQueryConstants.FIND_SUBCLASSES);
    }
    private boolean isFindUsages() {
        return refactoring.getBooleanValue(WhereUsedQuery.FIND_REFERENCES);
    }
    private boolean isFindDirectSubclassesOnly() {
        return refactoring.getBooleanValue(WhereUsedQueryConstants.FIND_DIRECT_SUBCLASSES);
    }
    
    private boolean isFindOverridingMethods() {
        return refactoring.getBooleanValue(WhereUsedQueryConstants.FIND_OVERRIDING_METHODS);
    }

    private boolean isSearchFromBaseClass() {
        return false;
    }

    private boolean isSearchInComments() {
        return refactoring.getBooleanValue(WhereUsedQuery.SEARCH_IN_COMMENTS);
    }
}
