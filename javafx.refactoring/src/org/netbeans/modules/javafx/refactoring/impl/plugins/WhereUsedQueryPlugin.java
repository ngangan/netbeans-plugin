/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Sun Microsystems, Inc. All rights reserved.
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

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.ElementKind;
import org.netbeans.api.java.source.TreePathHandle;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.refactoring.RefactoringSupport;
import org.netbeans.modules.javafx.refactoring.impl.WhereUsedElement;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.repository.ClassModel;
import org.netbeans.modules.javafx.refactoring.repository.ElementDef;
import org.netbeans.modules.javafx.refactoring.repository.Usage;
import org.netbeans.modules.refactoring.api.Problem;
import org.netbeans.modules.refactoring.api.WhereUsedQuery;
import org.netbeans.modules.refactoring.java.api.WhereUsedQueryConstants;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
import org.openide.filesystems.FileObject;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class WhereUsedQueryPlugin extends JavaFXRefactoringPlugin {
    private WhereUsedQuery refactoring;

    public WhereUsedQueryPlugin(WhereUsedQuery refactoring) {
        this.refactoring = refactoring;
    }

    public Problem checkParameters() {
        return null;
    }

    public Problem fastCheckParameters() {
        return null;
    }

    public Problem preCheck() {
        return null;
    }

    public Problem prepare(RefactoringElementsBag reb) {
        fireProgressListenerStart(WhereUsedQuery.PREPARE, -1);
        final ElementDef edef = getElementDef();

        final ClassIndex ci = RefactoringSupport.classIndex(refactoring);

        switch(edef.getKind()) {
            case METHOD: {
                collectMethodUsages(edef, reb, ci);
                break;
            }
            case FIELD:
            case PARAMETER:
            case LOCAL_VARIABLE:
            case ENUM_CONSTANT: {
                collectFieldUsages(edef, reb, ci);
                break;
            }
            case CLASS:
            case INTERFACE:
            case ENUM: {
                collectTypeUsages(edef, reb, ci);
                break;
            }
        }
        fireProgressListenerStop();
        return null;
    }

    private void collectMethodUsages(ElementDef edef, RefactoringElementsBag reb, final ClassIndex ci) {
        if (isCancelled()) return;
        
        final List<ElementDef> edefList = new ArrayList<ElementDef>();

        edefList.add(edef);
        if (isSearchFromBaseClass()) {
            for(ElementDef overridden : edef.getOverridden()) {
                edefList.add(overridden);
                if (!overridden.isOverriding()) {
                    edef = overridden;
                    break;
                }
            }
        }
        
        for(ElementDef checking : edefList) {
            if (isCancelled()) return;
            if (isFindUsages()) {
                fireProgressListenerStart(WhereUsedQuery.INIT, 1);
                Set<FileObject> files = ci.getResources(
                    checking.createHandle(),
                    EnumSet.of(ClassIndex.SearchKind.METHOD_REFERENCES),
                    EnumSet.allOf(ClassIndex.SearchScope.class));
                fireProgressListenerStop();
                fireProgressListenerStart(WhereUsedQuery.PREPARE, files.size());
                for(FileObject fo : files)  {
                    fireProgressListenerStep();
                    if (!SourceUtils.isJavaFXFile(fo)) continue;
                    
                    ClassModel cm = RefactoringSupport.classModelFactory(refactoring).classModelFor(fo);
                    for(Usage usg : cm.getUsages(checking)) {
                        // include only references; no declarations
                        if (usg.getStartPos() != usg.getDef().getStartFQN()) {
                            reb.add(refactoring, WhereUsedElement.create(usg));
                        }
                    }
                    fireProgressListenerStep();
                }
                fireProgressListenerStop();
            }
            if (isFindOverridingMethods()) {
                ElementHandle teh = null;
                if (checking.getKind().isClass() || checking.getKind().isInterface()) {
                    teh = checking.createHandle();
                } else {
                    ElementHandle eh = checking.createHandle();
                    teh = new ElementHandle(ElementKind.CLASS, new String[]{eh.getSignatures()[0]});
                }
                fireProgressListenerStart(WhereUsedQuery.INIT, 1);
                Set<FileObject> files = ci.getDependencyClosure(teh);
                fireProgressListenerStop();
                fireProgressListenerStart(WhereUsedQuery.PREPARE, files.size());
                for(FileObject fo : files) {
                    fireProgressListenerStep();
                    if (!SourceUtils.isJavaFXFile(fo)) continue;
                    ClassModel cm = RefactoringSupport.classModelFactory(refactoring).classModelFor(fo);
                    for(ElementDef refDef : cm.getElementDefs(EnumSet.of(edef.getKind()))) {
                        if (refDef.overrides(edef)) {
                            reb.add(refactoring, WhereUsedElement.create(cm.getDefaultUsage(refDef)));
                        }
                     }
                }
                fireProgressListenerStop();
            }
        }
    }

    private void collectFieldUsages(final ElementDef edef, RefactoringElementsBag reb, final ClassIndex ci) {
        if (isCancelled()) return;
        
        if (!edef.isIndexable()) {
            // local references
            ClassModel cm = getClassModel();
            for(Usage usg : cm.getUsages(edef)) {
                if (usg.getStartPos() != usg.getDef().getStartFQN()) {
                    reb.add(refactoring, WhereUsedElement.create(usg));
                }
            }
            return;
        }
        final Set<FileObject> references = ci.getResources(
            edef.createHandle(),
            EnumSet.of(ClassIndex.SearchKind.FIELD_REFERENCES),
            EnumSet.allOf(ClassIndex.SearchScope.class));

        ElementHandle teh = null;
        if (edef.getKind().isClass() || edef.getKind().isInterface()) {
            teh = edef.createHandle();
        } else {
            ElementHandle eh = edef.createHandle();
            teh = new ElementHandle(ElementKind.CLASS, new String[]{eh.getSignatures()[0]});
        }
        if (isCancelled()) return;
        references.addAll(ci.getResources(teh, EnumSet.of(ClassIndex.SearchKind.IMPLEMENTORS), EnumSet.allOf(ClassIndex.SearchScope.class)));

        fireProgressListenerStart(WhereUsedQuery.PREPARE, references.size());
        for(FileObject ref : references) {
            if (isCancelled()) return;
            fireProgressListenerStep();
            if (!SourceUtils.isJavaFXFile(ref)) continue;
            ClassModel refcm = RefactoringSupport.classModelFactory(refactoring).classModelFor(ref);

            if (isFindUsages()) {
                for(Usage usg : refcm.getUsages(edef)) {
                    if (usg.getStartPos() != usg.getDef().getStartFQN()) {
                        reb.add(refactoring, WhereUsedElement.create(usg));
                    }
                }
            }
        }
        fireProgressListenerStop();
    }

    private void collectTypeUsages(final ElementDef edef, RefactoringElementsBag reb, final ClassIndex ci) {
        if (isCancelled()) return;

        Set<FileObject> references = new HashSet<FileObject>();

        if (isFindUsages()) {
            references.addAll(ci.getResources(
                edef.createHandle(),
                EnumSet.of(ClassIndex.SearchKind.TYPE_REFERENCES),
                EnumSet.allOf(ClassIndex.SearchScope.class))
            );
        } else if (isFindDirectSubclassesOnly() || isFindSubclasses()) {
            references.addAll(ci.getDependencyClosure(edef.createHandle()));
        }

        Map<ElementDef, Set<FileObject>> workset = new HashMap<ElementDef, Set<FileObject>>();

        Set<ElementDef> checked = new HashSet<ElementDef>();

        workset.put(edef, references);

        while (!workset.isEmpty()) {
            if (isCancelled()) return;
            
            ElementDef checking = workset.keySet().iterator().next();
            Set<FileObject> files = workset.remove(checking);

            if (checked.contains(checking)) continue;
            fireProgressListenerStart(WhereUsedQuery.PREPARE, files.size());
            for(FileObject ref : files) {
                fireProgressListenerStep();
                if (!SourceUtils.isJavaFXFile(ref)) continue;
                
                ClassModel refcm = RefactoringSupport.classModelFactory(refactoring).classModelFor(ref);

                Usage.Kind kind = Usage.Kind.REFERENCE;
                if (isFindSubclasses() || isFindDirectSubclassesOnly()) {
                    kind = Usage.Kind.SUBTYPE;
                }
                for(Usage usg : refcm.getUsages(checking, EnumSet.of(kind))) {
                    if (usg.getStartPos() != usg.getDef().getStartFQN()) {
                        reb.add(refactoring, WhereUsedElement.create(usg));
                    }
                }
                if (isFindSubclasses()) {
                    for(ElementDef typeRef : refcm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE, ElementKind.ENUM))) {
                        if (typeRef.overrides(checking)) {
                            workset.put(typeRef, ci.getResources(
                                typeRef.createHandle(),
                                EnumSet.of(ClassIndex.SearchKind.IMPLEMENTORS),
                                EnumSet.allOf(ClassIndex.SearchScope.class))
                            );
                        }
                    }
                }
                checked.add(checking);
            }
            fireProgressListenerStop();
        }
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
        return refactoring.getBooleanValue(WhereUsedQueryConstants.SEARCH_FROM_BASECLASS);
    }

    private boolean isSearchInComments() {
        return refactoring.getBooleanValue(WhereUsedQuery.SEARCH_IN_COMMENTS);
    }

    private ClassModel getClassModel() {
        FileObject fo = getRefactoringFO();
        if (fo != null) {
            return RefactoringSupport.classModelFactory(refactoring).classModelFor(fo);
        }
        return null;
    }

    private FileObject refFo = null;
    synchronized private FileObject getRefactoringFO() {
        if (refFo == null) {
            refFo = refactoring.getRefactoringSource().lookup(FileObject.class);
            if (refFo == null) {
                refFo = refactoring.getContext().lookup(FileObject.class);
            }
            if (refFo == null) {
                TreePathHandle tph = refactoring.getRefactoringSource().lookup(TreePathHandle.class);
                if (tph != null) {
                    refFo = tph.getFileObject();
                    refactoring.getContext().add(refFo);
                }
            }
        }
        return refFo;
    }

    private ElementDef refdef = null;
    synchronized private ElementDef getElementDef() {
        if (refdef == null) {
            refdef = refactoring.getRefactoringSource().lookup(ElementDef.class);
            if (refdef == null) {
                final TreePathHandle tph = refactoring.getRefactoringSource().lookup(TreePathHandle.class);
                refdef = RefactoringSupport.fromJava(tph);
            }
        }
        return refdef;
    }
}
