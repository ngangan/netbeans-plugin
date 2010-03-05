/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.impl.plugins;

import java.io.IOException;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.ElementUtilities;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.modules.javafx.refactoring.impl.WhereUsedElement;
import org.netbeans.modules.javafx.refactoring.impl.WhereUsedQueryConstants;
import org.netbeans.modules.javafx.refactoring.repository.ClassModel;
import org.netbeans.modules.javafx.refactoring.repository.ClassModelFactory;
import org.netbeans.modules.javafx.refactoring.repository.ElementDef;
import org.netbeans.modules.javafx.refactoring.repository.Usage;
import org.netbeans.modules.refactoring.api.Problem;
import org.netbeans.modules.refactoring.api.WhereUsedQuery;
import org.netbeans.modules.refactoring.spi.ProgressProviderAdapter;
import org.netbeans.modules.refactoring.spi.RefactoringElementImplementation;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
import org.netbeans.modules.refactoring.spi.RefactoringPlugin;
import org.netbeans.modules.refactoring.spi.Transaction;
import org.openide.filesystems.FileObject;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class WhereUsedQueryPlugin extends ProgressProviderAdapter implements RefactoringPlugin {
    private WhereUsedQuery refactoring;

    final private ClassModelFactory factory;

    public WhereUsedQueryPlugin(WhereUsedQuery refactoring) {
        this.refactoring = refactoring;
        factory = this.refactoring.getContext().lookup(ClassModelFactory.class);
    }

    public void cancelRequest() {
        //
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
        final ElementDef edef = refactoring.getRefactoringSource().lookup(ElementDef.class);

        FileObject srcFo = refactoring.getRefactoringSource().lookup(FileObject.class);
        if (srcFo == null) {
            srcFo = refactoring.getContext().lookup(FileObject.class);
        }

        ClassModel cm = factory.classModelFor(srcFo);

//        if (isFindUsages()) {
//            for(Usage usg : cm.getUsages(edef)) {
//                // include only usages; no declarations
//                if (usg.getStartPos() != usg.getDef().getStartFQN()) {
//                    reb.add(refactoring, WhereUsedElement.create(usg));
//                }
//            }
//        }

        ClasspathInfo cpInfo = ClasspathInfo.create(srcFo);
        final ClassIndex ci = cpInfo.getClassIndex();

        switch(edef.getKind()) {
            case METHOD: {
                collectMethodUsages(edef, reb, ci);
                break;
            }
            case FIELD:
            case PARAMETER:
            case LOCAL_VARIABLE:
            case ENUM_CONSTANT: {
                collectFieldUsages(edef, cm, reb, ci);
                break;
            }
            case CLASS:
            case INTERFACE:
            case ENUM: {
                collectTypeUsages(edef, reb, ci);
                break;
            }
        }

        return null;
    }

    private void collectMethodUsages(ElementDef edef, RefactoringElementsBag reb, final ClassIndex ci) {
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
            if (isFindUsages()) {
                for(FileObject fo : ci.getResources(
                    checking.createHandle(),
                    EnumSet.of(ClassIndex.SearchKind.METHOD_REFERENCES),
                    EnumSet.allOf(ClassIndex.SearchScope.class))
                ) {
                    ClassModel cm = factory.classModelFor(fo);
                    for(Usage usg : cm.getUsages(checking)) {
                        // include only references; no declarations
                        if (usg.getStartPos() != usg.getDef().getStartFQN()) {
                            reb.add(refactoring, WhereUsedElement.create(usg));
                        }
                    }
                }
            }
            if (isFindOverridingMethods()) {
                Element e = checking.getElement();
                TypeElement te = ElementUtilities.enclosingTypeElement(e);
                for(FileObject fo : ci.getResources(
                    ElementHandle.create(te),
                    EnumSet.of(ClassIndex.SearchKind.IMPLEMENTORS),
                    EnumSet.allOf(ClassIndex.SearchScope.class))
                ) {
                    ClassModel cm = factory.classModelFor(fo);
                    for(ElementDef refdef : cm.getElementDefs(EnumSet.of(edef.getKind()))) {
                        if (refdef.overrides(edef)) {
                            reb.add(refactoring, WhereUsedElement.create(cm.getDefaultUsage(refdef)));
                        }
                     }
                }
            }
        }
    }

    private void collectFieldUsages(final ElementDef edef, ClassModel cm, RefactoringElementsBag reb, final ClassIndex ci) {
        if (!edef.isIndexable()) {
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

        TypeElement te = ElementUtilities.enclosingTypeElement(edef.getElement());
        references.addAll(ci.getResources(ElementHandle.create(te), EnumSet.of(ClassIndex.SearchKind.IMPLEMENTORS), EnumSet.allOf(ClassIndex.SearchScope.class)));

        for(FileObject ref : references) {
            ClassModel refcm = factory.classModelFor(ref);

            if (isFindUsages()) {
                for(Usage usg : refcm.getUsages(edef)) {
                    reb.add(refactoring, WhereUsedElement.create(usg));
                }
            }
        }
    }

    private void collectTypeUsages(final ElementDef edef, RefactoringElementsBag reb, final ClassIndex ci) {
        Set<FileObject> references = new HashSet<FileObject>();

        if (isFindUsages()) {
            references.addAll(ci.getResources(
                edef.createHandle(),
                EnumSet.of(ClassIndex.SearchKind.TYPE_REFERENCES),
                EnumSet.allOf(ClassIndex.SearchScope.class))
            );
        } else if (isFindDirectSubclassesOnly() || isFindSubclasses()) {
            references.addAll(ci.getResources(
                edef.createHandle(),
                EnumSet.of(ClassIndex.SearchKind.IMPLEMENTORS),
                EnumSet.allOf(ClassIndex.SearchScope.class))
            );
        }

        Map<ElementDef, Set<FileObject>> workset = new HashMap<ElementDef, Set<FileObject>>();

        Set<ElementDef> checked = new HashSet<ElementDef>();

        workset.put(edef, references);

        while (!workset.isEmpty()) {
            ElementDef checking = workset.keySet().iterator().next();
            Set<FileObject> files = workset.remove(checking);

            if (checked.contains(checking)) continue;

            for(FileObject ref : files) {
                ClassModel refcm = factory.classModelFor(ref);

                Usage.Kind kind = Usage.Kind.REFERENCE;
                if (isFindSubclasses() || isFindDirectSubclassesOnly()) {
                    kind = Usage.Kind.SUBTYPE;
                }
                for(Usage usg : refcm.getUsages(checking, EnumSet.of(kind))) {
                    reb.add(refactoring, WhereUsedElement.create(usg));
                }
                if (isFindSubclasses()) {
                    for(ElementDef typeRef : refcm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE))) {
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
}
