/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * Portions Copyrighted 1997-2008 Sun Microsystems, Inc.
 */

package org.netbeans.modules.visage.refactoring.impl.plugins;

import com.sun.visage.api.tree.VisageTreePath;
import com.sun.visage.api.tree.Scope;
import com.sun.visage.api.tree.Tree;
import java.io.IOException;
import java.util.Collection;
import java.util.EnumSet;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.ElementFilter;
import org.netbeans.api.java.source.TreePathHandle;
import org.netbeans.api.visage.source.ClassIndex;
import org.netbeans.api.visage.source.ClasspathInfo;
import org.netbeans.api.visage.source.CompilationController;
import org.netbeans.api.visage.source.CompilationInfo;
import org.netbeans.api.visage.source.ElementHandle;
import org.netbeans.api.visage.source.ElementUtilities;
import org.netbeans.api.visage.source.VisageSource;
import org.netbeans.api.visage.source.Task;
import org.netbeans.modules.visage.refactoring.RefactoringSupport;
import org.netbeans.modules.visage.refactoring.impl.visagec.SourceUtils;
import org.netbeans.modules.visage.refactoring.impl.plugins.elements.BaseRefactoringElementImplementation;
import org.netbeans.modules.visage.refactoring.impl.plugins.elements.ReindexFileElement;
import org.netbeans.modules.visage.refactoring.impl.plugins.elements.RenameInCommentsElement;
import org.netbeans.modules.visage.refactoring.impl.plugins.elements.RenameOccurencesElement;
import org.netbeans.modules.visage.refactoring.impl.scanners.LocalVarScanner;
import org.netbeans.modules.visage.refactoring.repository.ClassModel;
import org.netbeans.modules.visage.refactoring.repository.ElementDef;
import org.netbeans.modules.visage.refactoring.repository.GlobalDef;
import org.netbeans.modules.visage.refactoring.repository.Usage;
import org.netbeans.modules.visage.refactoring.transformations.ReplaceTextTransformation;
import org.netbeans.modules.visage.refactoring.transformations.Transformation;
import org.netbeans.modules.refactoring.api.Problem;
import org.netbeans.modules.refactoring.api.RenameRefactoring;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
import org.openide.cookies.SaveCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class RenameRefactoringPlugin extends VisageRefactoringPlugin {
    private RenameRefactoring refactoring;

    public RenameRefactoringPlugin(RenameRefactoring refactoring) {
        this.refactoring = refactoring;
        TreePathHandle tph = refactoring.getRefactoringSource().lookup(TreePathHandle.class);
        if (tph == null) {
            FileObject srcFo = getRefactoringFO();

            // make sure the file image on disk is up-to-date
            try {
                DataObject dobj = DataObject.find(srcFo);
                SaveCookie sc = dobj.getCookie(SaveCookie.class);
                if (sc != null) {
                    sc.save();
                }
            } catch (IOException iOException) {
            }
        }
    }

    

    public Problem checkParameters() {
        FileObject fo = getRefactoringFO();
        if (!SourceUtils.isVisageFile(fo)) return null;
        
        fireProgressListenerStart(RenameRefactoring.PARAMETERS_CHECK, 4);
        final ElementDef edef = getElementDef();
        if (edef == null) return null;

        final Problem p[] = new Problem[1];

        String msg = null;
        if (edef.getKind() == ElementKind.METHOD && edef.isOverriding()) {
            p[0] = chainProblems(p[0], new Problem(false, NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_Overrides"))); // NOI18N
        }

        fireProgressListenerStep();

        if (edef.getKind().isClass()) {
            if (!edef.getNestingKind().isNested()) {
                ClasspathInfo cpInfo = refactoring.getContext().lookup(ClasspathInfo.class);
                ClassIndex ci = cpInfo.getClassIndex();
                Set<FileObject> typeDefFOs = ci.getResources(edef.createHandle(), EnumSet.of(ClassIndex.SearchKind.TYPE_DEFS), EnumSet.allOf(ClassIndex.SearchScope.class));
                FileObject typeDefFO = typeDefFOs.iterator().next();

                String oldFqn = edef.createHandle().getQualifiedName();
                int pkgDelimitIndex = oldFqn.lastIndexOf(edef.getName());
                String pkgname = oldFqn.substring(0, pkgDelimitIndex > 0 ? pkgDelimitIndex - 1 : 0);
                String newFqn = pkgname + "." + refactoring.getNewName(); // NOI18N

                if (!ci.getDeclaredTypes(newFqn, ClassIndex.NameKind.EXACT, EnumSet.allOf(ClassIndex.SearchScope.class)).isEmpty()) {
                    msg = NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_ClassClash", new Object[] {refactoring.getNewName(), pkgname});
                }
                FileObject parentFolder = typeDefFO.getParent();
                Enumeration enumeration = parentFolder.getFolders(false);
                while (enumeration.hasMoreElements()) {
                    FileObject subfolder = (FileObject) enumeration.nextElement();
                    if (subfolder.getName().equals(refactoring.getNewName())) {
                        msg = NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_ClassPackageClash", new Object[] {refactoring.getNewName(), pkgname});
                    }
                }
            }
        }
        fireProgressListenerStep();
        if (edef.getKind().isClass() || edef.getKind().isInterface()) { // #187336: Only do the filename check when renaming a type
            FileObject primFile = fo;
            FileObject folder = primFile.getParent();
            FileObject existing = folder.getFileObject(refactoring.getNewName(), primFile.getExt());
            if (existing != null && primFile != existing) {
                // primFile != existing is check for case insensitive filesystems; #136434
                msg = NbBundle.getMessage(RenameRefactoringPlugin.class,
                        "ERR_ClassClash", refactoring.getNewName(), folder.getPath()); // NOI18N
            }
        }
        fireProgressListenerStep();
        if (msg == null) {
            if (edef.getKind() == ElementKind.LOCAL_VARIABLE || edef.getKind() == ElementKind.PARAMETER) {
                msg = variableClashes();
            } else {
                msg = clashes();
            }
        }
        fireProgressListenerStep();
        if (msg != null) {
            p[0] = chainProblems(p[0], new Problem(true, msg));
        }
        
        fireProgressListenerStop();
        
        return p[0];
    }

    public Problem fastCheckParameters() {
        FileObject fo = getRefactoringFO();
        if (!SourceUtils.isVisageFile(fo)) return null;
        
        ClassModel cm = getClassModel();

        final ElementDef edef = getElementDef();
        if (edef == null) return null;
        
        Problem p = null;
        if (edef.getName().equals(refactoring.getNewName())) {
            p = chainProblems(p, new Problem(true, NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_NameNotChanged"))); // NOI18N
        }

        if (!Utilities.isJavaIdentifier(refactoring.getNewName())) {
            String msg = edef.getKind() == ElementKind.PACKAGE?
                NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_InvalidPackage", new Object[]{refactoring.getNewName()}) :
                NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_InvalidIdentifier", new Object[]{refactoring.getNewName()}); //NOI18N

            p = chainProblems(p, new Problem(true, msg));
        }

        for(ElementDef ed : cm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE))) {
            if (ed.getName().equals(refactoring.getNewName())) {
                p = chainProblems(p, new Problem(true, NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_CanNotRenameExisting", refactoring.getNewName())));
            }
        }

        return p;
    }

    public Problem preCheck() {
        FileObject fo = getRefactoringFO();
        if (!SourceUtils.isVisageFile(fo)) return null;
        
        final ElementDef edef = getElementDef();
        if (edef == null) return null;

        if (!edef.isFromSource()) { // the element definition is not from sources
            return new Problem(true, NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_CannotRefactorLibraryClass", SourceUtils.getEnclosingTypeName(edef)));
        }
        
        final Problem p[] = new Problem[1];

        fireProgressListenerStart(RenameRefactoring.PRE_CHECK, 4);

        final Element el = edef.getElement();
        VisageSource jfxs = VisageSource.forFileObject(fo);
        try {
            jfxs.runUserActionTask(new Task<CompilationController>() {

                public void run(CompilationController cc) throws Exception {

                    if (el != null) {
                        switch (el.getKind()) {
                            case METHOD: {
                                fireProgressListenerStep();
                                fireProgressListenerStep();
                                Collection<ExecutableElement> overriddenByMethods = SourceUtils.getOverridingMethods((ExecutableElement)el, cc);
                                fireProgressListenerStep();
                                if (el.getModifiers().contains(Modifier.NATIVE)) {
                                    p[0] = chainProblems(p[0], new Problem(false, NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_RenameNative", el)));
                                }
                                if (!overriddenByMethods.isEmpty()) {
                                    String msg = NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_IsOverridden",
                                            new Object[] {ElementUtilities.enclosingTypeElement(el).getSimpleName().toString()});
                                    p[0] = chainProblems(p[0], new Problem(false, msg));
                                }
                                for (ExecutableElement e : overriddenByMethods) {
                                    if (e.getModifiers().contains(Modifier.NATIVE)) {
                                        p[0] = chainProblems(p[0], new Problem(false, NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_RenameNative", e)));
                                    }
                                }
                                Collection<ExecutableElement> overridesMethods = SourceUtils.getOverridenMethods((ExecutableElement)el, cc);
                                fireProgressListenerStep();
                                if (!overridesMethods.isEmpty()) {
                                    boolean fatal = false;
                                    for (ExecutableElement method : overridesMethods) {
                                        if (method.getModifiers().contains(Modifier.NATIVE)) {
                                            p[0] = chainProblems(p[0], new Problem(false, NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_RenameNative", method))); // NOI18N
                                        }
                                        Element parentClz = method.getEnclosingElement();
                                        while (parentClz != null && !parentClz.getKind().isClass() && !parentClz.getKind().isInterface()) {
                                            parentClz = parentClz.getEnclosingElement();
                                        }
                                        if (parentClz == null || SourceUtils.isFromLibrary(parentClz, cc)) {
                                            fatal = true;
                                            break;
                                        }
                                    }
                                    String msg = NbBundle.getMessage(RenameRefactoringPlugin.class, fatal?"ERR_Overrides_Fatal":"ERR_Overrides"); // NOI18N
                                    p[0] = chainProblems(p[0], new Problem(fatal, msg));
                                    fireProgressListenerStep();
                                }
                                break;
                            }
                        }
                    } else {
                        p[0] = chainProblems(p[0], new Problem(true, NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_ErroneousSource", cc.getFileObject().getPath())));
                    }
                }
            }, true);
        } catch (IOException iOException) {
        }
        
        fireProgressListenerStop();
        return p[0];
    }

    public Problem prepare(RefactoringElementsBag bag) {
        fireProgressListenerStart(RenameRefactoring.INIT, 6);
        final ElementDef edef = getElementDef();
        fireProgressListenerStep();

        if (edef == null) return null; // fail early

        final FileObject fo = getRefactoringFO();
        ClassIndex ci = RefactoringSupport.classIndex(refactoring);

        fireProgressListenerStep();

        final Set<FileObject> files = new HashSet<FileObject>();
        if (SourceUtils.isVisageFile(fo)) {
            files.add(fo);
        }

        if (edef.isIndexable()) {
            Set<ElementDef> edefs = new HashSet<ElementDef>();
            edefs.add(edef);
            if (edef.getKind() == ElementKind.METHOD) {
                edefs.addAll(edef.getOverridden());
            }
            for(ElementDef ed : edefs) {
                if (isCancelled()) return null;
                ElementHandle eh = ed.createHandle();
                files.addAll(ci.getResources(
                        eh,
                        EnumSet.of(ClassIndex.SearchKind.TYPE_REFERENCES, ClassIndex.SearchKind.TYPE_DEFS, ClassIndex.SearchKind.METHOD_REFERENCES, ClassIndex.SearchKind.FIELD_REFERENCES),
                        EnumSet.allOf(ClassIndex.SearchScope.class)));

                if (!(eh.getKind().isClass() || eh.getKind().isInterface())) {
                    eh = new ElementHandle(ElementKind.CLASS, new String[]{eh.getSignatures()[0]});
                }
                if (eh != null && (eh.getKind().isInterface() || eh.getKind().isClass())) {
                    files.addAll(ci.getDependencyClosure(eh));
                }
            }
        }
        fireProgressListenerStop();
        if (isCancelled()) return null;
        fireProgressListenerStart(RenameRefactoring.PREPARE, files.size());
        for(FileObject file : files) {
            if (isCancelled()) return null;
            fireProgressListenerStep();
            if (!SourceUtils.isVisageFile(file)) continue;

            RenameOccurencesElement updateRefs = new RenameOccurencesElement(edef.getName(), refactoring.getNewName(), file, bag.getSession()) {

                @Override
                protected Set<Transformation> prepareTransformations(FileObject fo) {
                    ClassModel localCm = RefactoringSupport.classModelFactory(refactoring).classModelFor(fo);

                    Set<Transformation> transformations = new HashSet<Transformation>();
                    Set<Usage> usages = new HashSet<Usage>();
                    usages.addAll(localCm.getUsages(edef, EnumSet.of(Usage.Kind.REFERENCE)));
                    if (edef.getKind() == ElementKind.METHOD) {
                        for(Usage usg : localCm.getUsages()) {
                            if (usg.getDef().getKind() == edef.getKind()) {
                                if (usg.getDef().overrides(edef) || edef.overrides(usg.getDef())) {
                                    usages.add(usg);
                                }
                            }
                        }
                    }
                    for(Usage usg : usages) {
                       transformations.add(new ReplaceTextTransformation(usg.getStartPos(), getOldName(), getNewName()));
                    }
                    return transformations;
                }
            };
            BaseRefactoringElementImplementation updateRefsInComment = null;
            if (refactoring.isSearchInComments()) {
                updateRefsInComment = new RenameInCommentsElement(edef.getName(), refactoring.getNewName(), file, bag.getSession());
            }
            boolean needReindex = true;
            if (updateRefs.hasChanges()) {
                bag.add(refactoring, updateRefs);
                needReindex = false;
            }
            if (updateRefsInComment != null && updateRefsInComment.hasChanges()) {
                bag.add(refactoring, updateRefsInComment);
                needReindex = false;
            }
            if (needReindex && !file.equals(fo)) {
                bag.addFileChange(refactoring, new ReindexFileElement(file));
            }
        }
        fireProgressListenerStop();
        return null;
    }

    private String variableClashes() {
        final ElementDef edef = getElementDef();
        final FileObject srcFo = getRefactoringFO();
        if (edef == null) return null;

        VisageSource jfxs = VisageSource.forFileObject(srcFo);
        final String[] msg = new String[1];

        try {
            jfxs.runUserActionTask(new Task<CompilationController>() {

                public void run(CompilationController cc) throws Exception {
                    if (edef != null) {
                        Element var = cc.getElementUtilities().elementFor(edef.getStartPos());

                        VisageTreePath tp = cc.getPath(var);

                        LocalVarScanner lookup = new LocalVarScanner(cc, refactoring.getNewName());
                        VisageTreePath scopeBlok = tp;
                        EnumSet set = EnumSet.of(Tree.VisageKind.BLOCK_EXPRESSION, Tree.VisageKind.FOR_EXPRESSION_FOR, Tree.VisageKind.FUNCTION_DEFINITION, Tree.VisageKind.CLASS_DECLARATION);
                        while (scopeBlok != null && scopeBlok.getLeaf() != null && !set.contains(scopeBlok.getLeaf().getVisageKind())) {
                            scopeBlok = scopeBlok.getParentPath();
                        }
                        if (scopeBlok != null) {
                            lookup.scan(scopeBlok, var);

                            if (lookup.hasRefernces()) {
                                msg[0] = NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_LocVariableClash", new Object[]{refactoring.getNewName()}); // NOI18N
                                return;
                            }

                            Scope scope = null;
                            if (tp != null) {
                                scope = cc.getTrees().getScope(tp);
                                if (scope != null) {
                                    for (Element el : scope.getLocalElements()) {
                                        if (el.getSimpleName().toString().equals(refactoring.getNewName())) {
                                            msg[0] = NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_LocVariableClash", new Object[]{refactoring.getNewName()}); // NOI18N
                                            return;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }, true);
        } catch (IOException e) {
        }
        
        return msg[0];
    }

    private String clashes() {
        final ElementDef edef = getElementDef();
        if (edef == null) {
            return null;
        }
        
        Element feature = edef.getElement();

        Element dc = feature.getEnclosingElement();
        ElementKind kind = edef.getKind();
        final String newName = refactoring.getNewName();
        
        if (kind.isClass() || kind.isInterface()) {
            for (Element current:ElementFilter.typesIn(dc.getEnclosedElements())) {
                if (current.getSimpleName().toString().equals(newName)) {
                    return NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_InnerClassClash",new Object[] {newName,dc.getSimpleName()}); // NOI18N
                }
            }
        } else if (kind==ElementKind.METHOD) {
            if (ElementUtilities.alreadyDefinedIn((CharSequence) newName, (ExecutableElement) feature, (TypeElement) dc)) {
                return NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_MethodClash", new Object[] {newName,dc.getSimpleName()}); // NOI18N
            }
        } else if (kind.isField()) {
            for (Element current:ElementFilter.fieldsIn(dc.getEnclosedElements())) {
                if (current.getSimpleName().toString().equals(newName)) {
                    return NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_FieldClash", new Object[] {newName,dc.getSimpleName()}); // NOi18N
                }
            }
        }
        return null;
    }

    private static final Problem isSourceElement(TypeElement el, CompilationInfo info) {
        Problem preCheckProblem = null;
        if (SourceUtils.isFromLibrary(el, info)) { //NOI18N
            preCheckProblem = new Problem(true, NbBundle.getMessage(
                    RenameRefactoringPlugin.class, "ERR_CannotRefactorLibraryClass", // NOI18N
                    el
                    ));
            return preCheckProblem;
        }
        FileObject file = SourceUtils.getFile(el,info);
        // RetoucheUtils.isFromLibrary already checked file for null
        if (!SourceUtils.isFileInOpenProject(file)) {
            preCheckProblem =new Problem(true, NbBundle.getMessage(
                    RenameRefactoringPlugin.class,
                    "ERR_ProjectNotOpened", // NOI18N
                    FileUtil.getFileDisplayName(file)));
            return preCheckProblem;
        }
        return null;
    }

    private static final Problem isSourceElement(Element el, CompilationInfo info) {
        Element e = el;
        while(e != null && (!e.getKind().isClass() && !e.getKind().isInterface())) {
            e = e.getEnclosingElement();
        }
        return e != null ? isSourceElement((TypeElement)e, info) : null;
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
                if (tph != null) { // java infrastructure might not provide the TreePathHandle (#187458)
                    refdef = RefactoringSupport.fromJava(tph);
                }
            }
        }
        return refdef;
    }
}
