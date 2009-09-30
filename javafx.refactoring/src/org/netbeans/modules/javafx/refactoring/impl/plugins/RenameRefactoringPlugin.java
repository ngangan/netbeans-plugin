/*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 *  Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 * 
 *  The contents of this file are subject to the terms of either the GNU
 *  General Public License Version 2 only ("GPL") or the Common
 *  Development and Distribution License("CDDL") (collectively, the
 *  "License"). You may not use this file except in compliance with the
 *  License. You can obtain a copy of the License at
 *  http://www.netbeans.org/cddl-gplv2.html
 *  or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 *  specific language governing permissions and limitations under the
 *  License.  When distributing the software, include this License Header
 *  Notice in each file and include the License file at
 *  nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Sun in the GPL Version 2 section of the License file that
 *  accompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 * 
 *  Contributor(s):
 * 
 *  Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.refactoring.impl.plugins;

import org.netbeans.modules.javafx.refactoring.impl.scanners.RenameScanner;
import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.UnitTree;
import java.io.IOException;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClassIndex.SearchKind;
import org.netbeans.api.javafx.source.ClassIndex.SearchScope;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.modules.javafx.refactoring.impl.RenameRefactoringElement;
import org.netbeans.modules.javafx.refactoring.impl.TransformationContext;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.TreePathHandle;
import org.netbeans.modules.refactoring.api.Problem;
import org.netbeans.modules.refactoring.api.RenameRefactoring;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
import org.netbeans.modules.refactoring.spi.RefactoringPlugin;
import org.openide.filesystems.FileObject;
import org.netbeans.modules.refactoring.spi.RefactoringElementImplementation;
import org.openide.filesystems.FileUtil;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.lookup.Lookups;
import org.openide.util.lookup.ProxyLookup;

/**
 *
 * @author Jaroslav Bachorik
 */
public class RenameRefactoringPlugin implements RefactoringPlugin {
    private final AtomicBoolean requestCancelled = new AtomicBoolean(false);

    private TreePathHandle treePathHandle = null;
    private String fileName;
    private RenameRefactoring refactoring;

    public RenameRefactoringPlugin(RenameRefactoring rename) {
        this.refactoring = rename;
        TreePathHandle tph = rename.getRefactoringSource().lookup(TreePathHandle.class);
        if (tph!=null) {
            treePathHandle = tph;
        } else {
            JavaFXSource source = JavaFXSource.forFileObject(rename.getRefactoringSource().lookup(FileObject.class));
            try {
                source.runUserActionTask(new Task<CompilationController>() {
                    public void cancel() {
                    }

                    public void run(CompilationController co) throws Exception {
                        UnitTree cut = co.getCompilationUnit();
                        for (Tree t: cut.getTypeDecls()) {
                            Element e = co.getTrees().getElement(JavaFXTreePath.getPath(cut, t));
                            if (e!=null && e.getSimpleName().toString().equals(co.getFileObject().getName())) {
                                treePathHandle = TreePathHandle.create(JavaFXTreePath.getPath(cut, t), co);
                                refactoring.getContext().add(co);
                                break;
                            }
                        }
                    }
                }, false);
            } catch (IllegalArgumentException ex) {
                ex.printStackTrace();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
        if (treePathHandle != null) {
            fileName = treePathHandle.getFileObject().getName();
        }
    }

    public void cancelRequest() {
        requestCancelled.set(true);
    }

    public Problem checkParameters() {
        final String newElementName = refactoring.getNewName();

//        RenameRefactoringEx extraInfo = refactoring.getContext().lookup(RenameRefactoringEx.class);
//        if (extraInfo.isRenamingFile() || extraInfo.isFileNameInSync()) return null; // has been checked in fast-check

        final AtomicReference<Problem> problem = new AtomicReference<Problem>();

        JavaFXSource jfxs = JavaFXSource.forFileObject(treePathHandle.getFileObject());
        try {
            jfxs.runUserActionTask(new Task<CompilationController>() {
                public void run(final CompilationController cc) throws Exception {
                    switch(treePathHandle.getKind()) {
                        case CLASS_DECLARATION: {
                            TypeElement te = (TypeElement) treePathHandle.resolveElement(cc);
                            switch(te.getNestingKind()) {
                                case MEMBER: {
                                    final TypeElement parent = (TypeElement)te.getEnclosingElement();
                                    JavaFXTreePathScanner<Void, Void> scanner = new JavaFXTreePathScanner<Void, Void>() {
                                        @Override
                                        public Void visitClassDeclaration(ClassDeclarationTree node, Void p) {
                                            TypeElement current = (TypeElement)cc.getTrees().getElement(getCurrentPath());
                                            if (current.getEnclosingElement().equals(parent)) {
                                                if (current.getSimpleName().toString().equals(newElementName)) {
                                                    problem.set(new Problem(true, "Can not rename to " + current.getQualifiedName() + " - it already exists."));
                                                }
                                            }
                                            return super.visitClassDeclaration(node, p);
                                        }
                                    };
                                    scanner.scan(cc.getCompilationUnit(), null);
                                }
                            }
                            break;
                        }
                    }
                }
            }, true);
        } catch (IOException iOException) {
        }

        return problem.get();
    }

    public Problem fastCheckParameters() {
        final String newElementName = refactoring.getNewName();

        if (fileName.equals(treePathHandle.getSimpleName())) {
            return checkFileNameClash(newElementName, treePathHandle.getFileObject());
        }

        return null;
    }

    public Problem preCheck() {
        final Problem[] problem = new Problem[1];
        JavaFXSource jfxs = JavaFXSource.forFileObject(treePathHandle.getFileObject());
        try {
            jfxs.runWhenScanFinished(new Task<CompilationController>() {

                public void run(CompilationController info) throws Exception {
                    Element el = treePathHandle.resolveElement(info);
                    while (el != null && (el.getKind() != ElementKind.CLASS && el.getKind() != ElementKind.INTERFACE)) {
                        el = el.getEnclosingElement();
                    }
                    problem[0] = el != null ? isSourceElement(el, info) : null;
                }
            }, true);
        } catch (IOException e) {
            return new Problem(true, e.getLocalizedMessage());
        }

       return problem[0];
    }

    public Problem prepare(final RefactoringElementsBag bag) {
        Lookup l = refactoring.getRefactoringSource();
        final Set<TreePathHandle> references = new HashSet<TreePathHandle>();
        final Map<FileObject, TransformationContext> contextMap = new HashMap<FileObject, TransformationContext>();

//        references.add(treePathHandle);

        JavaFXSource jfxs = JavaFXSource.forFileObject(treePathHandle.getFileObject());
        try {
            final Set<FileObject> refFos = new HashSet<FileObject>();
            refFos.add(treePathHandle.getFileObject());
            final ElementHandle[] handle = new ElementHandle[1];

            jfxs.runUserActionTask(new Task<CompilationController>() {

                public void run(final CompilationController cc) throws Exception {
                    final ClassIndex ci = cc.getClasspathInfo().getClassIndex();
                    Element el = treePathHandle.resolveElement(cc);
                    handle[0] = ElementHandle.create(el);
                    switch(el.getKind()) {
                        case CLASS:
                        case INTERFACE: {
                            refFos.addAll(ci.getResources(handle[0], EnumSet.of(SearchKind.TYPE_REFERENCES, SearchKind.TYPE_DEFS), EnumSet.allOf(SearchScope.class)));
                            if (((TypeElement)el).getNestingKind() == NestingKind.TOP_LEVEL) {
                                new JavaFXTreePathScanner<Void, Void>() {

                                    @Override
                                    public Void visitClassDeclaration(ClassDeclarationTree node, Void p) {
                                        TypeElement te = (TypeElement)cc.getTrees().getElement(getCurrentPath());
                                        if (te.getNestingKind() == NestingKind.MEMBER) {
                                            refFos.addAll(ci.getResources(ElementHandle.create(te), EnumSet.of(SearchKind.TYPE_REFERENCES, SearchKind.TYPE_DEFS), EnumSet.allOf(SearchScope.class)));
                                        }
                                        return super.visitClassDeclaration(node, p);
                                    }

                                }.scan(cc.getCompilationUnit(), null);
                            }
                            break;
                        }
                        case FIELD: {
                            refFos.addAll(ci.getResources(handle[0], EnumSet.of(SearchKind.FIELD_REFERENCES), EnumSet.allOf(SearchScope.class)));
                            break;
                        }
                        case METHOD: {
                            refFos.addAll(ci.getResources(handle[0], EnumSet.of(SearchKind.METHOD_REFERENCES), EnumSet.allOf(SearchScope.class)));
                            break;
                        }
                    }
                }
            }, true);

            for(FileObject fo : refFos) {
                contextMap.put(fo, new TransformationContext());
                jfxs = JavaFXSource.forFileObject(fo);
                jfxs.runUserActionTask(new Task<CompilationController>() {

                    public void run(final CompilationController cc) throws Exception {
                        JavaFXTreePathScanner<Void, Set<TreePathHandle>> scanner = new RenameScanner(treePathHandle.getSimpleName(), handle[0], cc);
                        scanner.scan(cc.getCompilationUnit(), references);
                    }
                }, true);
            }

            for(TreePathHandle tph : references) {
                RefactoringElementImplementation refImpl = RenameRefactoringElement.create(tph, refactoring.getNewName(), treePathHandle.getSimpleName(), new ProxyLookup(l, Lookups.singleton(contextMap.get(tph.getFileObject()))));
                if (refImpl != null) {
                    bag.add(refactoring, refImpl);
                } else {
                    // ignore
                }
            }
        } catch (IOException e) {
            return new Problem(true, e.getLocalizedMessage());
        }
        return null;
    }

    private Problem checkFileNameClash(String newName, FileObject target) {
        for(FileObject fo : target.getParent().getChildren()) {
            if (!fo.equals(target) && newName.equals(fo.getName())) {
                String msg = NbBundle.getMessage(RenameRefactoringPlugin.class, fo.isFolder() ? "MSG_PackageExists" : "MSG_FileExists", fo.getName()); // NOI18N
                return new Problem(true, msg);
            }
        }
        return null;
    }

    public static final Problem isSourceElement(Element el, CompilationInfo info) {
        Problem preCheckProblem = null;
        if (SourceUtils.isFromLibrary(el, info.getClasspathInfo())) { //NOI18N
            preCheckProblem = new Problem(true, NbBundle.getMessage(
                    RenameRefactoringPlugin.class, "ERR_CannotRefactorLibraryClass",
                    el
                    ));
            return preCheckProblem;
        }
        FileObject file = SourceUtils.getFile(el,info.getClasspathInfo());
        // RetoucheUtils.isFromLibrary already checked file for null
        if (!SourceUtils.isFileInOpenProject(file)) {
            preCheckProblem =new Problem(true, NbBundle.getMessage(
                    RenameRefactoringPlugin.class,
                    "ERR_ProjectNotOpened",
                    FileUtil.getFileDisplayName(file)));
            return preCheckProblem;
        }
        return null;
    }
}
