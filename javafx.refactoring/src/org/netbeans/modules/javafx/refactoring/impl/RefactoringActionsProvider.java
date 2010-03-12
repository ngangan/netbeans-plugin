/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
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
 * Portions Copyrighted 1997-2008 Sun Microsystems, Inc.
 */
package org.netbeans.modules.javafx.refactoring.impl;

import java.awt.datatransfer.Transferable;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.NestingKind;
import javax.swing.Action;
import javax.swing.JOptionPane;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import org.netbeans.api.fileinfo.NonRecursiveFolder;
import org.netbeans.api.java.source.CompilationController;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.Task;
import org.netbeans.api.java.source.TreePathHandle;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClassIndex.SearchKind;
import org.netbeans.api.javafx.source.ClassIndex.SearchScope;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.modules.javafx.refactoring.RefactoringSupport;
import org.netbeans.modules.javafx.refactoring.impl.RefactoringActionsProvider.NodeToElementTask;
import org.netbeans.modules.javafx.refactoring.impl.RefactoringActionsProvider.TextComponentTask;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.impl.ui.CopyClassUI;
import org.netbeans.modules.javafx.refactoring.impl.ui.CopyClassesUI;
import org.netbeans.modules.javafx.refactoring.impl.ui.MoveClassUI;
import org.netbeans.modules.javafx.refactoring.impl.ui.MoveClassesUI;
import org.netbeans.modules.javafx.refactoring.impl.ui.RenameRefactoringUI;
import org.netbeans.modules.javafx.refactoring.impl.ui.SafeDeleteUI;
import org.netbeans.modules.javafx.refactoring.impl.ui.WhereUsedQueryUI;
import org.netbeans.modules.javafx.refactoring.repository.ClassModel;
import org.netbeans.modules.javafx.refactoring.repository.ClassModelFactory;
import org.netbeans.modules.javafx.refactoring.repository.ElementDef;
import org.netbeans.modules.refactoring.api.MoveRefactoring;
import org.netbeans.modules.refactoring.api.MultipleCopyRefactoring;
import org.netbeans.modules.refactoring.api.RenameRefactoring;
import org.netbeans.modules.refactoring.api.SafeDeleteRefactoring;
import org.netbeans.modules.refactoring.api.SingleCopyRefactoring;
import org.netbeans.modules.refactoring.api.WhereUsedQuery;
import org.netbeans.modules.refactoring.api.ui.ExplorerContext;
import org.netbeans.modules.refactoring.api.ui.RefactoringActionsFactory;
import org.netbeans.modules.refactoring.spi.ui.ActionsImplementationProvider;
import org.netbeans.modules.refactoring.spi.ui.RefactoringUI;
import org.netbeans.modules.refactoring.spi.ui.UI;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;
import org.openide.nodes.Node;
import org.openide.text.CloneableEditorSupport;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.datatransfer.PasteType;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;
import org.openide.util.lookup.ServiceProvider;
import org.openide.windows.TopComponent;

/**
 *
 * @author Jaroslav Bachorik
 */
@ServiceProvider(service = ActionsImplementationProvider.class)
public class RefactoringActionsProvider extends ActionsImplementationProvider {

    final private static Logger LOGGER = Logger.getLogger(RefactoringActionsProvider.class.getName());

    @Override
    public boolean canFindUsages(Lookup lkp) {
        FileObject file = lkp.lookup(FileObject.class);
        return file != null && file.getMIMEType().equals("text/x-fx");
    }
    volatile private boolean isFindUsages;

    @Override
    public void doFindUsages(Lookup lkp) {
        if (isFindUsages) {
            return;
        }

        Runnable task;
        final InstanceContent lkpContent = new InstanceContent();
        final WhereUsedQuery query = new WhereUsedQuery(new AbstractLookup(lkpContent));

        EditorCookie ec = lkp.lookup(EditorCookie.class);
        if (isFromEditor(ec)) {
            task = new TextComponentTask(ec) {

                @Override
                protected RefactoringUI createRefactoringUI(FileObject srcFo, int startOffset, int endOffset) {
                    ClassModel cm = RefactoringSupport.classModelFactory(query).classModelFor(srcFo);
                    ElementDef edef = cm.getDefForPos(startOffset);
                    if (edef == null) {
                        return null;
                    }

                    FileObject javaFile = org.netbeans.api.java.source.SourceUtils.getFile(edef.createHandle().toJava(), org.netbeans.api.java.source.ClasspathInfo.create(srcFo));

                    lkpContent.add(edef);
                    query.getContext().add(srcFo);
                    if (javaFile != null) {
                        TreePathHandle tph = RefactoringSupport.toJava(edef.createHandle(), javaFile);
                        if (tph != null) {
                            lkpContent.add(tph);
                        }
                    }
                    return new WhereUsedQueryUI(query);
                }
            };
        } else {
            task = new NodeToElementTask(lkp.lookupAll(Node.class)) {

                protected RefactoringUI createRefactoringUI(FileObject srcFo) {
                    ClassModel cm = RefactoringSupport.classModelFactory(query).classModelFor(srcFo);
                    ElementDef edef = null;
                    for (ElementDef typeDef : cm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.ENUM, ElementKind.INTERFACE))) {
                        if (edef == null) {
                            edef = typeDef;
                        }
                        if (typeDef.getName().equals(srcFo.getName())) {
                            edef = typeDef;
                            break;
                        }
                    }
                    if (edef == null) {
                        return null;
                    }
                    lkpContent.add(edef);

                    query.getContext().add(srcFo);
                    return new WhereUsedQueryUI(query);
                }
            };
        }
        try {
            isFindUsages = true;
            task.run();
        } finally {
            isFindUsages = false;
        }
    }

    @Override
    public boolean canRename(Lookup lkp) {
//        if (!isRefactoringEnabled()) return false;
        Node target = lkp.lookup(Node.class);

        DataObject dobj = (target != null ? target.getCookie(DataObject.class) : null);
        return dobj != null && SourceUtils.isJavaFXFile(dobj.getPrimaryFile());
    }

    @Override
    public void doRename(final Lookup lookup) {
        Runnable task;
        final InstanceContent lkpContent = new InstanceContent();
        final RenameRefactoring ref = new RenameRefactoring(new AbstractLookup(lkpContent));
        EditorCookie ec = lookup.lookup(EditorCookie.class);
        if (isFromEditor(ec)) {
            task = new TextComponentTask(ec) {

                @Override
                protected RefactoringUI createRefactoringUI(final FileObject srcFo, int startOffset, int endOffset) {
                    ClassModel cm = RefactoringSupport.classModelFactory(ref).classModelFor(srcFo);

                    final ElementDef edef = cm.getDefForPos(startOffset);
                    if (edef == null) {
                        return null;
                    }

                    ClasspathInfo cpInfo = ClasspathInfo.create(srcFo);
                    ClassIndex ci = cpInfo.getClassIndex();

                    FileObject javaFile = org.netbeans.api.java.source.SourceUtils.getFile(edef.createHandle().toJava(), org.netbeans.api.java.source.ClasspathInfo.create(srcFo));

                    if (edef.getKind() == ElementKind.PACKAGE) {
                        lkpContent.add(new NonRecursiveFolder() {

                            public FileObject getFolder() {
                                return srcFo.getParent();
                            }
                        });
                    } else {
                        lkpContent.add(edef);
                        if (edef.getName().equals(srcFo.getName())) {
                            lkpContent.add(srcFo);
                        } else {
                            if (edef.getKind().isClass() || edef.getKind().isInterface()) {
                                Set<FileObject> defining = cpInfo.getClassIndex().getResources(edef.createHandle(), EnumSet.of(SearchKind.TYPE_DEFS), EnumSet.allOf(SearchScope.class));
                                if (!defining.isEmpty()) {
                                    lkpContent.add(defining.iterator().next());
                                }
                            }
                        }
                        if (javaFile != null) {
                            if ((edef.getKind().isClass() || edef.getKind().isInterface()) && edef.getName().equals(javaFile.getName())) {
                                lkpContent.add(javaFile);
                            }
                            TreePathHandle tph = RefactoringSupport.toJava(edef.createHandle(), javaFile);
                            if (tph != null) {
                                lkpContent.add(tph);
                            }
                        }
                    }
                    ref.getContext().add(srcFo);
                    ref.getContext().add(cpInfo);
                    ref.getContext().add(ci);


                    return new RenameRefactoringUI(ref);
                }
            };
        } else if (nodeHandle(lookup)) {
            task = null;
//            task = new TreePathHandleTask(new HashSet<Node>(lookup.lookupAll(Node.class)), true) {
//
//                RenameRefactoringUI ui;
//
//                @Override
//                protected void locationResolved(ElementLocation location, ClassModelFactory factory, CompilationInfo javac) {
//                    if (renameFile) {
//                        ui = new RenameRefactoringUI(location.getSourceFile(), location, factory);
//                    } else {
//                        ui = new RenameRefactoringUI(location, factory);
//                    }
//                }
//
//                @Override
//                protected RefactoringUI createRefactoringUI(Collection<ElementLocation> locations, ClassModelFactory factory) {
//                    return ui;
//                }
//
//            };

        } else {
            // canRename is valid only for single node
            task = new NodeToFileObjectTask(Collections.singleton(lookup.lookup(Node.class))) {

                @Override
                protected RefactoringUI createRefactoringUI(FileObject[] files, NonRecursiveFolder[] pkgs) {
                    String newName = getName(lookup);
                    RefactoringUI ui = null;

                    lkpContent.add(pkgs[0] != null ? pkgs[0] : files[0]);
                    ClasspathInfo cpInfo = null;

                    if (files[0] != null || pkgs[0] != null) {
                        if (files[0] != null) {
                            ClassModel cm = RefactoringSupport.classModelFactory(ref).classModelFor(files[0]);
                            ElementDef typeDef = null;
                            for(ElementDef edef : cm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE, ElementKind.ENUM))) {
                                if (edef.getName().equals(files[0].getName())) {
                                    typeDef = edef;
                                    break;
                                }
                            }
                            lkpContent.add(typeDef);
                            cpInfo = ClasspathInfo.create(files[0]);
                        } else {
                            cpInfo = ClasspathInfo.create(pkgs[0].getFolder());
                        }
                    }
                    if (cpInfo != null) {
                        ref.getContext().add(cpInfo);
                        ref.getContext().add(cpInfo.getClassIndex());
                    }
                    ui = new RenameRefactoringUI(ref, newName);
                    return ui;
                }
            };
        }
        SourceUtils.invokeAfterScanFinished(task, getActionName(RefactoringActionsFactory.renameAction()));
    }

    @Override
    public boolean canMove(Lookup lkp) {
        Collection<? extends Node> nodes = new HashSet<Node>(lkp.lookupAll(Node.class));
        ExplorerContext drop = lkp.lookup(ExplorerContext.class);
        FileObject fo = getTarget(lkp);
        if (fo != null) {
            if (!fo.isFolder()) {
                return false;
            }
            if (!SourceUtils.isOnSourceClasspath(fo)) {
                return false;
            }

            //it is drag and drop
            Set<DataFolder> folders = new HashSet<DataFolder>();
            boolean jdoFound = false;
            for (Node n : nodes) {
                DataObject dob = n.getCookie(DataObject.class);
                if (dob == null) {
                    return false;
                }
                if (!SourceUtils.isOnSourceClasspath(dob.getPrimaryFile())) {
                    return false;
                }
                if (dob instanceof DataFolder) {
                    if (FileUtil.getRelativePath(dob.getPrimaryFile(), fo) != null) {
                        return false;
                    }
                    folders.add((DataFolder) dob);
                } else if (SourceUtils.isJavaFXFile(dob.getPrimaryFile())) {
                    jdoFound = true;
                }
            }
            if (jdoFound) {
                return true;
            }
            for (DataFolder fold : folders) {
                for (Enumeration<DataObject> e = (fold).children(true); e.hasMoreElements();) {
                    if (SourceUtils.isJavaFXFile(e.nextElement().getPrimaryFile())) {
                        return true;
                    }
                }
            }
            return false;
        } else {
            //regular invocation
            boolean result = false;
            for (Node n : nodes) {
                final DataObject dob = n.getCookie(DataObject.class);
                if (dob == null) {
                    return false;
                }
                if (dob instanceof DataFolder) {
                    return drop != null;
                }
                if (!SourceUtils.isOnSourceClasspath(dob.getPrimaryFile())) {
                    return false;
                }
                if (SourceUtils.isJavaFXFile(dob.getPrimaryFile())) {
                    result = true;
                }
            }
            return result;
        }
    }

    @Override
    public void doMove(final Lookup lkp) {
        Runnable task;
        EditorCookie ec = lkp.lookup(EditorCookie.class);

        final InstanceContent lkpContent = new InstanceContent();
        final MoveRefactoring ref = new MoveRefactoring(new AbstractLookup(lkpContent));

        if (isFromEditor(ec)) {
            task = new TextComponentTask(ec) {

                @Override
                protected RefactoringUI createRefactoringUI(FileObject srcFo, int startOffset, int endOffset) {
                    ClassModel cm = RefactoringSupport.classModelFactory(ref).classModelFor(srcFo);

                    ElementDef edef = cm.getDefForPos(startOffset);

                    if (edef == null) {
                        LOGGER.log(Level.INFO, "doMove: " + edef, new NullPointerException("e")); // NOI18N
                        return null;
                    }
                    if ((edef.getKind().isClass() || edef.getKind().isInterface()) && edef.getNestingKind() == NestingKind.TOP_LEVEL) {
                        if (edef.getName().equals(srcFo.getName())) {
                            return null; // new MoveClassUI(d);
                        }
                    }

                    ClasspathInfo cpInfo = ClasspathInfo.create(srcFo);
                    ref.getContext().add(cpInfo);
                    ref.getContext().add(cpInfo.getClassIndex());

                    lkpContent.add(srcFo);

                    return new MoveClassUI(ref);
                }
            };
        } else {
            task = new NodeToFileObjectTask(new HashSet<Node>(lkp.lookupAll(Node.class))) {

                @Override
                protected RefactoringUI createRefactoringUI(FileObject[] files, NonRecursiveFolder[] pkgs) {
                    PasteType paste = getPaste(lkp);
                    FileObject tar = getTarget(lkp);
                    if (files.length > 0 && files[0] != null) {
                        ClasspathInfo cpInfo = ClasspathInfo.create(files[0]);
                        ref.getContext().add(cpInfo);
                        ref.getContext().add(cpInfo.getClassIndex());
                    }
                    if (files.length == 1) {
                        if (!files[0].isFolder()) {
                            lkpContent.add(files[0]);
                            return new MoveClassUI(ref, tar, paste);
                        } else {
                            for (FileObject fo : files) {
                                lkpContent.add(fo);
                            }
                            return new MoveClassesUI(ref, tar, paste);
                        }
                    } else {
                        for (FileObject fo : files) {
                            lkpContent.add(fo);
                        }
                        return new MoveClassesUI(ref, tar, paste);
                    }
                }
            };
        }
        SourceUtils.invokeAfterScanFinished(task, getActionName(RefactoringActionsFactory.renameAction()));
    }

    @Override
    public boolean canCopy(Lookup lkp) {
        Collection<? extends Node> nodes = new HashSet<Node>(lkp.lookupAll(Node.class));
        if (nodes.size() < 1) {
            return false;
        }
        for (Iterator<? extends Node> iter = nodes.iterator(); iter.hasNext();) {
            Node n = iter.next();
            DataObject dob = n.getCookie(DataObject.class);
            if (dob == null) {
                return false;
            }

            ExplorerContext dict = lkp.lookup(ExplorerContext.class);
            FileObject fob = getTarget(lkp);
            if (dict != null && dict.getTargetNode() != null && fob == null) { //NOI18N
                //unknown target
                return false;
            }
            if (fob != null) {
                if (!fob.isFolder() || !SourceUtils.isOnSourceClasspath(fob)) {
                    return false;
                }
                FileObject fo = dob.getPrimaryFile();
                if (!SourceUtils.isRefactorable(fo)) { //NOI18N
                    return false;
                }

            } else {
                FileObject fo = dob.getPrimaryFile();
                if (!SourceUtils.isRefactorable(fo)) { //NOI18N
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public void doCopy(final Lookup lkp) {
        final InstanceContent lkpContent = new InstanceContent();

        Runnable task = new NodeToFileObjectTask(new HashSet<Node>(lkp.lookupAll(Node.class))) {

            @Override
            protected RefactoringUI createRefactoringUI(FileObject[] files, NonRecursiveFolder[] pkgs) {
                for(FileObject fo : files) {
                    lkpContent.add(fo);
                }
                if (files.length == 1) {
                    ClasspathInfo cpInfo = ClasspathInfo.create(files[0]);
                    SingleCopyRefactoring ref = new SingleCopyRefactoring(new AbstractLookup(lkpContent));
                    ref.getContext().add(cpInfo);
                    ref.getContext().add(cpInfo.getClassIndex());
                    return new CopyClassUI(ref, getTarget(lkp), getPaste(lkp));
                } else {
                    ClasspathInfo cpInfo = ClasspathInfo.create(files[0]);
                    MultipleCopyRefactoring ref = new MultipleCopyRefactoring(new AbstractLookup(lkpContent));
                    ref.getContext().add(cpInfo);
                    ref.getContext().add(cpInfo.getClassIndex());
                    return new CopyClassesUI(ref, getTarget(lkp), getPaste(lkp));
                }
            }
        };
//        }
        SourceUtils.invokeAfterScanFinished(task, getActionName(RefactoringActionsFactory.copyAction()));
    }

    @Override
    public boolean canDelete(Lookup lookup) {
        if (SourceUtils.isScanInProgress()) {
            return false;
        }
        Collection<? extends Node> nodes = new HashSet<Node>(lookup.lookupAll(Node.class));
        //We live with a 2 pass validation of the selected nodes for now since
        //the code will become unreadable if we attempt to implement all checks
        //in one pass.
        if (isSelectionHeterogeneous(nodes)) {
            return false;
        }
        for (Node n : nodes) {
//            ElementLocation tph = n.getLookup().lookup(ElementLocation.class);
//            if (tph != null) {
//                return SourceUtils.isRefactorable(tph.getSourceFile());
//            }
            DataObject dataObject = n.getCookie(DataObject.class);
            if (dataObject == null) {
                return false;
            }
            FileObject fileObject = dataObject.getPrimaryFile();
            if (isRefactorableFolder(dataObject)) {
                return true;
            }
            if (!SourceUtils.isRefactorable(fileObject)) {
                return false;
            }
        }
        return !nodes.isEmpty();
    }

    @Override
    public void doDelete(final Lookup lookup) {
        Runnable task;
        EditorCookie ec = lookup.lookup(EditorCookie.class);
        final boolean b = lookup.lookup(ExplorerContext.class) != null;

        final InstanceContent lkpContent = new InstanceContent();
        final SafeDeleteRefactoring ref = new SafeDeleteRefactoring(new AbstractLookup(lkpContent));

        if (isFromEditor(ec)) {
            task = new TextComponentTask(ec) {

                @Override
                protected RefactoringUI createRefactoringUI(FileObject srcFo, int startOffset, int endOffset) {
                    ClassModel cm = RefactoringSupport.classModelFactory(ref).classModelFor(srcFo);

                    ElementDef edef = cm.getDefForPos(startOffset);
                    if (edef == null) {
                        LOGGER.log(Level.INFO, "doDelete: " + edef, new NullPointerException("selected")); // NOI18N
                        return null;
                    }
                    if (edef.getKind() == ElementKind.PACKAGE || edef.getNestingKind() == NestingKind.TOP_LEVEL) {
                        if (edef.getName().equals(srcFo.getName())) {
                            lkpContent.add(srcFo);
                        }
                    }
                    lkpContent.add(edef);
                    
                    ClasspathInfo cpInfo = ClasspathInfo.create(srcFo);
                    ref.getContext().add(cpInfo);
                    ref.getContext().add(cpInfo.getClassIndex());

                    return new SafeDeleteUI(ref);
                }
            };
        } else if (nodeHandle(lookup)) {
            task = null;
//            task = new TreePathHandleTask(new HashSet<Node>(lookup.lookupAll(Node.class))) {
//
//                @Override
//                protected RefactoringUI createRefactoringUI(Collection<ElementLocation> handles, ClassModelFactory factory) {
//                    if (renameFile) {
//                        FileObject[] files = new FileObject[handles.size()];
//                        int i = 0;
//                        for (ElementLocation handle : handles) {
//                            files[i++] = handle.getSourceFile();
//                        }
//                        return new SafeDeleteUI(files, handles, b, factory);
//                    } else {
//                        return new SafeDeleteUI(handles.toArray(new ElementLocation[handles.size()]), factory);
//                    }
//                }
//            };
        } else {
            task = new NodeToFileObjectTask(new HashSet<Node>(lookup.lookupAll(Node.class))) {

                @Override
                protected RefactoringUI createRefactoringUI(FileObject[] files, NonRecursiveFolder[] pkgs) {
                    if (pkgs[0] != null) {
                        lkpContent.add(pkgs[0]);
                    } else {
                        lkpContent.add(files[0]);
                    }
                    return new SafeDeleteUI(ref, b);
                }
            };
        }
        SourceUtils.invokeAfterScanFinished(task, getActionName(RefactoringActionsFactory.safeDeleteAction()));
    }

    private static boolean isSelectionHeterogeneous(Collection<? extends Node> nodes) {
        boolean folderSelected = false;
        boolean nonFolderNodeSelected = false;
        for (Node node : nodes) {
            DataObject dataObject = node.getCookie(DataObject.class);
            if (dataObject == null) {
                continue;
            }
            if (isRefactorableFolder(dataObject)) {
                if (folderSelected || nonFolderNodeSelected) {
                    return true;
                } else {
                    folderSelected = true;
                }
            } else {
                nonFolderNodeSelected = true;
            }
        }

        return false;
    }

    static boolean isFromEditor(EditorCookie ec) {
        if (ec != null && ec.getOpenedPanes() != null) {
            TopComponent activetc = TopComponent.getRegistry().getActivated();
            if (activetc instanceof CloneableEditorSupport.Pane) {
                return true;
            }
        }
        return false;
    }

    static boolean nodeHandle(Lookup lookup) {
        return false;
//        Node n = lookup.lookup(Node.class);
//        if (n != null) {
//            if (n.getLookup().lookup(ElementHandle.class) != null) {
//                return true;
//            }
//        }
//        return false;
    }

    private static boolean isRefactorableFolder(DataObject dataObject) {
        FileObject fileObject = dataObject.getPrimaryFile();
        if (/* #159628 */!Boolean.TRUE.equals(fileObject.getAttribute("isRemoteAndSlow"))) { // NOI18N
            FileObject[] children = fileObject.getChildren();
            if (children == null || children.length <= 0) {
                return false;
            }
        }

        return (dataObject instanceof DataFolder)
                && SourceUtils.isFileInOpenProject(fileObject)
                && SourceUtils.isOnSourceClasspath(fileObject)
                && !SourceUtils.isClasspathRoot(fileObject);
    }

//    public static abstract class TreePathHandleTask implements Runnable, Task<CompilationController> {
//
//        private Collection<ElementLocation> locations = new ArrayList<ElementLocation>();
//        private ElementLocation current;
//        boolean renameFile;
//
//        public TreePathHandleTask(Collection<? extends Node> nodes) {
//            this(nodes, false);
//        }
//
//        public TreePathHandleTask(Collection<? extends Node> nodes, boolean useFirstHandle) {
//            for (Node n : nodes) {
//                ElementLocation temp = n.getLookup().lookup(ElementLocation.class);
//                if (temp != null) {
//                    locations.add(temp);
//                    if (useFirstHandle) {
//                        break;
//                    }
//                }
//            }
//        }
//
//        public void cancel() {
//        }
//
//        public void run(CompilationController info) throws Exception {
//            Element el = info.getElementUtilities().elementFor(current.getStartPosition());
//            if (el != null && el instanceof TypeElement && !((TypeElement) el).getNestingKind().isNested()) {
//                if (info.getFileObject().getName().equals(el.getSimpleName().toString())) {
//                    renameFile = true;
//                }
//            }
//            locationResolved(current, null, info);
//        }
//
//        public void run() {
//            for (ElementLocation location : locations) {
//                FileObject f = location.getSourceFile();
//                current = location;
//                JavaFXSource source = JavaFXSource.forFileObject(f);
//                assert source != null;
//                try {
//                    source.runUserActionTask(this, true);
//                } catch (IllegalArgumentException ex) {
//                    ex.printStackTrace();
//                } catch (IOException ex) {
//                    ex.printStackTrace();
//                }
//            }
//
//            TopComponent activetc = TopComponent.getRegistry().getActivated();
//
//            RefactoringUI ui = createRefactoringUI(locations, null);
//            if (ui != null) {
//                UI.openRefactoringUI(ui, activetc);
//            } else {
//                JOptionPane.showMessageDialog(null, NbBundle.getMessage(RefactoringActionsProvider.class, "ERR_CannotRenameKeyword"));
//            }
//        }
//
//        /**
//         * This is the place where subclasses may collect info about handles.
//         * @param handle handle
//         * @param javac context of running transaction
//         */
//        protected void locationResolved(ElementLocation location, ClassModelFactory factory, CompilationInfo javac) {
//        }
//
//        protected abstract RefactoringUI createRefactoringUI(Collection<ElementLocation> locations, ClassModelFactory factory);
//    }

    public static abstract class TextComponentTask implements Runnable {

        private JTextComponent textC;
        private int caret;
        private int start;
        private int end;
        private RefactoringUI ui;

        public TextComponentTask(EditorCookie ec) {
            this.textC = ec.getOpenedPanes()[0];
            this.caret = textC.getCaretPosition();
            this.start = textC.getSelectionStart();
            this.end = textC.getSelectionEnd();
            assert caret != -1;
            assert start != -1;
            assert end != -1;
        }

        public void cancel() {
        }

//        public void run(CompilationController cc) throws Exception {
////            Element selectedElement = cc.getElementUtilities().elementFor(caret);
//            ElementDef elDef = cmf.classModelFor(cc.getFileObject()).getDefForPos(caret);
//            ElementLocation location = new ElementLocation(elDef, cc.getFileObject());
//
//        }
        public final void run() {
            FileObject srcFo = null;
            Object source = textC.getDocument().getProperty(Document.StreamDescriptionProperty);
            if (source instanceof DataObject) {
                DataObject dObj = (DataObject) source;
                if (dObj != null) {
                    srcFo = dObj.getPrimaryFile();
                }
            }

            if (srcFo == null) {
                return;
            }

            ui = createRefactoringUI(srcFo, start, end);
            TopComponent activetc = TopComponent.getRegistry().getActivated();

            if (ui != null) {
                UI.openRefactoringUI(ui, activetc);
            } else {
                JOptionPane.showMessageDialog(null, NbBundle.getMessage(RefactoringActionsProvider.class, "ERR_CannotRenameKeyword"));
            }
        }

        protected abstract RefactoringUI createRefactoringUI(FileObject srcFo, int startOffset, int endOffset);
    }

    public static abstract class NodeToElementTask implements Runnable {

        private Node node;
        private RefactoringUI ui;

        public NodeToElementTask(Collection<? extends Node> nodes) {
            assert nodes.size() == 1;
            this.node = nodes.iterator().next();
        }

        public void cancel() {
        }

//        public void run(CompilationController info) throws Exception {
//            UnitTree unit = info.getCompilationUnit();
//            if (unit.getTypeDecls().isEmpty()) {
//                ui = createRefactoringUI(null, factory, info);
//            } else {
//                ElementHandle eh = ElementHandle.create(JavafxTreeInfo.symbolFor((JFXTree)unit.getTypeDecls().get(0)));
//                ClassModel cm = factory.classModelFor(info.getFileObject());
//                ElementDef def = null;
//                for(ElementDef edef : cm.getElementDefs(EnumSet.of(ElementKind.CLASS, ElementKind.INTERFACE, ElementKind.ENUM))) {
//                    if (edef.createHandle().equals(eh)) {
//                        def = edef;
//                        break;
//                    } else if (def == null) {
//                        def = edef;
//                    }
//                }
//                if (def != null) {
//                    ui = createRefactoringUI(new ElementLocation(def, info.getFileObject()), factory, info);
//                }
//            }
//        }
        public final void run() {
            DataObject o = node.getCookie(DataObject.class);
            ui = createRefactoringUI(o.getPrimaryFile());

            if (ui != null) {
                UI.openRefactoringUI(ui);
            } else {
                JOptionPane.showMessageDialog(null, NbBundle.getMessage(RefactoringActionsProvider.class, "ERR_NoTypeDecls"));
            }
        }

        protected abstract RefactoringUI createRefactoringUI(FileObject srcFo);
    }

    public static abstract class NodeToFileObjectTask implements Runnable {

        private Collection<? extends Node> nodes;
        private NonRecursiveFolder pkg[];
//        Collection<ElementLocation> locations = new ArrayList<ElementLocation>();
//        private Node currentNode;

        public NodeToFileObjectTask(Collection<? extends Node> nodes) {
            this.nodes = nodes;
        }

        public void cancel() {
        }

        public void run() {
            FileObject[] fobs = new FileObject[nodes.size()];
            pkg = new NonRecursiveFolder[fobs.length];
            int i = 0;
            for (Node node : nodes) {
                DataObject dob = node.getCookie(DataObject.class);
                if (dob != null) {
                    fobs[i] = dob.getPrimaryFile();
                    pkg[i++] = node.getLookup().lookup(NonRecursiveFolder.class);
                }
            }
            RefactoringUI ui = createRefactoringUI(fobs, pkg);
            if (ui != null) {
                UI.openRefactoringUI(ui);
            } else {
                JOptionPane.showMessageDialog(null, NbBundle.getMessage(RefactoringActionsProvider.class, "ERR_NoTypeDecls"));
            }
        }

        protected abstract RefactoringUI createRefactoringUI(FileObject[] selectedElement, NonRecursiveFolder[] pkgs);
    }

    static String getName(Lookup look) {
        ExplorerContext ren = look.lookup(ExplorerContext.class);
        if (ren == null) {
            return null;
        }
        return ren.getNewName(); //NOI18N
    }

    static String getActionName(Action action) {
        String arg = (String) action.getValue(Action.NAME);
        arg = arg.replace("&", ""); // NOI18N
        return arg.replace("...", ""); // NOI18N
    }

    private FileObject getTarget(Lookup look) {
        ExplorerContext drop = look.lookup(ExplorerContext.class);
        if (drop == null) {
            return null;
        }
        Node n = drop.getTargetNode();
        if (n == null) {
            return null;
        }
        DataObject dob = n.getCookie(DataObject.class);
        if (dob != null) {
            return dob.getPrimaryFile();
        }
        return null;
    }

    private PasteType getPaste(Lookup look) {
        ExplorerContext drop = look.lookup(ExplorerContext.class);
        if (drop == null) {
            return null;
        }
        Transferable orig = drop.getTransferable();
        if (orig == null) {
            return null;
        }
        Node n = drop.getTargetNode();
        if (n == null) {
            return null;
        }
        PasteType[] pt = n.getPasteTypes(orig);
        if (pt.length == 1) {
            return null;
        }
        return pt[1];
    }

    private static boolean isRefactoringEnabled() {
        return Boolean.getBoolean("javafx.refactoring");
    }
}
