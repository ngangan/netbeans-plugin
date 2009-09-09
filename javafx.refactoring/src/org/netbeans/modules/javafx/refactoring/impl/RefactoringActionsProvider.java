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

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.UnitTree;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.swing.Action;
import javax.swing.JOptionPane;
import javax.swing.text.JTextComponent;
import org.netbeans.api.fileinfo.NonRecursiveFolder;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.TreePathHandle;
import org.netbeans.modules.javafx.refactoring.impl.ui.RenameRefactoringUI;
import org.netbeans.modules.refactoring.api.ui.ExplorerContext;
import org.netbeans.modules.refactoring.api.ui.RefactoringActionsFactory;
import org.netbeans.modules.refactoring.spi.ui.ActionsImplementationProvider;
import org.netbeans.modules.refactoring.spi.ui.RefactoringUI;
import org.netbeans.modules.refactoring.spi.ui.UI;
import org.openide.ErrorManager;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataNode;
import org.openide.loaders.DataObject;
import org.openide.nodes.Node;
import org.openide.text.CloneableEditorSupport;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.lookup.ServiceProvider;
import org.openide.windows.TopComponent;

/**
 *
 * @author Jaroslav Bachorik
 */
@ServiceProvider(service=ActionsImplementationProvider.class)
public class RefactoringActionsProvider extends ActionsImplementationProvider {
    final private static Logger LOGGER = Logger.getLogger(RefactoringActionsProvider.class.getName());

    @Override
    public boolean canFindUsages(Lookup lkp) {
        FileObject file = lkp.lookup(FileObject.class);
        return file != null && file.getMIMEType().equals("text/x-fx");
    }

    @Override
    public void doFindUsages(Lookup lkp) {
        FileObject fo = lkp.lookup(FileObject.class);
        JavaFXSource jfxs = JavaFXSource.forFileObject(fo);
        try {
            jfxs.runUserActionTask(new Task<CompilationController>() {
                public void run(CompilationController cc) throws Exception {
                    ClassIndex ci = cc.getClasspathInfo().getClassIndex();
                    for(TypeElement te : cc.getTopLevelElements()) {

                    for(ElementHandle eh : ci.getElements(ElementHandle.create(te), EnumSet.allOf(ClassIndex.SearchKind.class), EnumSet.allOf(ClassIndex.SearchScope.class))) {
                        System.out.println(">>> " + eh);
                    }
                    }
                }
            }, true);
        } catch (IOException e) {}
    }

    @Override
    public boolean canRename(Lookup lkp) {
        DataNode target = lkp.lookup(DataNode.class);
        return target != null && SourceUtils.isJavaFXFile(target.getDataObject().getPrimaryFile());
    }

    @Override
    public void doRename(final Lookup lookup) {
        Runnable task;
        EditorCookie ec = lookup.lookup(EditorCookie.class);
        if (isFromEditor(ec)) {
            task = new TextComponentTask(ec) {
                @Override
                protected RefactoringUI createRefactoringUI(TreePathHandle selectedElement,int startOffset,int endOffset, final CompilationInfo info) {
                    Element selected = selectedElement.resolveElement(info);
                    if (selected==null) {
                        LOGGER.log(Level.INFO, "doRename: " + selectedElement, new NullPointerException("selected")); // NOI18N
                        return null;
                    }
                    if (selected.getKind() == ElementKind.CONSTRUCTOR) {
                        selected = selected.getEnclosingElement();
                        selectedElement = TreePathHandle.create(info.getTrees().getPath(selected), info);
                    }
                    if (selected.getKind() == ElementKind.PACKAGE) {
                        NonRecursiveFolder folder = new NonRecursiveFolder() {
                            public FileObject getFolder() {
                                return info.getFileObject().getParent();
                            }
                        };
                        return new RenameRefactoringUI(folder);
                    } else {
                        return new RenameRefactoringUI(selectedElement, info);
                    }
                }
            };
        } else if (nodeHandle(lookup)) {
            task = new TreePathHandleTask(new HashSet<Node>(lookup.lookupAll(Node.class)), true) {

                RenameRefactoringUI ui;

                @Override
                protected void treePathHandleResolved(TreePathHandle handle, CompilationInfo javac) {
                    ui = new RenameRefactoringUI(handle, javac);
                }

                @Override
                protected RefactoringUI createRefactoringUI(Collection<TreePathHandle> handles) {
                    return ui;
                }

            };

        } else {
//            task = new NodeToFileObjectTask(new HashSet(lookup.lookupAll(Node.class))) {
            // canRename is valid only for single node
            task = new NodeToFileObjectTask(Collections.singleton(lookup.lookup(Node.class))) {

                RenameRefactoringUI ui;

                @Override
                protected void nodeTranslated(Node node, Collection<TreePathHandle> handles, CompilationInfo javac) {
                    String newName = getName(lookup);
                    ui = newName != null
                            ? new RenameRefactoringUI(javac.getFileObject(), newName, handles==null||handles.isEmpty()?null:handles.iterator().next(), javac)
                            : new RenameRefactoringUI(handles==null||handles.isEmpty()?null:handles.iterator().next(), javac);
                }

                @Override
                protected RefactoringUI createRefactoringUI(FileObject[] selectedElements, Collection<TreePathHandle> handles) {
                    if (ui == null) {
                        String newName = getName(lookup);
                        if (newName != null) {
                            ui = pkg[0] != null
                                    ? new RenameRefactoringUI(pkg[0], newName)
                                    : new RenameRefactoringUI(selectedElements[0], newName, null, null);
                        } else {
                            ui = pkg[0]!= null
                                    ? new RenameRefactoringUI(pkg[0])
                                    : null; //new RenameRefactoringUI(selectedElements[0], null, null);
                        }
                    }
                    return ui;
                }
            };
        }
        SourceUtils.invokeAfterScanFinished(task, getActionName(RefactoringActionsFactory.renameAction()));
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
        Node n = lookup.lookup(Node.class);
        if (n!=null) {
            if (n.getLookup().lookup(TreePathHandle.class)!=null)
                return true;
        }
        return false;
    }

    private static boolean isRefactorableFolder(DataObject dataObject) {
        FileObject fileObject = dataObject.getPrimaryFile();
        if (/* #159628 */!Boolean.TRUE.equals(fileObject.getAttribute("isRemoteAndSlow"))) { // NOI18N
            FileObject[] children = fileObject.getChildren();
            if (children == null || children.length <= 0) {
                return false;
            }
        }

        return (dataObject instanceof DataFolder) &&
                SourceUtils.isFileInOpenProject(fileObject) &&
                SourceUtils.isOnSourceClasspath(fileObject) &&
                !SourceUtils.isClasspathRoot(fileObject);
    }

    public static abstract class TreePathHandleTask implements Runnable, Task<CompilationController> {
        private Collection<TreePathHandle> handles = new ArrayList<TreePathHandle>();
        private TreePathHandle current;
        boolean renameFile;

        public TreePathHandleTask(Collection<? extends Node> nodes) {
            this(nodes, false);
        }

        public TreePathHandleTask(Collection<? extends Node> nodes, boolean useFirstHandle) {
            for (Node n:nodes) {
                TreePathHandle temp = n.getLookup().lookup(TreePathHandle.class);
                if (temp!=null) {
                    handles.add(temp);
                    if (useFirstHandle) {
                        break;
                    }
                }
            }
        }

        public void cancel() {
        }

        public void run(CompilationController info) throws Exception {
            Element el = current.resolveElement(info);
            if (el!=null && el instanceof TypeElement && !((TypeElement)el).getNestingKind().isNested()) {
                if (info.getFileObject().getName().equals(el.getSimpleName().toString())) {
                    renameFile = true;
                }
            }
            treePathHandleResolved(current, info);
        }

        public void run() {
            for (TreePathHandle handle:handles) {
                FileObject f = handle.getFileObject();
                current = handle;
                JavaFXSource source = JavaFXSource.forFileObject(f);
                assert source != null;
                try {
                    source.runUserActionTask(this, true);
                } catch (IllegalArgumentException ex) {
                    ex.printStackTrace();
                } catch (IOException ex) {
                    ex.printStackTrace();
                }
            }

            TopComponent activetc = TopComponent.getRegistry().getActivated();

            RefactoringUI ui = createRefactoringUI(handles);
            if (ui!=null) {
                UI.openRefactoringUI(ui, activetc);
            } else {
                JOptionPane.showMessageDialog(null,NbBundle.getMessage(RefactoringActionsProvider.class, "ERR_CannotRenameKeyword"));
            }
        }

        /**
         * This is the place where subclasses may collect info about handles.
         * @param handle handle
         * @param javac context of running transaction
         */
        protected void treePathHandleResolved(TreePathHandle handle, CompilationInfo javac) {
        }

        protected abstract RefactoringUI createRefactoringUI(Collection<TreePathHandle> handles);
    }

    public static abstract class TextComponentTask implements Runnable, Task<CompilationController> {
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

        public void run(CompilationController cc) throws Exception {
            JavaFXTreePath selectedElement = null;
            selectedElement = cc.getTreeUtilities().pathFor(caret);
            //workaround for issue 89064
            if (selectedElement.getLeaf().getJavaFXKind() == Tree.JavaFXKind.COMPILATION_UNIT) {
                List<? extends Tree> decls = cc.getCompilationUnit().getTypeDecls();
                if (!decls.isEmpty()) {
                    selectedElement = JavaFXTreePath.getPath(cc.getCompilationUnit(), decls.get(0));
                }
            }
            ui = createRefactoringUI(TreePathHandle.create(selectedElement, cc), start, end, cc);
        }

        public final void run() {
            try {
                JavaFXSource source = JavaFXSource.forDocument(textC.getDocument());
                source.runUserActionTask(this, true);
            } catch (IOException ioe) {
                ErrorManager.getDefault().notify(ioe);
                return ;
            }
            TopComponent activetc = TopComponent.getRegistry().getActivated();

            if (ui!=null) {
                UI.openRefactoringUI(ui, activetc);
            } else {
                JOptionPane.showMessageDialog(null,NbBundle.getMessage(RefactoringActionsProvider.class, "ERR_CannotRenameKeyword"));
            }
        }

        protected abstract RefactoringUI createRefactoringUI(TreePathHandle selectedElement,int startOffset,int endOffset, CompilationInfo info);
    }

    public static abstract class NodeToElementTask implements Runnable, Task<CompilationController>  {
        private Node node;
        private RefactoringUI ui;

        public NodeToElementTask(Collection<? extends Node> nodes) {
            assert nodes.size() == 1;
            this.node = nodes.iterator().next();
        }

        public void cancel() {
        }

        public void run(CompilationController info) throws Exception {
            UnitTree unit = info.getCompilationUnit();
            if (unit.getTypeDecls().isEmpty()) {
                ui = createRefactoringUI(null, info);
            } else {
                TreePathHandle representedObject = TreePathHandle.create(JavaFXTreePath.getPath(unit, unit.getTypeDecls().get(0)),info);
                ui = createRefactoringUI(representedObject, info);
            }
        }

        public final void run() {
            DataObject o = node.getCookie(DataObject.class);
            JavaFXSource source = JavaFXSource.forFileObject(o.getPrimaryFile());
            assert source != null;
            try {
                source.runUserActionTask(this, true);
            } catch (IllegalArgumentException ex) {
                ex.printStackTrace();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
            if (ui!=null) {
                UI.openRefactoringUI(ui);
            } else {
                JOptionPane.showMessageDialog(null,NbBundle.getMessage(RefactoringActionsProvider.class, "ERR_NoTypeDecls"));
            }
        }
        protected abstract RefactoringUI createRefactoringUI(TreePathHandle selectedElement, CompilationInfo info);
    }

    public static abstract class NodeToFileObjectTask implements Runnable, Task<CompilationController> {
        private Collection<? extends Node> nodes;
        public NonRecursiveFolder pkg[];
        Collection<TreePathHandle> handles = new ArrayList<TreePathHandle>();
        private Node currentNode;

        public NodeToFileObjectTask(Collection<? extends Node> nodes) {
            this.nodes = nodes;
        }

        public void cancel() {
        }

        public void run(CompilationController info) throws Exception {
            Collection<TreePathHandle> handlesPerNode = new ArrayList<TreePathHandle>();
            UnitTree unit = info.getCompilationUnit();
            Collection<TreePathHandle> publicHandles = new ArrayList<TreePathHandle>();
            Collection<TreePathHandle> sameNameHandles = new ArrayList<TreePathHandle>();
            for (Tree t: unit.getTypeDecls()) {
                Element e = info.getTrees().getElement(JavaFXTreePath.getPath(unit, t));
                if (e == null || !(e.getKind().isClass() || e.getKind().isInterface())) {
                    // syntax errors #111195
                    continue;
                }
                if (e.getSimpleName().toString().equals(info.getFileObject().getName())) {
                    TreePathHandle representedObject = TreePathHandle.create(JavaFXTreePath.getPath(unit,t),info);
                    sameNameHandles.add(representedObject);
                }
                if (e.getModifiers().contains(Modifier.PUBLIC)) {
                    TreePathHandle representedObject = TreePathHandle.create(JavaFXTreePath.getPath(unit,t),info);
                    publicHandles.add(representedObject);
                }
            }
            if (!publicHandles.isEmpty()) {
                handlesPerNode.addAll(publicHandles);
            } else {
                handlesPerNode.addAll(sameNameHandles);
            }

            if (!handlesPerNode.isEmpty()) {
                handles.addAll(handlesPerNode);
                nodeTranslated(currentNode, handlesPerNode, info);
            }
        }

        public void run() {
            FileObject[] fobs = new FileObject[nodes.size()];
            pkg = new NonRecursiveFolder[fobs.length];
            int i = 0;
            for (Node node:nodes) {
                DataObject dob = node.getCookie(DataObject.class);
                if (dob!=null) {
                    fobs[i] = dob.getPrimaryFile();
                    if (SourceUtils.isJavaFXFile(fobs[i])) {
                        JavaFXSource source = JavaFXSource.forFileObject(fobs[i]);
                        assert source != null;
                        try {
                            currentNode = node;
                            // XXX this could be optimize by ClasspasthInfo in case of more than one file
                            source.runUserActionTask(this, true);
                        } catch (IllegalArgumentException ex) {
                            ex.printStackTrace();
                        } catch (IOException ex) {
                            ex.printStackTrace();
                        } finally {
                            currentNode = null;
                        }
                    }

                    pkg[i++] = node.getLookup().lookup(NonRecursiveFolder.class);
                }
            }
            RefactoringUI ui = createRefactoringUI(fobs, handles);
            if (ui!=null) {
                UI.openRefactoringUI(ui);
            } else {
                JOptionPane.showMessageDialog(null,NbBundle.getMessage(RefactoringActionsProvider.class, "ERR_NoTypeDecls"));
            }
        }

        /**
         * Notifies subclasses about the translation.
         * This is the place where subclasses may collect info about handles.
         * @param node node that is translated
         * @param handles handles translated from the node
         * @param javac context of running translation
         */
        protected void nodeTranslated(Node node, Collection<TreePathHandle> handles, CompilationInfo javac) {
        }

        protected abstract RefactoringUI createRefactoringUI(FileObject[] selectedElement, Collection<TreePathHandle> handles);
    }

    static String getName(Lookup look) {
        ExplorerContext ren = look.lookup(ExplorerContext.class);
        if (ren==null)
            return null;
        return ren.getNewName(); //NOI18N
    }

    static String getActionName(Action action) {
        String arg = (String) action.getValue(Action.NAME);
        arg = arg.replace("&", ""); // NOI18N
        return arg.replace("...", ""); // NOI18N
    }
}
