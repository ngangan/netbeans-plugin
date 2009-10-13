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
package org.netbeans.modules.javafx.refactoring.impl.plugins;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree;
import java.io.IOException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.*;
import org.netbeans.api.fileinfo.NonRecursiveFolder;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.queries.VisibilityQuery;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.refactoring.api.*;
import org.netbeans.modules.refactoring.spi.ProgressProviderAdapter;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
import org.netbeans.modules.refactoring.spi.RefactoringPlugin;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;
import org.openide.util.Exceptions;
import org.openide.util.NbBundle;

/**
 * Implemented abilities:
 * <ul>
 * <li>Move file(s)</li>
 * <li>Move folder(s)</li>
 * <li>Rename folder</li>
 * <li>Rename package</li>
 * </ul>
 */
public class MoveRefactoringPlugin extends ProgressProviderAdapter implements RefactoringPlugin  {

    private Map packagePostfix = new HashMap();
    final AbstractRefactoring refactoring;
    final boolean isRenameRefactoring;
    ArrayList<FileObject> filesToMove = new ArrayList<FileObject>();
    HashMap<FileObject,ElementHandle> classes;
    /** list of folders grouped by source roots */
    List<List<FileObject>> foldersToMove = new ArrayList<List<FileObject>>();
    /** collection of packages that will change its name */
    Set<String> packages;
    Map<FileObject, Set<FileObject>> whoReferences = new HashMap<FileObject, Set<FileObject>>();
    
    public MoveRefactoringPlugin(MoveRefactoring move) {
        this.refactoring = move;
        this.isRenameRefactoring = false;
        setup(move.getRefactoringSource().lookupAll(FileObject.class), "", true);
    }
    
    public MoveRefactoringPlugin(RenameRefactoring rename) {
        this.refactoring = rename;
        this.isRenameRefactoring = true;
        FileObject fo = rename.getRefactoringSource().lookup(FileObject.class);
        if (fo!=null) {
            setup(Collections.singletonList(fo), "", true);
        } else {
            setup(Collections.singletonList((rename.getRefactoringSource().lookup(NonRecursiveFolder.class)).getFolder()), "", false); // NOI18N
        }
    }
    
    @Override
    public Problem preCheck() {
        Problem preCheckProblem = null;
        for (FileObject file:filesToMove) {
            if (!SourceUtils.isElementInOpenProject(file)) {
                preCheckProblem = JavaFXRefactoringPlugin.createProblem(preCheckProblem, true, NbBundle.getMessage(
                        MoveRefactoringPlugin.class,
                        "ERR_ProjectNotOpened",
                        FileUtil.getFileDisplayName(file)));
            }
        }
        return preCheckProblem;
    }

    @Override
    public Problem checkParameters() {
        return null;
    }

    @Override
    public Problem fastCheckParameters() {
        if (isRenameRefactoring) {
            //folder rename
            FileObject f = refactoring.getRefactoringSource().lookup(FileObject.class);
            if (f!=null) {
                String newName = ((RenameRefactoring) refactoring).getNewName();
                if (!SourceUtils.isValidPackageName(newName)) {
                    String msg = new MessageFormat(NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_InvalidPackage")).format(
                            new Object[] {newName}
                    );
                    return new Problem(true, msg);
                }
                
                if (f.getParent().getFileObject(newName, f.getExt())!=null) {
                    String msg = new MessageFormat(NbBundle.getMessage(RenameRefactoringPlugin.class,"ERR_PackageExists")).format(
                            new Object[] {newName}
                    );
                    return new Problem(true, msg);
                }
            }
        }
        if (!isRenameRefactoring) {
            try {
                for (FileObject f: filesToMove) {
                    if (!SourceUtils.isJavaFXFile(f))
                        continue;
                    String targetPackageName = this.getTargetPackageName(f);
                    if (!SourceUtils.isValidPackageName(targetPackageName)) {
                        String s = NbBundle.getMessage(RenameRefactoringPlugin.class, "ERR_InvalidPackage"); //NOI18N
                        String msg = new MessageFormat(s).format(
                                new Object[] {targetPackageName}
                        );
                        return new Problem(true, msg);
                    }
                    FileObject targetRoot = SourceUtils.getClassPathRoot(((MoveRefactoring)refactoring).getTarget().lookup(URL.class));
                    FileObject targetF = targetRoot.getFileObject(targetPackageName.replace('.', '/'));
                    
                    String pkgName = null;
                    if ((targetF!=null && !targetF.canWrite())) {
                        return new Problem(true, new MessageFormat(NbBundle.getMessage(MoveRefactoringPlugin.class,"ERR_PackageIsReadOnly")).format( // NOI18N
                                new Object[] {targetPackageName}
                        ));
                    }
                    
                    //                this.movingToDefaultPackageMap.put(r, Boolean.valueOf(targetF!= null && targetF.equals(classPath.findOwnerRoot(targetF))));
                    pkgName = targetPackageName;
                    
                    if (pkgName == null) {
                        pkgName = ""; // NOI18N
                    } else if (pkgName.length() > 0) {
                        pkgName = pkgName + '.';
                    }
                    //targetPrefix = pkgName;
                    
                    //                JavaClass[] sourceClasses = (JavaClass[]) sourceClassesMap.get(r);
                    //                String[] names = new String [sourceClasses.length];
                    //                for (int x = 0; x < names.length; x++) {
                    //                    names [x] = sourceClasses [x].getName();
                    //                }
                    //
                    //                FileObject movedFile = JavaMetamodel.getManager().getDataObject(r).getPrimaryFile();
                    String fileName = f.getName();
                    if (targetF!=null) {
                        FileObject[] children = targetF.getChildren();
                        for (int x = 0; x < children.length; x++) {
                            if (children[x].getName().equals(fileName) && "java".equals(children[x].getExt()) && !children[x].equals(f) && !children[x].isVirtual()) { //NOI18N
                                return new Problem(true, new MessageFormat(
                                        NbBundle.getMessage(MoveRefactoringPlugin.class,"ERR_ClassToMoveClashes")).format(new Object[] {fileName} // NOI18N
                                ));
                            }
                        } // for
                    }
                }
            } catch (IOException ioe) {
                //do nothing
            }
        }
        return null;
    }

    private Problem checkProjectDeps(Set<FileObject> a) {
        if (!isRenameRefactoring) {
            Set<FileObject> sourceRoots = new HashSet<FileObject>();
            for (FileObject file : filesToMove) {
                ClassPath cp = ClassPath.getClassPath(file, ClassPath.SOURCE);
                if (cp != null) {
                    FileObject root = cp.findOwnerRoot(file);
                    sourceRoots.add(root);
                }
            }
            URL target = ((MoveRefactoring) refactoring).getTarget().lookup(URL.class);
            if (target == null) {
                return null;
            }
            try {
                FileObject r = SourceUtils.getClassPathRoot(target);
                URL targetUrl = URLMapper.findURL(r, URLMapper.EXTERNAL);
                Set<URL> deps = SourceUtils.getDependentRoots(targetUrl);
                for (FileObject sourceRoot : sourceRoots) {
                    URL sourceUrl = URLMapper.findURL(sourceRoot, URLMapper.INTERNAL);
                    if (!deps.contains(sourceUrl)) {
                        Project sourceProject = FileOwnerQuery.getOwner(sourceRoot);
                        for (FileObject affected: a) {
                            if (FileOwnerQuery.getOwner(affected).equals(sourceProject) && !filesToMove.contains(affected)) {
                                Project targetProject = FileOwnerQuery.getOwner(r);
                                assert sourceProject!=null;
                                assert targetProject!=null;
                                String sourceName = ProjectUtils.getInformation(sourceProject).getDisplayName();
                                String targetName = ProjectUtils.getInformation(targetProject).getDisplayName();
                                return JavaFXRefactoringPlugin.createProblem(null, false, NbBundle.getMessage(MoveRefactoringPlugin.class, "ERR_MissingProjectDeps", sourceName, targetName));
                            }
                        }
                    }
                }
            } catch (IOException iOException) {
                Exceptions.printStackTrace(iOException);
            }
        }
        return null;
    }

    private Set<FileObject> getRelevantFiles() {
        ClassIndex idx = getClassIndex();
        Set<FileObject> set = new HashSet<FileObject>();
        for (Map.Entry<FileObject, ElementHandle> entry : classes.entrySet()) {
            //set.add(SourceUtils.getFile(el, cpInfo));
            Set<FileObject> files = idx.getResources(entry.getValue(), EnumSet.of(ClassIndex.SearchKind.TYPE_REFERENCES, ClassIndex.SearchKind.IMPLEMENTORS),EnumSet.of(ClassIndex.SearchScope.SOURCE));
            set.addAll(files);
            whoReferences.put(entry.getKey(), files);
        }
        set.addAll(filesToMove);
        return set;
    }    
    
    private void initClasses() {
        classes = new HashMap<FileObject,ElementHandle>();
        for (int i=0;i<filesToMove.size();i++) {
            final int j = i;
            try {
                JavaFXSource source = JavaFXSource.forFileObject(filesToMove.get(i));
                
                source.runUserActionTask(new Task<CompilationController>() {
                    
                    public void run(final CompilationController parameter) throws Exception {
                        List<? extends Tree> trees= parameter.getCompilationUnit().getTypeDecls();
                        for (Tree t: trees) {
                            if (t.getJavaFXKind() == Tree.JavaFXKind.CLASS_DECLARATION) {
                                if (((ClassDeclarationTree) t).getSimpleName().toString().equals(filesToMove.get(j).getName())) {
                                    classes.put(filesToMove.get(j), ElementHandle.create(parameter.getTrees().getElement(JavaFXTreePath.getPath(parameter.getCompilationUnit(), t))));
                                    return ;
                                }
                            }
                        }
                              
                    }
                }, true);
            } catch (IOException ex) {
                java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, ex.getMessage(), ex);
            }
            
        }
    }

    private void initPackages() {
        if (foldersToMove.isEmpty()) {
            packages = Collections.emptySet();
            return;
        } else {
            packages = new HashSet<String>();
        }
        
        for (List<FileObject> folders : foldersToMove) {
            ClassPath cp = ClassPath.getClassPath(folders.get(0), ClassPath.SOURCE);
            for (FileObject folder : folders) {
                String pkgName = cp.getResourceName(folder, '.', false);
                packages.add(pkgName);
            }
        }
    }
    
    public Problem prepare(RefactoringElementsBag elements) {
        fireProgressListenerStart(ProgressEvent.START, -1);
        initClasses();
        initPackages();
        
        Set<FileObject> a = getRelevantFiles();
        Problem p = checkProjectDeps(a);
        fireProgressListenerStep(a.size());
//        MoveTransformer t;
//        TransformTask task = new TransformTask(t=new MoveTransformer(this), null);
//        Problem prob = createAndAddElements(a, task, elements, refactoring);
        fireProgressListenerStop();
//        return prob != null ? prob : chainProblems(p, t.getProblem());
        return null;
    }
    
    private static Problem chainProblems(Problem p,Problem p1) {
        Problem problem;
        
        if (p==null) return p1;
        if (p1==null) return p;
        problem=p;
        while(problem.getNext()!=null) {
            problem=problem.getNext();
        }
        problem.setNext(p1);
        return p;
    }

    String getNewPackageName() {
        if (isRenameRefactoring) {
            return ((RenameRefactoring) refactoring).getNewName();
        } else {
            // XXX cache it !!!
            return SourceUtils.getPackageName(((MoveRefactoring) refactoring).getTarget().lookup(URL.class));
        }
    }
    
    String getTargetPackageName(FileObject fo) {
        if (isRenameRefactoring) {
            if (refactoring.getRefactoringSource().lookup(NonRecursiveFolder.class) !=null)
                //package rename
                return getNewPackageName();
            else {
                //folder rename
                FileObject folder = refactoring.getRefactoringSource().lookup(FileObject.class);
                ClassPath cp = ClassPath.getClassPath(folder, ClassPath.SOURCE);
                FileObject root = cp.findOwnerRoot(folder);
                String prefix = FileUtil.getRelativePath(root, folder.getParent()).replace('/','.');
                String postfix = FileUtil.getRelativePath(folder, fo.isFolder() ? fo : fo.getParent()).replace('/', '.');
                String t = concat(prefix, getNewPackageName(), postfix);
                return t;
            }
        } else if (packagePostfix != null) {
            if (fo == null) {
                return getNewPackageName();
            }
            String postfix = (String) packagePostfix.get(fo);
            String packageName = concat(null, getNewPackageName(), postfix);
            return packageName;
        } else
            return getNewPackageName();
    }
    
    private void setup(Collection fileObjects, String postfix, boolean recursively) {
        setup(fileObjects, postfix, recursively, null);
    }
    
    private void setup(Collection fileObjects, String postfix, boolean recursively, List<FileObject> sameRootList) {
        for (Iterator i = fileObjects.iterator(); i.hasNext(); ) {
            FileObject fo = (FileObject) i.next();
            if (SourceUtils.isJavaFXFile(fo)) {
                packagePostfix.put(fo, postfix.replace('/', '.'));
                filesToMove.add(fo);
            } else if (!(fo.isFolder())) {
                packagePostfix.put(fo, postfix.replace('/', '.'));
            } else if (VisibilityQuery.getDefault().isVisible(fo)) {
                //o instanceof DataFolder
                //CVS folders are ignored
                boolean addDot = !"".equals(postfix);
                Collection col = new ArrayList();
                for (FileObject fo2: fo.getChildren()) {
                    if (!fo2.isFolder() || (fo2.isFolder() && recursively)) 
                        col.add(fo2);
                }
                List<FileObject> curRootList = sameRootList;
                if (sameRootList == null) {
                    curRootList = new ArrayList<FileObject>();
                    foldersToMove.add(curRootList);
                }
                curRootList.add(fo);
                setup(col,
                        postfix + (addDot ? "." : "") + fo.getName(), // NOI18N
                        recursively,
                        curRootList);
            }
        }
    }
 
    private String concat(String s1, String s2, String s3) {
        String result = "";
        if (s1 != null && !"".equals(s1)) {
            result += s1 + "."; // NOI18N
        }
        result +=s2;
        if (s3 != null && !"".equals(s3)) {
            result += ("".equals(result)? "" : ".") + s3; // NOI18N
        }
        return result;
    }        

    final private Object indexLock = new Object();
    // @GuardedBy indexLock
    volatile private ClassIndex index = null;

    private ClassIndex getClassIndex() {
        if (index == null) {
            synchronized(indexLock) {
                FileObject fo = null;
                if (!filesToMove.isEmpty()) {
                    fo = filesToMove.get(0);
                } else if (!foldersToMove.isEmpty() && !foldersToMove.get(0).isEmpty()) {
                    fo = foldersToMove.get(0).get(0);
                }
                index = ClassIndex.forClasspathInfo(SourceUtils.getClasspathInfoFor(fo));
            }
        }
        return index;
    }

    public void cancelRequest() {
        // do nothing
    }


}    
