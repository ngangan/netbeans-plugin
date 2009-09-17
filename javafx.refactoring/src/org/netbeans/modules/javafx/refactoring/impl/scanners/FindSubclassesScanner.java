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

package org.netbeans.modules.javafx.refactoring.impl.scanners;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.Tree;
import com.sun.tools.javafx.api.JavafxcTrees;
import java.io.IOException;
import java.util.EnumSet;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.modules.javafx.refactoring.impl.WhereUsedElement;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.TreePathHandle;
import org.netbeans.modules.refactoring.api.WhereUsedQuery;
import org.netbeans.modules.refactoring.spi.RefactoringElementsBag;
import org.openide.filesystems.FileObject;
import org.openide.util.lookup.Lookups;

/**
 *
 * @author Jaroslav Bachorik
 */
public class FindSubclassesScanner extends JavaFXTreePathScanner<Void, RefactoringElementsBag> {
    private TreePathHandle searchHandle;
    private ElementHandle elementHandle;
    private WhereUsedQuery refactoring;

    private CompilationController cc;
    private boolean recursive;

    public FindSubclassesScanner(WhereUsedQuery refactoring, TreePathHandle handle, CompilationController cc, boolean recursive) {
        this.searchHandle = handle;
        this.elementHandle = ElementHandle.create(handle.resolveElement(cc));
        this.refactoring = refactoring;
        this.recursive = recursive;
        this.cc = cc;
    }

    @Override
    public Void visitClassDeclaration(ClassDeclarationTree node, final RefactoringElementsBag elements) {
        switch (elementHandle.getKind()) {
            case CLASS:
            case INTERFACE: {
                for(ExpressionTree et : node.getSupertypeList()) {
                    JavaFXTreePath suprPath = JavafxcTrees.getPath(getCurrentPath(), et);
                    TypeElement supr = (TypeElement)cc.getTrees().getElement(suprPath);
                    if (supr.getQualifiedName().contentEquals(elementHandle.getQualifiedName())) {
                        elements.add(refactoring, WhereUsedElement.create(TreePathHandle.create(suprPath, cc), Lookups.singleton(searchHandle)));
                        if (recursive) {
                            ClassIndex ci = cc.getClasspathInfo().getClassIndex();
                            final TreePathHandle tpHandle = TreePathHandle.create(getCurrentPath(), cc);
                            ElementHandle eHandle = ElementHandle.create(cc.getTrees().getElement(getCurrentPath()));

                            for(FileObject fo : ci.getResources(eHandle, EnumSet.of(ClassIndex.SearchKind.IMPLEMENTORS), EnumSet.allOf(ClassIndex.SearchScope.class))) {
                                JavaFXSource jfxs = JavaFXSource.forFileObject(fo);
                                try {
                                    jfxs.runUserActionTask(new Task<CompilationController>() {
                                        public void run(CompilationController controller) throws Exception {
                                            new FindSubclassesScanner(refactoring, tpHandle, controller, recursive).scan(controller.getCompilationUnit(), elements);
                                        }
                                    }, true);
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }

                            }
                        }
                    }
                }
            }
        }
        return super.visitClassDeclaration(node, elements);
    }
}
