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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2008 Sun
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
package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.tools.mjavac.code.Symbol.ClassSymbol;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.javafx.source.support.EditorAwareJavaFXSourceTaskFactory;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;

/**
 *
 * @author karol harezlak
 */
public final class TestTaskFactory extends EditorAwareJavaFXSourceTaskFactory {

    private static final EnumSet<ClassIndex.SearchScope> SCOPE = EnumSet.of(ClassIndex.SearchScope.SOURCE);
    private static final EnumSet<ClassIndex.SearchKind> SEARCH = EnumSet.of(ClassIndex.SearchKind.TYPE_DEFS);
    private final AtomicBoolean cancel = new AtomicBoolean();

    public TestTaskFactory() {
        super(JavaFXSource.Phase.ANALYZED, JavaFXSource.Priority.NORMAL);
    }

    @Override
    protected CancellableTask<CompilationInfo> createTask(final FileObject file) {

        return new CancellableTask<CompilationInfo>() {

            @Override
            public void cancel() {
                cancel.set(true);
            }

            @Override
            public void run(final CompilationInfo compilationInfo) throws Exception {
                //To get Index
                //IndexingUtilities.getIndexValue(null);

                new JavaFXTreePathScanner() {

                    @Override
                    public Object visitClassDeclaration(ClassDeclarationTree node, Object p) {
                        JavaFXTreePath path = compilationInfo.getTrees().getPath(compilationInfo.getCompilationUnit(), node);
                        if (path == null) {
                            return super.visitClassDeclaration(node, p);
                        }
                        Element element = compilationInfo.getTrees().getElement(path);
                        if (element == null) {
                            return super.visitClassDeclaration(node, p);
                        }
                        ClassIndex classIndex = compilationInfo.getClasspathInfo().getClassIndex();
                        Set<ElementHandle<TypeElement>> handels = classIndex.getDeclaredTypes(((ClassSymbol) element).getSimpleName().toString(), ClassIndex.NameKind.SIMPLE_NAME, SCOPE);
                        if (!(element instanceof ClassSymbol)) {
                            return super.visitClassDeclaration(node, p);
                        }
                        for (ElementHandle<TypeElement> elementHandle : handels) {
                            if (!elementHandle.getQualifiedName().toString().equals(((ClassSymbol) element).getQualifiedName().toString())) {
                                continue;
                            }
                            Set<FileObject> fileObjects =  classIndex.getResources(elementHandle, SEARCH, SCOPE);
                            if (!fileObjects.isEmpty()) {
                                FileObject fileObject = fileObjects.iterator().next();
                                JavaFXSource jfxs = JavaFXSource.forFileObject(fileObject);
                                Task task = new Task<CompilationInfo>() {

                                    public void run(CompilationInfo parameter) throws Exception {
                                        System.out.println(parameter.getFileObject().getName());
                                    }

                                };
                                try {
                                    jfxs.runWhenScanFinished(task, true);
                                } catch (IOException ex) {
                                    Exceptions.printStackTrace(ex);
                                }
                            }
                        }

                        return super.visitClassDeclaration(node, p);
                    }
                }.scan(compilationInfo.getCompilationUnit(), null);
            }
        };

    }


}
