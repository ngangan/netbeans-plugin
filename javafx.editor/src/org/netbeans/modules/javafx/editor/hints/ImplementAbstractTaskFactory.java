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
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.Tree;


import java.util.Collection;
import java.util.HashSet;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.support.EditorAwareJavaSourceTaskFactory;
import org.netbeans.api.javafx.source.JavaFXSource;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.tools.javac.code.Symbol.MethodSymbol;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.swing.text.Document;
import org.netbeans.api.javafx.editor.FXSourceUtils;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.editor.hints.HintsModel.Hint;
import org.netbeans.spi.editor.hints.ChangeInfo;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.netbeans.spi.editor.hints.ErrorDescriptionFactory;
import org.netbeans.spi.editor.hints.Fix;
import org.netbeans.spi.editor.hints.HintsController;
import org.netbeans.spi.editor.hints.Severity;
import org.openide.filesystems.FileObject;

/**
 *
 * @author karol harezlak
 */
public class ImplementAbstractTaskFactory extends EditorAwareJavaSourceTaskFactory {

    private EnumSet<ClassIndex.SearchScope> SCOPE = EnumSet.of(ClassIndex.SearchScope.SOURCE);

    public ImplementAbstractTaskFactory() {
        super(JavaFXSource.Phase.ANALYZED, JavaFXSource.Priority.LOW);
    }

    @Override
    protected CancellableTask<CompilationInfo> createTask(final FileObject file) {
        return new CancellableTask<CompilationInfo>() {

            public void cancel() {
            }

            public void run(final CompilationInfo compilationInfo) throws Exception {
                final Collection<Tree> classTrees = new HashSet<Tree>();
                final List<MethodSymbol> abstractMethods = new ArrayList<MethodSymbol>();
                final List<Tree> lastTree = new ArrayList<Tree>(1);
                final Collection<Tree> existingMethods = new HashSet<Tree>();

                JavaFXTreePathScanner<Void, HintsModel> visitor = new JavaFXTreePathScanner<Void, HintsModel>() {

                    @Override
                    public Void visitClassDeclaration(ClassDeclarationTree node, HintsModel p) {
                        lastTree.add(0, node);
                        if (node.getExtends() != null) {
                            for (Tree extendsTree : node.getExtends()) {
                                classTrees.add(extendsTree);
                            }
                        }

                        return super.visitClassDeclaration(node, p);
                    }

                    @Override
                    public Void visitFunctionDefinition(FunctionDefinitionTree node, HintsModel p) {
                        
                        return super.visitFunctionDefinition(node, p);
                    }


                };

                visitor.scan(compilationInfo.getCompilationUnit(), null);
                ClassIndex classIndex = ClasspathInfo.create(file).getClassIndex();
                for (Tree tree : classTrees) {
                    Set<ElementHandle<TypeElement>> options = classIndex.getDeclaredTypes(tree.toString(), ClassIndex.NameKind.SIMPLE_NAME, SCOPE);
                    for (ElementHandle<TypeElement> elementHandle : options) {
                        TypeElement typeElement = elementHandle.resolve(compilationInfo);
                        if (typeElement == null) {
                            continue;
                        }
                        Collection<? extends Element> elements = compilationInfo.getElements().getAllMembers(typeElement);
                        for (Element element : elements) {
                            boolean isAbstract = false;
                            boolean isPublic = false;
                            if (element instanceof MethodSymbol) {
                                MethodSymbol methodSymbol = (MethodSymbol) element;
                                for (Modifier modifier : methodSymbol.getModifiers()) {
                                    if (modifier == Modifier.ABSTRACT) {
                                        isAbstract = true;
                                    } else if (modifier == Modifier.PUBLIC) {
                                        isPublic = true;
                                    }
                                }
                                if (isAbstract && isPublic) {
                                    abstractMethods.add(methodSymbol);
                                }
                            }
                        }
                    }
                }

                HintsModel model = new HintsModel(compilationInfo);
                for (Tree tree : classTrees) {
                    model.addHint(tree, abstractMethods);
                }
                if (model.getHints() != null) {
                    Collection<ErrorDescription> errors = new HashSet<ErrorDescription>();
                    for (Hint hint : model.getHints()) {
                        errors.add(getErrorDescription(file, hint, compilationInfo, lastTree.get(0))); //NOI18N
                    }
                    HintsController.setErrors(FXSourceUtils.getDocument(file), "Non implemented methods", errors); //NOI18N
                }
            }
        };
    }

    private ErrorDescription getErrorDescription(final FileObject file, final Hint hint, final CompilationInfo compilationInfo, final Tree lastTree) {
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        int start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), hint.getTree());
        int end = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), hint.getTree());

        Fix fix = new Fix() {

            public String getText() {
                return "Implement all abstract methods"; //NOI18N
            }

            public ChangeInfo implement() throws Exception {
                Document document = FXSourceUtils.getDocument(file);
                int positon = findPositionAtTheEnd(compilationInfo, lastTree);
                for (MethodSymbol methodSymbol : hint.getAbstractMethods()) {
                    document.insertString(positon, createMethod(methodSymbol), null);
                }
                return null;
            }

            private String createMethod(MethodSymbol methodSymbol) {
                StringBuilder method = new StringBuilder("    override ");
                for (Modifier modifier : methodSymbol.getModifiers()) {
                    switch (modifier) {
                        case ABSTRACT:
                            method.append("public ");
                    }
                }
                method.append("function "); //NOI18N
                method.append(methodSymbol.getQualifiedName() + " () ");
                method.append(" : ").append(methodSymbol.getReturnType().toString()).append(" { \n");
                method.append("        throw new UnsupportedOperationException('Not implemented yet');\n");
                method.append("    }\n");

                return method.toString();
            }
        };

        ErrorDescription ed = ErrorDescriptionFactory.createErrorDescription(Severity.HINT, "Implement all abstract methods", Collections.singletonList(fix), file, start, end);

        return ed;
    }

    private int findPositionAtTheEnd(CompilationInfo compilationInfo, Tree tree) {
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        int end = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), tree);
        return end;
    }
}
