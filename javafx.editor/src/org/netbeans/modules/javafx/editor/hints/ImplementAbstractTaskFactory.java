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
import com.sun.javafx.api.tree.CompoundAssignmentTree;
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.FunctionValueTree;
import com.sun.javafx.api.tree.InitDefinitionTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.MemberSelectTree;
import com.sun.javafx.api.tree.OnReplaceTree;
import com.sun.javafx.api.tree.Tree;


import com.sun.javafx.api.tree.TriggerTree;
import com.sun.javafx.api.tree.TypeClassTree;
import com.sun.javafx.api.tree.TypeFunctionalTree;
import com.sun.javafx.api.tree.TypeUnknownTree;
import java.util.Collection;
import java.util.HashSet;
import javax.swing.text.BadLocationException;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.support.EditorAwareJavaSourceTaskFactory;
import org.netbeans.api.javafx.source.JavaFXSource;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.tools.javac.code.Symbol.MethodSymbol;
import com.sun.tools.javac.code.Symbol.VarSymbol;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.swing.SwingUtilities;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import org.netbeans.api.javafx.editor.FXSourceUtils;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.Imports;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.javafx.editor.hints.HintsModel.Hint;
import org.netbeans.spi.editor.hints.ChangeInfo;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.netbeans.spi.editor.hints.ErrorDescriptionFactory;
import org.netbeans.spi.editor.hints.Fix;
import org.netbeans.spi.editor.hints.HintsController;
import org.netbeans.spi.editor.hints.Severity;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;

/**
 *
 * @author karol harezlak
 */
public class ImplementAbstractTaskFactory extends EditorAwareJavaSourceTaskFactory {

    private static final String EXCEPTION = "java.lang.UnsupportedOperationException"; //NOI18N
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
                final Map<Element, Collection<Tree>> classTrees = new HashMap<Element, Collection<Tree>>();
                final Map<Element, List<MethodSymbol>> abstractMethods = new HashMap<Element, List<MethodSymbol>>();
                final Map<Element, Collection<MethodSymbol>> overriddenMethods = new HashMap<Element, Collection<MethodSymbol>>();

                JavaFXTreePathScanner<Void, HintsModel> visitor = new JavaFXTreePathScanner<Void, HintsModel>() {

                    @Override
                    public Void visitClassDeclaration(ClassDeclarationTree node, HintsModel p) {
                        if (node.getExtends() != null) {
                            Element currentClass = compilationInfo.getTrees().getElement(getCurrentPath());
                            if (classTrees.get(currentClass) == null) {
                                classTrees.put(currentClass, new HashSet<Tree>());
                            }
                            Collection<Tree> extendsSet = classTrees.get(currentClass);
                            for (Tree extendsTree : node.getExtends()) {
                                extendsSet.add(extendsTree);
                            }
                            classTrees.put(currentClass, extendsSet);
                        }
                        return super.visitClassDeclaration(node, p);
                    }

                    @Override
                    public Void visitFunctionDefinition(FunctionDefinitionTree node, HintsModel p) {
                            if (node.toString().contains(" overridefunction ")) {
                                Element element = compilationInfo.getTrees().getElement(getCurrentPath());
                                Element currentClass = element.getEnclosingElement();
                                if (element instanceof MethodSymbol) {
                                    if (overriddenMethods.get(currentClass) == null) {
                                        overriddenMethods.put(currentClass, new HashSet<MethodSymbol>());
                                    }
                                    Collection<MethodSymbol> methods = overriddenMethods.get(currentClass);
                                    methods.add((MethodSymbol) element);
                                    overriddenMethods.put(currentClass, methods);
                                }
                            }
                        return super.visitFunctionDefinition(node, p);
                    }
                };

                visitor.scan(compilationInfo.getCompilationUnit(), null);
                ClassIndex classIndex = ClasspathInfo.create(file).getClassIndex();
                for (Element currentClass : classTrees.keySet()) {
                    for (Tree tree : classTrees.get(currentClass)) {
                        Set<ElementHandle<TypeElement>> options = classIndex.getDeclaredTypes(tree.toString(), ClassIndex.NameKind.SIMPLE_NAME, SCOPE);
                        for (ElementHandle<TypeElement> elementHandle : options) {
                            TypeElement typeElement = elementHandle.resolve(compilationInfo);
                            if (typeElement == null) {
                                continue;
                            }
                            Collection<? extends Element> elements = getAllMembers(typeElement, compilationInfo);
                            for (Element element : elements) {
                                if (element instanceof MethodSymbol) {
                                    MethodSymbol methodSymbol = (MethodSymbol) element;
                                    for (Modifier modifier : methodSymbol.getModifiers()) {
                                        if (modifier == Modifier.ABSTRACT) {
                                            Collection<MethodSymbol> overridenMethodList = overriddenMethods.get(currentClass);
//                                           if (overridenMethodList == null || overriddenMethods.size() == 0) {
//                                                continue;
//                                            }
//                                            for (MethodSymbol overridenMethodSymbol : overridenMethodList) {
//                                                if (methodSymbol.overrides(overridenMethodSymbol, overridenMethodSymbol.enclClass(), null, true)){
//                                                    System.out.println("");
//                                                }
//                                            }
                                            element.getEnclosingElement();
                                            if (abstractMethods.get(currentClass) == null) {
                                                abstractMethods.put(currentClass, new ArrayList<MethodSymbol>());
                                            }
                                            List<MethodSymbol> methods = abstractMethods.get(currentClass);
                                            methods.add(methodSymbol);
                                            abstractMethods.put(currentClass, methods);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                HintsModel model = new HintsModel(compilationInfo);
                for (Element currentClass : abstractMethods.keySet()) {
                    Tree currentTree = compilationInfo.getTrees().getTree(currentClass);
                    model.addHint(currentTree, abstractMethods.get(currentClass), currentClass);
                }
                if (model.getHints() != null) {
                    Collection<ErrorDescription> errors = new HashSet<ErrorDescription>();
                    for (Hint hint : model.getHints()) {
                        errors.add(getErrorDescription(file, hint, compilationInfo)); //NOI18N
                    }
                    HintsController.setErrors(FXSourceUtils.getDocument(file), "Non implemented methods", errors); //NOI18N
                }
            }
        };
    }
    //TODO Temporary log for issue 148890

    private Collection<? extends Element> getAllMembers(TypeElement typeElement, CompilationInfo compilationInfo) {
        Collection<? extends Element> elements = null;
        try {
            elements = compilationInfo.getElements().getAllMembers(typeElement);
        } catch (NullPointerException npe) {
            npe.printStackTrace();
            System.err.println("* e = " + typeElement);
            System.err.println("* e.getKind() = " + typeElement.getKind());
            System.err.println("* e.asType() = " + typeElement.asType());
        }
        return elements;
    }

    private ErrorDescription getErrorDescription(final FileObject file, final Hint hint, final CompilationInfo compilationInfo) {
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        int start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), hint.getTree());
        int end = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), hint.getTree());

        Fix fix = new Fix() {

            public String getText() {
                return "Implement all abstract methods"; //NOI18N
            }

            public ChangeInfo implement() throws Exception {
                final StringBuilder methods = new StringBuilder();

                for (MethodSymbol methodSymbol : hint.getAbstractMethods()) {
                    methods.append(createMethod(methodSymbol));
                }
                final Document document = FXSourceUtils.getDocument(file);

                final int positon = findPositionAtTheEnd(compilationInfo, hint.getTree());
                SwingUtilities.invokeLater(new Runnable() {

                    public void run() {
                        try {
                            document.insertString(positon, methods.toString(), null);
                            JTextComponent target = Utilities.getFocusedComponent();
                            Imports.addImport(target, EXCEPTION);
                            for (MethodSymbol method : hint.getAbstractMethods()) {
                                Imports.addImport(target, method.getReturnType().toString());
                                for (VarSymbol var : method.getParameters()) {
                                    Imports.addImport(target, var.asType().toString());
                                }
                            }
                        } catch (BadLocationException ex) {
                            Exceptions.printStackTrace(ex);
                        }
                    }
                });

                return null;
            }

            private String createMethod(MethodSymbol methodSymbol) {
                StringBuilder method = new StringBuilder("\n");
                method.append("\toverride ");
                for (Modifier modifier : methodSymbol.getModifiers()) {
                    switch (modifier) {
                        case PUBLIC:
                            method.append("public ");
                    }
                }

                method.append("function ").append(methodSymbol.getQualifiedName() + " (");
                Iterator<VarSymbol> iterator = methodSymbol.getParameters().iterator();
                while (iterator.hasNext()) {
                    VarSymbol var = iterator.next();
                    method.append(var.getSimpleName()).append(" : ").append(HintsUtils.getClassSimpleName(var.asType().toString()));
                    if (iterator.hasNext()) {
                        method.append(", ");
                    }
                }
                String returnType = methodSymbol.getReturnType().toString();
                if (returnType.equals("void")) {
                    returnType = "Void";
                } else {
                    returnType = HintsUtils.getClassSimpleName(returnType);
                } //NOI18N
                method.append(")").append(" : ").append(returnType).append(" { \n");
                method.append("\t\tthrow new UnsupportedOperationException('Not implemented yet');\n");
                method.append("\t}\n");

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
