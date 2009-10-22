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

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.tools.javac.code.Symbol.MethodSymbol;
import com.sun.tools.javafx.code.JavafxClassSymbol;
import com.sun.tools.javafx.tree.JFXClassDeclaration;
import com.sun.tools.javafx.tree.JFXImport;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.support.EditorAwareJavaFXSourceTaskFactory;
import org.netbeans.api.javafx.source.JavaFXSource;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.swing.SwingUtilities;
import javax.swing.text.*;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.openide.filesystems.FileObject;
import org.openide.text.Annotation;
import org.openide.text.NbDocument;

/**
 *
 * @author karol harezlak
 */
public final class MarkOverriddenTaskFactory extends EditorAwareJavaFXSourceTaskFactory {

    private static final EnumSet<ClassIndex.SearchScope> SCOPE = EnumSet.of(ClassIndex.SearchScope.SOURCE, ClassIndex.SearchScope.DEPENDENCIES);
    private static final String ANNOTATION_TYPE = "org.netbeans.modules.javafx.editor.hints"; //NOI18N
    private final Map<Document, Collection<OverriddeAnnotation>> annotations = new WeakHashMap<Document, Collection<OverriddeAnnotation>>();
    private final AtomicBoolean cancel = new AtomicBoolean();

    public MarkOverriddenTaskFactory() {
        super(JavaFXSource.Phase.ANALYZED, JavaFXSource.Priority.LOW);
    }

    private void updateAnnotationsoverridden(CompilationInfo compilationInfo, Collection<OverriddeAnnotation> addedAnnotations) {
        final StyledDocument document = (StyledDocument) compilationInfo.getDocument();
        final Collection<OverriddeAnnotation> annotationsToRemoveCopy = annotations.get(document) == null ? null : new HashSet<OverriddeAnnotation>(annotations.get(document));
        final Collection<OverriddeAnnotation> addedAnnotationsCopy = new HashSet<OverriddeAnnotation>(addedAnnotations);

        Runnable update = new Runnable() {

            public void run() {
                if (document == null) {
                    return;
                }
                if (annotationsToRemoveCopy != null) {
                    for (Annotation annotation : annotationsToRemoveCopy) {
                        NbDocument.removeAnnotation(document, annotation);
                    }
                }
                for (OverriddeAnnotation annotation : addedAnnotationsCopy) {
                    Position position = null;
                    try {
                        position = document.createPosition(annotation.getPosition());
                    } catch (BadLocationException ex) {
                        ex.printStackTrace();
                    }
                    if (document != null && position != null) {
                        NbDocument.addAnnotation(document, position, annotation.getPosition(), annotation);
                    }
                }
                annotations.put(document, addedAnnotationsCopy);
            }
        };
        runRunnable(update);
    }

    private void runRunnable(Runnable task) {
        if (SwingUtilities.isEventDispatchThread()) {
            task.run();
        } else {
            SwingUtilities.invokeLater(task);
        }
    }

    @Override
    protected CancellableTask<CompilationInfo> createTask(final FileObject file) {
        final Map<Element, Collection<Tree>> classTrees = new HashMap<Element, Collection<Tree>>();
        final Map<Element, List<MethodSymbol>> overriddenMethods = new HashMap<Element, List<MethodSymbol>>();
        final Collection<OverriddeAnnotation> addedAnotations = new HashSet<OverriddeAnnotation>();
        final Collection<JFXImport> imports = new HashSet<JFXImport>();
        final Collection<Element> classesKeys = new HashSet<Element>(overriddenMethods.keySet());
        final Map<String, Collection<ElementHandle<TypeElement>>> optionsCache = new HashMap<String, Collection<ElementHandle<TypeElement>>>();
        final Map<ElementHandle<TypeElement>, TypeElement> typeElementCash = new HashMap<ElementHandle<TypeElement>, TypeElement>();
        final Map<TypeElement, Collection<? extends Element>> elementsCash = new HashMap<TypeElement, Collection<? extends Element>>();

        return new CancellableTask<CompilationInfo>() {

            public void cancel() {
                cancel.set(true);
            }

            public void run(final CompilationInfo compilationInfo) throws Exception {
                cancel.set(false);
                final JavaFXTreePathScanner<Void, Void> visitor = new OverrideVisitor(compilationInfo, classTrees, overriddenMethods, imports, true);
                visitor.scan(compilationInfo.getCompilationUnit(), null);
                for (Element classElement : classesKeys) {
                    if (overriddenMethods.size() == 0
                            || !isAnnon(classElement)
                            && HintsUtils.checkString(classElement.getSimpleName().toString())) {
                        updateAnnotationsoverridden(compilationInfo, addedAnotations);
                        overriddenMethods.remove(classElement);
                        clear();
                        return;
                    }
                }
                final ClassIndex classIndex = ClasspathInfo.create(file).getClassIndex();
                for (Element currentClass : classTrees.keySet()) {
                    if (cancel.get()) {
                        clear();
                        return;
                    }
                    Collection<Tree> superTypes = classTrees.get(currentClass);
                    if (superTypes == null) {
                        continue;
                    }
                    Collection<MethodSymbol> methods = overriddenMethods.get(currentClass);
                    if (methods == null) {
                        continue;
                    }
                    boolean isCurrentClass = false;
                    for (MethodSymbol method : methods) {
                        if (method.owner == currentClass) {
                            isCurrentClass = true;
                        }
                    }
                    if (!isCurrentClass) {
                        continue;
                    }
                    Tree currentClassTree = compilationInfo.getTrees().getTree(currentClass);
                    superTypes.add(currentClassTree);
                    for (Tree superTree : superTypes) {
                        if (superTree == null) {
                            continue;
                        }

                        String className = null;
                        if (superTree instanceof JFXClassDeclaration) {
                            className = ((JFXClassDeclaration) superTree).getSimpleName().toString();
                        } else {
                            className = HintsUtils.getClassSimpleName(superTree.toString());
                        }
                        if (HintsUtils.checkString(className)) {
                            continue;
                        }
                        Collection<ElementHandle<TypeElement>> options = optionsCache.get(className);
                        if (options == null) {
                            options = classIndex.getDeclaredTypes(className, ClassIndex.NameKind.SIMPLE_NAME, SCOPE);
                            optionsCache.put(className, options);
                        }
                        if (options == null) {
                            continue;
                        }
                        for (ElementHandle<TypeElement> elementHandle : options) {
                            TypeElement typeElement = typeElementCash.get(elementHandle);
                            if (typeElement == null) {
                                typeElement = elementHandle.resolve(compilationInfo);
                                typeElementCash.put(elementHandle, typeElement);
                            }
                            if (typeElement == null) {
                                continue;
                            }
//                            if (!HintsUtils.isClassUsed(typeElement, imports, compilationInfo, classTrees.keySet(), superElement)) {
//                                continue;
//                            }
                            Collection<? extends Element> elements = elementsCash.get(typeElement);
                            if (elements == null) {
                                elements = getAllMembers(typeElement, compilationInfo);
                                elementsCash.put(typeElement, elements);
                            }
                            if (elements == null) {
                                continue;
                            }
                            //Collection<MethodSymbol> overriddens = new HashSet<MethodSymbol>();
                            for (Element element : elements) {
                                if (element instanceof ExecutableElement) {
                                    MethodSymbol overridden = HintsUtils.isAlreadyDefined(methods, (MethodSymbol) element, compilationInfo);
                                    if (overridden != null) {
                                        Tree tree = compilationInfo.getTrees().getTree(overridden);
                                        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
                                        int start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), tree);
                                        addedAnotations.add(new OverriddeAnnotation(start, element.getEnclosingElement()));
                                    }
                                }
                            }
                        }

                    }
                }
                updateAnnotationsoverridden(compilationInfo, addedAnotations);
                clear();
            }

            private void clear() {
                addedAnotations.clear();
                classTrees.clear();
                overriddenMethods.clear();
                imports.clear();
                classesKeys.clear();
                optionsCache.clear();
                typeElementCash.clear();
                elementsCash.clear();
            }
        };
    }

    private boolean isAnnon(Element element) {
        if (!(element instanceof JavafxClassSymbol)) {
            return false;
        }
        JavafxClassSymbol classSymbol = ((JavafxClassSymbol) element);
        classSymbol.getSuperTypes();
        if (!classSymbol.isLocal()) {
            return false;
        }
        String name = element.toString();
        int lastIndex = name.lastIndexOf("$"); //NOI18N
        if (lastIndex < 0) {
            return false;
        }
        if (!name.substring(lastIndex).contains("anon")) { //NOI18N
            return false;
        }
        return true;
    }

    private MethodSymbol checkIfOveridden(Collection<MethodSymbol> elementsToCheck, MethodSymbol overridden) {
        //String overrridenName = overridden.getSimpleName().toString();
//        for (ExecutableElement override : elementsToCheck) {
////            if (!overridden.getSimpleName().toString().equals(overrridenName)) {
////                continue;
////            }
////            TypeElement typeOverridden = ElementUtilities.enclosingTypeElement(overridden);
////            if (typeOverridden == null) {
////                continue;
////            }
//            //TODO Workaround for java.lang.NullPointerException at com.sun.tools.javac.code.Types$DefaultTypeVisitor.visit(Types.java:3183)
////            try {
////                if (compilationInfo.getElements().overrides(override, overridden, typeOverridden)) {
////                    return override;
////                }
////            } catch (Exception npe) {
////                npe.printStackTrace();
////            }
//
//
//        }
        // Work around for a problem with mixin compilationInfo.getElements().overrides(override, overridden, typeOverridden) which does not work with mixin
        return null;

    }

    private Collection<? extends Element> getAllMembers(TypeElement typeElement, CompilationInfo compilationInfo) {
        Collection<? extends Element> elements = null;
        try {
            elements = compilationInfo.getElements().getAllMembers(typeElement);
        } catch (NullPointerException npe) {
            npe.printStackTrace();
            System.err.println("* e = " + typeElement); //NOI18N
            System.err.println("* e.getKind() = " + typeElement.getKind()); //NOI18N
            System.err.println("* e.asType() = " + typeElement.asType()); //NOI18N
        }
        return elements;
    }

    private static class OverriddeAnnotation extends Annotation {

        private int positon;
        private String superClassFQN;

        public OverriddeAnnotation(int position, Element element) {
            this.positon = position;
            if (element instanceof JavafxClassSymbol) {
                JavafxClassSymbol superClassSymbol = (JavafxClassSymbol) element;
                superClassFQN = superClassSymbol.getQualifiedName().toString();
            }
        }

        @Override
        public String getAnnotationType() {
            return ANNOTATION_TYPE;
        }

        @Override
        public String getShortDescription() {
            return "Overrides: " + superClassFQN; //NOI18N
        }

        int getPosition() {
            return positon;
        }

        @Override
        public String toString() {
            return superClassFQN + " " + positon;
        }
    }
}
