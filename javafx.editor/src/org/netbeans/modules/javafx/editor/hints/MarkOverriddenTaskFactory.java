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

import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.tools.mjavac.code.Symbol.ClassSymbol;
import com.sun.tools.mjavac.code.Symbol.MethodSymbol;
import com.sun.tools.mjavac.code.Type;
import com.sun.tools.javafx.code.JavafxClassSymbol;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.support.EditorAwareJavaFXSourceTaskFactory;
import org.netbeans.api.javafx.source.JavaFXSource;
import java.util.*;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.lang.model.element.Element;
import javax.swing.SwingUtilities;
import javax.swing.text.*;
import javax.tools.Diagnostic;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.openide.filesystems.FileObject;
import org.openide.text.Annotation;
import org.openide.text.NbDocument;
import org.openide.util.NbBundle;

/**
 *
 * @author karol harezlak
 */
public final class MarkOverriddenTaskFactory extends EditorAwareJavaFXSourceTaskFactory {

    private static final String ANNOTATION_TYPE = "org.netbeans.modules.javafx.editor.hints"; //NOI18N
    private final Map<Document, Collection<OverriddeAnnotation>> annotations = new WeakHashMap<Document, Collection<OverriddeAnnotation>>();
    private final AtomicBoolean cancel = new AtomicBoolean();

    public MarkOverriddenTaskFactory() {
        super(JavaFXSource.Phase.ANALYZED, JavaFXSource.Priority.LOW);
    }

    @Override
    protected CancellableTask<CompilationInfo> createTask(final FileObject file) {
        final Map<Element, Collection<Tree>> classTrees = new HashMap<Element, Collection<Tree>>();
        final Map<Element, List<MethodSymbol>> overriddenMethods = new HashMap<Element, List<MethodSymbol>>();
        final Collection<OverriddeAnnotation> addedAnotations = new HashSet<OverriddeAnnotation>();
        final Collection<Element> classesKeys = new HashSet<Element>(overriddenMethods.keySet());

        return new CancellableTask<CompilationInfo>() {

            public void cancel() {
                cancel.set(true);
            }

            public void run(final CompilationInfo compilationInfo) throws Exception {
                cancel.set(false);
                final JavaFXTreePathScanner<Void, Void> visitor = new OverrideVisitor(compilationInfo, classTrees, overriddenMethods, true);
                visitor.scan(compilationInfo.getCompilationUnit(), null);
                for (Element classElement : classesKeys) {
                    if (overriddenMethods.isEmpty() || (!HintsUtils.isAnnon(classElement) && HintsUtils.checkString(classElement.getSimpleName().toString()))) {
                        updateAnnotationsOverridden(compilationInfo, addedAnotations);
                        clear();
                        return;
                    }
                }
                SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
                for (Element currentClass : classTrees.keySet()) {
                    if (overriddenMethods.get(currentClass) == null || overriddenMethods.get(currentClass).isEmpty() ) {
                        continue;
                    }
                    ClassSymbol classSymbol = (ClassSymbol) currentClass;
                    List<MethodSymbol> methods = new ArrayList<MethodSymbol>(overriddenMethods.get(currentClass));
                    for (Element element : classSymbol.getEnclosedElements()) {
                        if (element instanceof MethodSymbol) {
                            MethodSymbol overridden = HintsUtils.isAlreadyDefined(methods, (MethodSymbol) element, compilationInfo);
                            if (overridden != null) {
                                methods.remove(overridden);
                                Tree tree = compilationInfo.getTrees().getTree(overridden);
                                int start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), tree);
                                boolean isInErrorZone = false;
                                for (Diagnostic diagnostic : compilationInfo.getDiagnostics()) {
                                    if (diagnostic.getStartPosition() <= start && diagnostic.getEndPosition() >= start) {
                                        isInErrorZone = true;
                                    }
                                }
                                if (start > 0 && !isInErrorZone) {
                                    addedAnotations.add(new OverriddeAnnotation(start, compilationInfo.getJavafxTypes().toJavaFXString(classSymbol.getSuperclass())));
                                }
                            }
                        }
                    }
                }
                updateAnnotationsOverridden(compilationInfo, addedAnotations);
                clear();
            }

            private void updateAnnotationsOverridden(CompilationInfo compilationInfo, Collection<OverriddeAnnotation> addedAnnotations) {
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

            private void clear() {
                addedAnotations.clear();
                classTrees.clear();
                overriddenMethods.clear();
                classesKeys.clear();
            }
        };
    }

  

    private static class OverriddeAnnotation extends Annotation {

        private int positon;
        private String superClassFQN;

        public OverriddeAnnotation(int position, String superClassFQN) {
            this.positon = position;
            this.superClassFQN = superClassFQN;
        }

        @Override
        public String getAnnotationType() {
            return ANNOTATION_TYPE;
        }

        @Override
        public String getShortDescription() {
            return NbBundle.getMessage(MarkOverriddenTaskFactory.class, "LABEL_OVERRIDES") + superClassFQN; //NOI18N
        }

        int getPosition() {
            return positon;
        }
    }
}
