/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
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
package org.netbeans.modules.visage.editor.hints;

import com.sun.visage.api.tree.ClassDeclarationTree;
import com.sun.visage.api.tree.IdentifierTree;
import com.sun.visage.api.tree.ImportTree;
import com.sun.visage.api.tree.VisageTreePath;
import com.sun.visage.api.tree.VisageTreePathScanner;
import com.sun.visage.api.tree.SourcePositions;
import com.sun.visage.api.tree.Tree;
import com.sun.tools.mjavac.code.Symbol.ClassSymbol;
import com.sun.tools.visage.tree.VSGInstanciate;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.swing.text.*;
import org.netbeans.api.visage.source.CancellableTask;
import org.netbeans.api.visage.source.VisageSource;
import javax.tools.Diagnostic;
import org.netbeans.api.visage.source.ClassIndex;
import org.netbeans.api.visage.source.CompilationInfo;
import org.netbeans.api.visage.source.ElementHandle;
import org.netbeans.spi.editor.hints.*;
import org.openide.filesystems.FileObject;
import org.openide.util.NbBundle;

/**
 *
 * @author karol harezlak
 */
public final class AddImportTaskFactory extends VisageAbstractEditorHint {

    private static final EnumSet<ClassIndex.SearchScope> SCOPE = EnumSet.of(ClassIndex.SearchScope.SOURCE, ClassIndex.SearchScope.DEPENDENCIES);
    private static final String HINTS_IDENT = "addimportvisage"; //NOI18N
    private static final String ERROR_CODE1 = "compiler.err.cant.resolve.location";//NOI18N
    private static final String ERROR_CODE2 = "compiler.err.cant.resolve";//NOI18N
    private static final String MESSAGE = NbBundle.getMessage(AddImportTaskFactory.class, "TITLE_ADD_IMPORT"); //NOI18N
    private static final Comparator IMPORT_COMPERATOR = new ImportComperator();
    private final AtomicBoolean cancel = new AtomicBoolean();

    public AddImportTaskFactory() {
        super(VisageSource.Phase.ANALYZED, VisageSource.Priority.NORMAL);
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
                cancel.set(false);
                if (compilationInfo.getDocument() == null) {
                    return;
                }
                if (!compilationInfo.isErrors()) {
                    HintsController.setErrors(compilationInfo.getDocument(), HINTS_IDENT, Collections.EMPTY_LIST);
                    return;
                }
                Collection<Diagnostic> diagnostics = getValidDiagnostics(compilationInfo.getDiagnostics());
                if (diagnostics.isEmpty()) {
                    HintsController.setErrors(compilationInfo.getDocument(), HINTS_IDENT, Collections.EMPTY_LIST);
                    return;
                }
                final Collection<ClassSymbol> imports = new HashSet<ClassSymbol>();
                final String[] currentPackageName = new String[1];
                final ClassIndex classIndex = compilationInfo.getClasspathInfo().getClassIndex();
                new VisageTreePathScanner<Void, Void>() {

                    @Override
                    public Void visitClassDeclaration(ClassDeclarationTree node, Void p) {
                        Element element = compilationInfo.getTrees().getElement(getCurrentPath());
                        if (element != null) {
                            currentPackageName[0] = compilationInfo.getElements().getPackageOf(element).getQualifiedName().toString();
                        }

                        return null;
                    }

                    @Override
                    public Void visitImport(ImportTree node, Void p) {
                        node.getQualifiedIdentifier();
                        VisageTreePath path = compilationInfo.getTrees().getPath(compilationInfo.getCompilationUnit(), node.getQualifiedIdentifier());
                        Element element = compilationInfo.getTrees().getElement(path);
                        if (element instanceof ClassSymbol) {
                            ClassSymbol classSymbol = (ClassSymbol) element;
                            imports.add(classSymbol);
                        }

                        return super.visitImport(node, p);
                    }
                }.scan(compilationInfo.getCompilationUnit(), null);
                final Map<String, Collection<ElementHandle<TypeElement>>> optionsCache = new HashMap<String, Collection<ElementHandle<TypeElement>>>();
                final List<ErrorDescription> errors = new ArrayList<ErrorDescription>();
                for (final Diagnostic diagnostic : diagnostics) {
                    if (cancel.get()) {
                        break;
                    }
                    VisageTreePath path = compilationInfo.getTreeUtilities().pathFor(diagnostic.getPosition());
                    Element element = compilationInfo.getTrees().getElement(path);
                    Tree superTree = path.getLeaf();
                    String potentialClassSimpleName = null;
                    if (element != null && element instanceof ClassSymbol) {
                        ClassSymbol classSymbol = (ClassSymbol) element;
                        potentialClassSimpleName = classSymbol.getSimpleName().toString();
                    } else if (superTree instanceof VSGInstanciate) {
                        final SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
                        final Tree[] tree = new Tree[1];
                        VisageTreePathScanner<Void, Void> scaner = new VisageTreePathScanner<Void, Void>() {

                            @Override
                            public Void visitIdentifier(IdentifierTree node, Void p) {
                                int position = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), node);
                                if (diagnostic.getStartPosition() == position) {
                                    tree[0] = node;

                                    return null;
                                }

                                return super.visitIdentifier(node, p);
                            }
                        };
                        scaner.scan(compilationInfo.getCompilationUnit(), null);
                        if (tree[0] != null) {
                            superTree = tree[0];
                            potentialClassSimpleName = superTree.toString();
                        }
                    }
                    if (potentialClassSimpleName == null || potentialClassSimpleName.length() == 0) {
                        continue;
                    }
                    if (isImportDefined(potentialClassSimpleName, imports)) {
                        continue;
                    }
                    potentialClassSimpleName = HintsUtils.getClassSimpleName(potentialClassSimpleName);
                    Collection<ElementHandle<TypeElement>> options = optionsCache.get(potentialClassSimpleName);
                    if (options == null) {
                        options = classIndex.getDeclaredTypes(potentialClassSimpleName, ClassIndex.NameKind.SIMPLE_NAME, SCOPE);
                        if (!options.isEmpty()) {
                            optionsCache.put(potentialClassSimpleName, options);
                        }
                    }
                    boolean exists = false;
                    List<Fix> fixesPerDiagnostic = new ArrayList<Fix>();
                    for (ElementHandle<TypeElement> elementHandle : options) {
                        potentialClassSimpleName = elementHandle.getQualifiedName();
                        String packageName = HintsUtils.getPackageName(potentialClassSimpleName);
                        if (packageName.length() == 0 || packageName.equals(currentPackageName[0])) {
                            continue;
                        }
                        for (ClassSymbol importFQN : imports) {
                            if (potentialClassSimpleName.equals(importFQN.getQualifiedName().toString())) {
                                exists = true;
                                break;
                            }
                        }
                        if (!exists) {
                            fixesPerDiagnostic.add(new FixImport(potentialClassSimpleName, compilationInfo.getDocument()));
                        }
                    }
                    if (fixesPerDiagnostic.isEmpty()) {
                        continue;
                    }
                    Collections.sort(fixesPerDiagnostic, IMPORT_COMPERATOR);
                    ErrorDescription er = ErrorDescriptionFactory.createErrorDescription(Severity.HINT, "", fixesPerDiagnostic, compilationInfo.getFileObject(), (int) diagnostic.getStartPosition(), (int) diagnostic.getEndPosition());//NOI18N
                    errors.add(er);
                }
                HintsController.setErrors(compilationInfo.getDocument(), HINTS_IDENT, errors);
            }

            private Collection<Diagnostic> getValidDiagnostics(Collection<Diagnostic> diagnostics) {
                Collection<Diagnostic> validDiagnostics = new HashSet<Diagnostic>();
                Collection<Long> errorLines = new HashSet<Long>();
                for (Diagnostic diagnostic : diagnostics) {
                    if (errorLines.contains(diagnostic.getLineNumber())) { //NOI18N
                        continue;
                    }
                    if (diagnostic.getCode().equals(ERROR_CODE1) || diagnostic.getCode().equals(ERROR_CODE2)) {
                        validDiagnostics.add(diagnostic);
                        errorLines.add(diagnostic.getLineNumber());
                    }
                }

                return validDiagnostics;
            }

            private boolean isImportDefined(String className, Collection<ClassSymbol> imports) {
                for (ClassSymbol importFQN : imports) {
                    if (importFQN.getSimpleName().toString().equals(className)) {
                        return true;
                    }
                }

                return false;
            }
        };
    }

    private static class FixImport implements Fix {

        private String fqn;
        private Document document;

        FixImport(String fqn, Document document) {
            this.fqn = fqn;
            this.document = document;
        }

        public String getFQN() {
            return this.fqn;
        }

        public String getText() {
            return MESSAGE + fqn;
        }

        public ChangeInfo implement() throws Exception {
            Runnable runnable = new Runnable() {

                public void run() {
                    HintsUtils.addImport(document, fqn);
                }
            };
            HintsUtils.runInAWT(runnable);

            return null;
        }
    }

    private static class ImportComperator implements Comparator<Fix> {

        public int compare(Fix fix1, Fix fix2) {
            FixImport fixImport1 = (FixImport) fix1;
            String fqnName1 = fixImport1.getFQN();
            if (fqnName1.contains(".")) { // NOI18N
                int index = fqnName1.indexOf("."); //NOI18N
                String pn = fqnName1.substring(0, index);
                if (pn.contains("visage")) { //NOI18N
                    return -1;
                }
            }

            return 1;
        }
    }
}
