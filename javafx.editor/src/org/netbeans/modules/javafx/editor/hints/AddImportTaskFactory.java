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

import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.tools.javafx.code.JavafxClassSymbol;
import java.util.*;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.swing.text.*;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.support.EditorAwareJavaSourceTaskFactory;
import org.netbeans.api.javafx.source.JavaFXSource;
import javax.tools.Diagnostic;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.Imports;
import org.netbeans.editor.Utilities;
import org.netbeans.spi.editor.hints.*;
import org.openide.filesystems.FileObject;
import org.openide.util.NbBundle;

/**
 *
 * @author karol harezlak
 */
public final class AddImportTaskFactory extends EditorAwareJavaSourceTaskFactory {

    private final static EnumSet<ClassIndex.SearchScope> SCOPE = EnumSet.of(ClassIndex.SearchScope.SOURCE, ClassIndex.SearchScope.DEPENDENCIES);
    private final static String HINTS_IDENT = "addimportjavafx"; //NOI18N
    private final static String ERROR_CODE1 = "compiler.err.cant.resolve.location";//NOI18N
    private final static String ERROR_CODE2 = "compiler.err.cant.resolve";//NOI18N

    public AddImportTaskFactory() {
        super(JavaFXSource.Phase.ANALYZED, JavaFXSource.Priority.LOW);
    }

    @Override
    protected CancellableTask<CompilationInfo> createTask(final FileObject file) {
        return new CancellableTask<CompilationInfo>() {

            @Override
            public void cancel() {
            }

            @Override
            public void run(final CompilationInfo compilationInfo) throws Exception {
                if (file == null) {
                    throw new IllegalArgumentException();
                }
                if (compilationInfo.getDocument() != null) {
                    HintsController.setErrors(compilationInfo.getDocument(), HINTS_IDENT, Collections.EMPTY_LIST);
                }
                if (!compilationInfo.isErrors()) {
                    return;
                }
                ClassIndex classIndex = ClasspathInfo.create(file).getClassIndex();
                List<ErrorDescription> errors = new ArrayList<ErrorDescription>();
                for (Diagnostic diagnostic : compilationInfo.getDiagnostics()) {
                    boolean onlyAbstractError = false;
                    if (diagnostic.getCode().equals(ERROR_CODE1) || diagnostic.getCode().equals(ERROR_CODE2)) {
                        onlyAbstractError = true;
                    }
                    if (!onlyAbstractError) {
                        continue;
                    }
                    final Collection<String> imports = new HashSet<String>();
                    new JavaFXTreePathScanner<Void, Void>() {

                        @Override
                        public Void visitImport(ImportTree node, Void p) {
                            node.getQualifiedIdentifier();
                            JavaFXTreePath path = compilationInfo.getTrees().getPath(compilationInfo.getCompilationUnit(), node.getQualifiedIdentifier());
                            Element element = compilationInfo.getTrees().getElement(path);
                            if (element instanceof JavafxClassSymbol) {
                                JavafxClassSymbol classSymbol = (JavafxClassSymbol) element;
                                imports.add(classSymbol.getQualifiedName().toString());
                            }

                            return super.visitImport(node, p);
                        }
                    }.scan(compilationInfo.getCompilationUnit(), null);
                    final int start = (int) diagnostic.getStartPosition();
                    int end = (int) diagnostic.getEndPosition();
                    int length = end - start;
                    String potentialFqn = null;
                    try {
                        potentialFqn = compilationInfo.getDocument().getText(start, length);
                    } catch (BadLocationException ex) {
                        ex.printStackTrace();
                    }
                    if (potentialFqn == null || potentialFqn.length() == 0) {
                        return;
                    }
                    potentialFqn = HintsUtils.getClassSimpleName(potentialFqn);
                    Set<ElementHandle<TypeElement>> options = classIndex.getDeclaredTypes(potentialFqn, ClassIndex.NameKind.SIMPLE_NAME, SCOPE);
                    List<Fix> listFQN = new ArrayList<Fix>();
                    boolean exists = false;
                    for (ElementHandle<TypeElement> elementHandle : options) {
                        potentialFqn = elementHandle.getQualifiedName();
                        for (String importFQN : imports) {
                            if (potentialFqn.equals(importFQN)) {
                                exists = true;
                                break;
                            }
                        }
                        if (!exists) {
                            listFQN.add(new FixImport(potentialFqn));
                        }
                    }
                    if (listFQN.isEmpty()) {
                        continue;
                    }
                    ErrorDescription er = ErrorDescriptionFactory.createErrorDescription(Severity.HINT, "", listFQN, compilationInfo.getFileObject(), start, end);//NOI18N
                    errors.add(er);
                }
                HintsController.setErrors(compilationInfo.getDocument(), HINTS_IDENT, errors);
            }
        };
    }

    private class FixImport implements Fix {

        private String fqn;

        public FixImport(String fqn) {
            this.fqn = fqn;
        }

        public String getText() {
            return NbBundle.getMessage(AddImportTaskFactory.class, "TITLE_ADD_IMPORT") + fqn; //NOI18N
        }

        public ChangeInfo implement() throws Exception {
            JTextComponent target = Utilities.getFocusedComponent();
            Imports.addImport(target, fqn);
            return null;
        }
    }
}
