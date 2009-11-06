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

import com.sun.javafx.api.tree.CatchTree;
import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import java.util.Collection;
import java.util.HashSet;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.support.EditorAwareJavaFXSourceTaskFactory;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.spi.editor.hints.HintsController;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.TryTree;
import com.sun.tools.javac.code.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import javax.swing.text.Document;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.modules.javafx.editor.hints.HintsModel.Hint;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.netbeans.spi.editor.hints.ErrorDescriptionFactory;
import org.netbeans.spi.editor.hints.Fix;
import org.netbeans.spi.editor.hints.Severity;
import org.openide.filesystems.FileObject;
import org.openide.util.NbBundle;

/**
 *
 * @author karol harezlak
 */
public class UncaughtExceptionsTaskFactory extends EditorAwareJavaFXSourceTaskFactory {

    private static final String HINTS_IDENT = "trycatchjavafx"; //NOI18N

    public UncaughtExceptionsTaskFactory() {
        super(JavaFXSource.Phase.ANALYZED, JavaFXSource.Priority.NORMAL);
    }

    @Override
    protected CancellableTask<CompilationInfo> createTask(final FileObject file) {
        return new CancellableTask<CompilationInfo>() {

            @Override
            public void cancel() {
            }

            @Override
            public void run(CompilationInfo compilationInfo) throws Exception {
                final Document document = compilationInfo.getDocument();
                if (!compilationInfo.getDiagnostics().isEmpty()) {
                    if (document != null) {
                        HintsController.setErrors(compilationInfo.getDocument(), HINTS_IDENT, Collections.EMPTY_LIST);
                    }
                    return;
                }
                final UncaughtExceptionsVisitor tcw = new UncaughtExceptionsVisitor(compilationInfo);
                final HintsModel model = new HintsModel(compilationInfo);
                tcw.scan(compilationInfo.getCompilationUnit(), model);
                new UncaughtExceptionsVisitorResolver().scan(compilationInfo.getCompilationUnit(), model);
                Collection<ErrorDescription> errors = new HashSet<ErrorDescription>();
                for (Hint hint : model.getHints()) {
                    errors.add(getErrorDescription(file, hint, compilationInfo));
                }
                if (document != null) {
                    HintsController.setErrors(compilationInfo.getDocument(), HINTS_IDENT, errors); 
                }
            }
        };
    }

    private ErrorDescription getErrorDescription(FileObject file, Hint hint, CompilationInfo compilationInfo) {
        StringBuilder sb = new StringBuilder(" "); //NOI18N
        Iterator<Type> iterator = hint.getExceptions().iterator();
        while (iterator.hasNext()) {
            sb.append(iterator.next().toString());
            if (iterator.hasNext()) {
                sb.append(", "); //NOI18N
            }
        }
        Fix fix = new UncaughtExceptionsFix(compilationInfo.getDocument(), hint, compilationInfo);
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        int start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), hint.getTree());
        int end = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), hint.getTree());
        ErrorDescription ed = ErrorDescriptionFactory.createErrorDescription(Severity.HINT, NbBundle.getMessage(UncaughtExceptionsTaskFactory.class, "TITLE_UNREPORTED_EXCEPTION") + sb, //NOI18N
                Collections.singletonList(fix), file, start, end);

        return ed;
    }

    private class UncaughtExceptionsVisitorResolver extends JavaFXTreePathScanner<Void, HintsModel> {

        @Override
        public Void visitTry(TryTree node, HintsModel model) {
            Collection<Hint> hints = new HashSet<Hint>(model.getHints());
            Collection<ExpressionTree> nodes = new ArrayList<ExpressionTree>(node.getBlock().getStatements());
            nodes.add(node.getBlock().getValue());
            for (Tree node_ : nodes) {
                for (Hint hint : hints) {
                    if (hint.getTree() == node_) {
                        Collection<Type> hintTypes = new ArrayList<Type>(hint.getExceptions());
                        for (Type hintType : hintTypes) {
                            //TODO JavaFXTypeClass does not provide full class name, it should use full class names not simple names
                            for (CatchTree catchType : node.getCatches()) {
                                String hintTypeName = HintsUtils.getMethodName(hintType.toString());
                                if (catchType.getParameter() == null) {
                                    continue;
                                }
                                String catchTypeName = HintsUtils.getMethodName(catchType.getParameter().getType().toString());
                                if (hintTypeName.equals(catchTypeName)) {
                                    hint.removeException(hintType);
                                }
                            }
                        }
                        if (hint.getExceptions().isEmpty()) {
                            model.removeHint(hint);
                        } else {
                            model.addCatchTree(hint, node);
                        }
                    }
                }
            }
            return super.visitTry(node, model);
        }
    }
}
