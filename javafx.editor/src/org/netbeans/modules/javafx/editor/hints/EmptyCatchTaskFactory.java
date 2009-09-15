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
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import java.util.Collection;
import java.util.HashSet;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.support.EditorAwareJavaSourceTaskFactory;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.spi.editor.hints.HintsController;
import com.sun.javafx.api.tree.SourcePositions;
import java.util.Collections;

import org.netbeans.api.javafx.editor.FXSourceUtils;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.modules.javafx.editor.hints.HintsModel.Hint;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.netbeans.spi.editor.hints.ErrorDescriptionFactory;
import org.netbeans.spi.editor.hints.Fix;
import org.netbeans.spi.editor.hints.Severity;
import org.openide.filesystems.FileObject;

/**
 *
 * @author karol harezlak
 */
public class EmptyCatchTaskFactory extends EditorAwareJavaSourceTaskFactory {

    public EmptyCatchTaskFactory() {
        super(JavaFXSource.Phase.ANALYZED, JavaFXSource.Priority.LOW);
    }


    @Override
    protected CancellableTask<CompilationInfo> createTask(final FileObject file) {
        return new CancellableTask<CompilationInfo>() {

            public void cancel() {
            }

            public void run(CompilationInfo compilationInfo) throws Exception {
                final HintsModel model = new HintsModel(compilationInfo);
                new JavaFXTreePathScanner() {

                    @Override
                    public Object visitCatch(CatchTree node, Object p) {
                        if (node.getBlock().getStatements().size() != 0 || node.getBlock().getValue() != null) {
                            model.addHint(null, node);
                        }
                        return super.visitCatch(node, p);
                    }
                }.scan(compilationInfo.getCompilationUnit(), null);
                
                if (model.getHints() != null) {
                    Collection<ErrorDescription> errors = new HashSet<ErrorDescription>();
                    for (Hint hint : model.getHints()) {
                        errors.add(getErrorDescription(file, hint, compilationInfo)); //NOI18N
                    }
                    HintsController.setErrors(FXSourceUtils.getDocument(file), "Try-Catch", errors); //NOI18N
                }
            }
        };

    }

     private ErrorDescription getErrorDescription(FileObject file, Hint hint, CompilationInfo compilationInfo) {
        StringBuilder sb = new StringBuilder(" "); //NOI18N
//        Iterator<Type> iterator = hint.getExceptions().iterator();
//        while (iterator.hasNext()) {
//            sb.append(iterator.next().toString());
//            if (iterator.hasNext()) {
//                sb.append(", "); //NOI18N
//            }
//        }
        Fix fix = new UncaughtExceptionsFix(FXSourceUtils.getDocument(file), hint, compilationInfo);
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        int start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), hint.getTree());
        int end = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), hint.getTree());
        ErrorDescription ed = ErrorDescriptionFactory.createErrorDescription(Severity.HINT, "DZIALA", //NOI18N
                Collections.singletonList(fix), file, start, end);

        return ed;
    }

   

}
