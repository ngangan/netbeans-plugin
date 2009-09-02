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

import com.sun.javafx.api.tree.SourcePositions;
import com.sun.tools.javac.code.Type;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import javax.swing.text.Document;

import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.support.EditorAwareJavaSourceTaskFactory;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.modules.javafx.editor.JavaFXDocument;
import org.netbeans.modules.javafx.editor.hints.UncaughtExceptionsModel.Hint;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.netbeans.spi.editor.hints.ErrorDescriptionFactory;
import org.netbeans.spi.editor.hints.Fix;
import org.netbeans.spi.editor.hints.HintsController;
import org.netbeans.spi.editor.hints.Severity;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;
import org.openide.util.NbBundle;

/**
 *
 * @author karol harezlak
 */
public class UncaughtExceptionsTaskFactory extends EditorAwareJavaSourceTaskFactory {

    public UncaughtExceptionsTaskFactory() {
        super(JavaFXSource.Phase.ANALYZED, JavaFXSource.Priority.LOW);
    }

    @Override
    protected CancellableTask<CompilationInfo> createTask(final FileObject file) {
        return new CancellableTask<CompilationInfo>() {

            @Override
            public void cancel() {
            }

            @Override
            public void run(CompilationInfo compilationInfo) throws Exception {
                if (file == null) {
                    throw new IllegalArgumentException("There is no associated fileobject for document."); // NOI18N
                }
                ClassIndex classIndex = ClasspathInfo.create(file).getClassIndex();
                UncaughtExceptionsVisitor tcw = new UncaughtExceptionsVisitor(compilationInfo, classIndex);
                UncaughtExceptionsModel model = new UncaughtExceptionsModel();
                tcw.scan(compilationInfo.getCompilationUnit(), model);
                new UncaughtExceptionsVisitorResolver().scan(compilationInfo.getCompilationUnit(), model);
                if (model.getThrowHints() != null) {
                    Collection<ErrorDescription> errors = new HashSet<ErrorDescription>();
                    for (Hint hint : model.getThrowHints()) {
                        errors.add(getErrorDescription(file, hint, compilationInfo));
                    }
                    HintsController.setErrors(getDocument(file), "Try-Catch", errors); //NOI18N
                }
            }
        };
    }

    private JavaFXDocument getDocument(FileObject file) {
        if (file == null || !file.isValid()) {
            return null;
        }

        DataObject dataObject = null;
        try {
            dataObject = DataObject.find(file);
        } catch (DataObjectNotFoundException e) {
            e.printStackTrace();
        }
        EditorCookie ec = dataObject != null ? dataObject.getLookup().lookup(EditorCookie.class) : null;
        if (ec == null) {
            return null;
        }
        Document document = ec.getDocument();
        if (document instanceof JavaFXDocument) {
            return (JavaFXDocument) document;
        }

        return null;
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
        Fix fix = new UncaughtExceptionsFix(getDocument(file), hint, compilationInfo);
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        int start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), hint.getTree());
        int end = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), hint.getTree());
        ErrorDescription ed = ErrorDescriptionFactory.createErrorDescription(Severity.HINT, NbBundle.getMessage(UncaughtExceptionsTaskFactory.class, "TITLE_UNREPORTED_EXCEPTION") + sb, //NOI18N
                Collections.singletonList(fix), file, start, end);

        return ed;
    }
}
