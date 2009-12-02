/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.tools.mjavac.util.JCDiagnostic;
import java.util.Collection;
import java.util.HashSet;
import java.util.StringTokenizer;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.lang.model.element.Element;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.tools.Diagnostic;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.support.EditorAwareJavaFXSourceTaskFactory;
import org.netbeans.modules.javafx.editor.JavaFXDocument;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.netbeans.spi.editor.hints.HintsController;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;

/**
 *
 * @author karol
 */
public final class CreateNewElementTaskFactory extends EditorAwareJavaFXSourceTaskFactory {

    private final AtomicBoolean cancel = new AtomicBoolean();
    private static final String ERROR_CODE1 = "compiler.err.cant.resolve.location"; //NOI18N
   // private static final String ERROR_CODE2 = "compiler.err.abstract.cant.be.instantiated"; //NOI18N
    private static final String HINT_IDENT = "createmethod"; //NOI18N
    private static final String FUNCTION = "function"; //NOI18N
    private static final String CLASS = "class"; //NOI18N
    private static final String VAR = "var"; //NOI18N

    public CreateNewElementTaskFactory() {
        super(JavaFXSource.Phase.ANALYZED, JavaFXSource.Priority.LOW);
    }

    @Override
    public CancellableTask<CompilationInfo> createTask(final FileObject file) {

        return new CancellableTask<CompilationInfo>() {

            public void cancel() {
                cancel.set(true);
            }

            public void run(final CompilationInfo compilationInfo) throws Exception {
                cancel.set(false);
                if (!(compilationInfo.getDocument() instanceof JavaFXDocument)) {
                    return;
                }
                //JavaFXDocument document = (JavaFXDocument) compilationInfo.getDocument();
                final Collection<ErrorDescription> errorDescriptions = new HashSet<ErrorDescription>();
                for (final Diagnostic diagnostic : compilationInfo.getDiagnostics()) {
                    if (!isValidError(diagnostic, compilationInfo) || cancel.get()) {
                        continue;
                    }
                    //int position = findPosition(compilationInfo, (int) diagnostic.getStartPosition(), (int) (diagnostic.getEndPosition() - diagnostic.getStartPosition()));
                    JavaFXTreePath path = compilationInfo.getTreeUtilities().pathFor(diagnostic.getPosition());

                    Element element = compilationInfo.getTrees().getElement(path);

                    if (diagnostic instanceof JCDiagnostic) {
                        JCDiagnostic d = (JCDiagnostic) diagnostic;
                        Object[] args = d.getArgs();
                        if (args[0].toString().equals(FUNCTION)) {
                            
                        } if (args[0].toString().equals(CLASS)) {

                        } if (args[0].toString().equals(VAR)) {

                        }
                    }
                    getErrorDescription(compilationInfo.getDocument(), file, compilationInfo, diagnostic);
                    errorDescriptions.add(getErrorDescription(compilationInfo.getDocument(), file, compilationInfo, diagnostic));
                }
                HintsController.setErrors(compilationInfo.getDocument(), HINT_IDENT, errorDescriptions);
            }
        };
    }

    private boolean isValidError(Diagnostic diagnostic, CompilationInfo compilationInfo) {
        if (diagnostic.getCode().equals(ERROR_CODE1)) {
            for (Diagnostic d : compilationInfo.getDiagnostics()) {
                if (d != diagnostic && d.getLineNumber() == diagnostic.getLineNumber()) {
                    return false;
                }
            }
            return true;
        }
        
        return false;
    }

    private int findPosition(CompilationInfo compilationInfo, int start, int lenght) {
        Document document = compilationInfo.getDocument();
        try {
            String text = document.getText(start, lenght);
            StringTokenizer st = new StringTokenizer(text);
            boolean isExtends = false;
            boolean isClass = false;

            while (st.hasMoreTokens()) {
                String token = st.nextToken();
                if (token.equals("extends")) { //NOI18N
                    isExtends = true;
                }
                if (token.equals("class")) { //NOI18N
                    isClass = true;
                }
            }
            if (isClass && !isExtends) {
                return -1;
            }
            int index = text.indexOf("{"); //NOI18N
            if (index > 0) {
                return start + index + 1;
            } else if (text.contains("(") || text.contains(")")) { //NOI18N
                return -1;
            } else {
                text = document.getText(start, document.getLength() - start);
                index = text.indexOf("{"); //NOI18N
                if (index < 0) {
                    return -1;
                }
                return start + index + 1;
            }
        } catch (BadLocationException ex) {
            ex.printStackTrace();
        }

        return -1;
    }

    private ErrorDescription getErrorDescription(final Document document,
            final FileObject file,
            final CompilationInfo compilationInfo,
            final Diagnostic diagnostic) {

            //        Fix fix = new Fix() {
            //
            //            public String getText() {
            //                return NbBundle.getMessage(OverrideAllTaskFactory.class, "TITLE_IMPLEMENT_ABSTRACT"); //NOI18N
            //            }
            //
            //            public ChangeInfo implement() throws Exception {
            //
            //                return null;
            //            }
            //        }
          try {
            document.getText((int) diagnostic.getPosition(), (int) (diagnostic.getEndPosition() - diagnostic.getStartPosition()));

        } catch (BadLocationException ex) {
            Exceptions.printStackTrace(ex);
        }

        return null;
    }
}