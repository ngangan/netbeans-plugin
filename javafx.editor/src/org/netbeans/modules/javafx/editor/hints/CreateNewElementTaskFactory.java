/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.ErroneousTree;
import com.sun.javafx.api.tree.FunctionInvocationTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.LiteralTree;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.VariableInvalidateTree;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javafx.code.JavafxClassSymbol;
import com.sun.tools.javafx.tree.JFXClassDeclaration;
import com.sun.tools.javafx.tree.JFXFunctionDefinition;
import com.sun.tools.javafx.tree.JFXFunctionInvocation;
import com.sun.tools.mjavac.util.JCDiagnostic;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import javax.swing.SwingUtilities;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import javax.tools.Diagnostic;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.Imports;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.support.EditorAwareJavaFXSourceTaskFactory;
import org.netbeans.modules.javafx.editor.JavaFXDocument;
import org.netbeans.modules.javafx.editor.imports.ui.FixItem;
import org.netbeans.spi.editor.hints.ChangeInfo;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.netbeans.spi.editor.hints.ErrorDescriptionFactory;
import org.netbeans.spi.editor.hints.Fix;
import org.netbeans.spi.editor.hints.HintsController;
import org.netbeans.spi.editor.hints.Severity;
import org.openide.filesystems.FileObject;

/**
 *
 * @author karol
 */
public final class CreateNewElementTaskFactory extends EditorAwareJavaFXSourceTaskFactory {

    private final AtomicBoolean cancel = new AtomicBoolean();
    private static final String ERROR_CODE = "compiler.err.cant.resolve.location"; //NOI18N
    private static final String HINT_IDENT = "createmethod"; //NOI18N
    private static final String FUNCTION_VALUE = "function"; //NOI18N
    private static final String CLASS_VALUE = "class"; //NOI18N
    private static final String VAR_VALUE = "variable"; //NOI18N
    private static Logger log = Logger.getLogger(CreateNewElementTaskFactory.class.getName());

    public CreateNewElementTaskFactory() {
        super(JavaFXSource.Phase.ANALYZED, JavaFXSource.Priority.LOW);
    }

    private enum Kind {

        CLASS, FUNCTION, VARIABLE
    };

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
                JavaFXDocument document = (JavaFXDocument) compilationInfo.getDocument();
                final Collection<ErrorDescription> errorDescriptions = new HashSet<ErrorDescription>();

                for (final Diagnostic diagnostic : compilationInfo.getDiagnostics()) {
                    if (isValidError(diagnostic, compilationInfo)
                            && !cancel.get()
                            && (diagnostic instanceof JCDiagnostic)) {

                        errorDescriptions.add(getErrorDescription(document, compilationInfo, (JCDiagnostic) diagnostic));
                    }
                }
                HintsController.setErrors(compilationInfo.getDocument(), HINT_IDENT, errorDescriptions);
            }
        };
    }

    private boolean isValidError(Diagnostic diagnostic, CompilationInfo compilationInfo) {
        if (diagnostic.getCode().equals(ERROR_CODE)) {
//            for (Diagnostic d : compilationInfo.getDiagnostics()) {
//                if (d.getLineNumber() == diagnostic.getLineNumber()) {
//                    return false;
//                }
//            }
            return true;
        }

        return false;
    }

    private ErrorDescription getErrorDescription(JavaFXDocument document,
            final CompilationInfo compilationInfo,
            final JCDiagnostic diagnostic) {


        String message = diagnostic.getMessage(Locale.ENGLISH);
        Kind kind = getKind(message);

        if (kind == null) {
            return null;
        }
        Fix fix = new VariableFix(kind, document, diagnostic, compilationInfo);
        ErrorDescription errorDescription = ErrorDescriptionFactory.createErrorDescription(Severity.HINT, "Create missing elements", Collections.singletonList(fix), compilationInfo.getFileObject(), (int) diagnostic.getStartPosition(), (int) diagnostic.getStartPosition());

        return errorDescription;
    }

    private Kind getKind(String message) {
        Kind kind = null;
        if (message.contains("symbol  : class")) {
            kind = Kind.CLASS;
        } else if (message.contains("symbol  : variable")) {
            kind = Kind.VARIABLE;
        } else if (message.contains("symbol  : function")) {
            kind = Kind.FUNCTION;
        }
        return kind;
    }

    private class VariableFix implements Fix {

        private final Kind kind;
        private final Document document;
        private final JCDiagnostic diagnostic;
        private final CompilationInfo compilationInfo;

        public VariableFix(Kind kind, Document document, JCDiagnostic diagnostic, CompilationInfo compilationInfo) {
            this.kind = kind;
            this.document = document;
            this.diagnostic = diagnostic;
            this.compilationInfo = compilationInfo;
        }

        public String getText() {
            return "Create " + kind;
        }

        public ChangeInfo implement() throws Exception {
            final GeneratedCode[] generatedCode = new GeneratedCode[1];
            if (kind == Kind.FUNCTION) {
                generatedCode[0] = createFunction();
            } else if (kind == Kind.VARIABLE) {
                // generatedCode[0] = createVariable(diagnostic);
            } else {
                // generatedCode = createInnerClass();
            }
            if (generatedCode[0] == null) {
               return null;
            }
            SwingUtilities.invokeLater(new Runnable() {

                public void run() {
                    try {
                        document.insertString(generatedCode[0].getPositon(), generatedCode[0].getCode(), null);
                        if (kind != Kind.FUNCTION) {
                            return;
                        }
                        JTextComponent target = HintsUtils.getEditorComponent(document);
                        if (target == null) {
                            log.severe("No GUI component for editor document " + document); //NOI18N
                            return;
                        }
                        Imports.addImport(target, HintsUtils.EXCEPTION_UOE);
                    } catch (Exception ex) {
                        ex.printStackTrace();
                    }
                }
            });

            return null;
        }

        private GeneratedCode createFunction() {
            StringBuffer function = new StringBuffer();
            Object name = diagnostic.getArgs()[1];

            final int position[] = new int[1];
            new JavaFXTreePathScanner<Void, Void>() {

                private ClassDeclarationTree currentClass;

                @Override
                public Void visitClassDeclaration(ClassDeclarationTree node, Void p) {
                    this.currentClass = node;
                    
                    return super.visitClassDeclaration(node, p);
                }

                @Override
                public Void visitMethodInvocation(FunctionInvocationTree node, Void p) {
                    SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
                    int startPosition = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), node);
                    ((JCDiagnostic)diagnostic.getArgs()[4]).getEndPosition();
                    if (startPosition == diagnostic.getStartPosition()) {
                        position[0] = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), currentClass.getClassMembers().get(currentClass.getClassMembers().size() -1 ));

                        return null;
                    }

                    return super.visitMethodInvocation(node, p);
                }
            }.scan(compilationInfo.getCompilationUnit(), null);

            String space = HintsUtils.calculateSpace(position[0], document);
            if (space.length() > 0) {
                space = space.substring(0, space.length() - 1);
            }
            function.append("\n\n"); //NOI18N
            function.append(space).append("function ").append(name).append("() {\n");
            function.append(space).append(HintsUtils.TAB).append("throw new UnsupportedOperationException('Not implemented yet');\n"); //NOI18N
            function.append(space).append("}\n"); //NOI18N

            return new GeneratedCode(position[0], function.toString());
        }

        private GeneratedCode createVariable(final JCDiagnostic diagnostic) {
            StringBuffer function = new StringBuffer();
            Object name = diagnostic.getArgs()[1];

            final int position[] = new int[1];
            new JavaFXTreePathScanner<Void, Void>() {

                @Override
                public Void visitIdentifier(IdentifierTree node, Void p) {
                    SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
                    int startPosition = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), node);
                    if (startPosition == diagnostic.getStartPosition()) {
                        Tree parentTree = getCurrentPath().getParentPath().getLeaf();
                        position[0] = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), parentTree) + 1;
                        if (position[0] < startPosition) {
                            position[0] = startPosition;
                        }
                        return null;
                    }
                    return super.visitIdentifier(node, p);
                }
            }.scan(compilationInfo.getCompilationUnit(), null);

            String space = HintsUtils.calculateSpace(position[0], document);
            function.append("\n");
            function.append(space).append(HintsUtils.TAB).append("var ").append(name).append(";");


            return new GeneratedCode(position[0], function.toString());
        }

        private String createInnerClass() {
            StringBuffer function = new StringBuffer();
            Object name = diagnostic.getArgs()[1];
            function.append("class ").append(name).append("{};\n");

            return function.toString();
        }
    }

    private class GeneratedCode {

        private int positon;
        private String code;

        GeneratedCode(int position, String code) {
            this.code = code;
            this.positon = position;
        }

        public String getCode() {
            return code;
        }

        public int getPositon() {
            return positon;
        }
    }
}
