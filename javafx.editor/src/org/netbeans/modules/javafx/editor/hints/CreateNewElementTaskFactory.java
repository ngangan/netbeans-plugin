/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.FunctionInvocationTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.tools.javafx.tree.JFXVar;
import com.sun.tools.mjavac.util.JCDiagnostic;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Logger;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import javax.tools.Diagnostic;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.Imports;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.support.EditorAwareJavaFXSourceTaskFactory;
import org.netbeans.modules.javafx.editor.JavaFXDocument;
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
 * @author karol
 */
public final class CreateNewElementTaskFactory extends EditorAwareJavaFXSourceTaskFactory {

    private final AtomicBoolean cancel = new AtomicBoolean();
    private static final String ERROR_CODE = "compiler.err.cant.resolve.location"; //NOI18N
    private static final String HINT_IDENT = "createmethod"; //NOI18N
    private static Logger log = Logger.getLogger(CreateNewElementTaskFactory.class.getName());

    public CreateNewElementTaskFactory() {
        super(JavaFXSource.Phase.ANALYZED, JavaFXSource.Priority.LOW);
    }

    private enum Kind {
        CLASS,
        FUNCTION,
        LOCAL_VARIABLE,
        VARIABLE,
        LOCAL_CLASS
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

                        String message = diagnostic.getMessage(Locale.ENGLISH);
                        Kind kinds[] = getKinds(message);
                        for (Kind kind : kinds) {
                            errorDescriptions.add(getErrorDescription(document, compilationInfo, (JCDiagnostic) diagnostic, kind));
                        }
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
            final JCDiagnostic diagnostic,
            final Kind kind) {

        if (kind == null) {
            return null;
        }
        Fix fix = new ElementFix(kind, document, diagnostic, compilationInfo);
        ErrorDescription errorDescription = ErrorDescriptionFactory.createErrorDescription(Severity.HINT, kind.toString(), Collections.singletonList(fix), compilationInfo.getFileObject(), (int) diagnostic.getStartPosition(), (int) diagnostic.getStartPosition());

        return errorDescription;
    }

    private Kind[] getKinds(String message) {
        if (message.contains("symbol  : class")) { //NOI18N
            return new Kind[]{Kind.LOCAL_CLASS};
        } else if (message.contains("symbol  : variable")) { //NOI18N
            return new Kind[]{Kind.VARIABLE, Kind.LOCAL_VARIABLE};
        } else if (message.contains("symbol  : function")) { //NOI18N
            return new Kind[]{Kind.FUNCTION}; //NOI18N
        }

        throw new IllegalStateException();
    }

    private class ElementFix implements Fix {

        private final Kind kind;
        private final Document document;
        private final JCDiagnostic diagnostic;
        private final CompilationInfo compilationInfo;

        public ElementFix(Kind kind, Document document, JCDiagnostic diagnostic, CompilationInfo compilationInfo) {
            this.kind = kind;
            this.document = document;
            this.diagnostic = diagnostic;
            this.compilationInfo = compilationInfo;
        }

        public String getText() {
            return "Create " + kind; //TODO
        }

        public ChangeInfo implement() throws Exception {
            final GeneratedCode[] generatedCode = new GeneratedCode[1];
            if (kind == Kind.FUNCTION) {
                generatedCode[0] = createFunction();
            } else if (kind == Kind.LOCAL_VARIABLE || kind == Kind.VARIABLE) {
                generatedCode[0] = createVariable(diagnostic, kind);
            } else if (kind == Kind.LOCAL_CLASS) {
                generatedCode[0] = createLocalClass();
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
            StringBuffer code = new StringBuffer();
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
                    if (startPosition == diagnostic.getStartPosition()) {
                        position[0] = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), currentClass.getClassMembers().get(currentClass.getClassMembers().size() - 1));

                        return null;
                    }

                    return super.visitMethodInvocation(node, p);
                }
            }.scan(compilationInfo.getCompilationUnit(), null);

            String space = HintsUtils.calculateSpace(position[0], document);
            if (space.length() > 0) {
                space = space.substring(0, space.length() - 1);
            }
            code.append("\n\n"); //NOI18N
            code.append(space).append("function ").append(name).append("() {\n"); //NOI18N
            code.append(space).append(HintsUtils.TAB).append("throw new UnsupportedOperationException('Not implemented yet');\n"); //NOI18N
            code.append(space).append("}\n"); //NOI18N

            return new GeneratedCode(position[0], code.toString());
        }

        private GeneratedCode createVariable(final JCDiagnostic diagnostic, Kind kind) {
            Object varName = diagnostic.getArgs()[1];
            GeneratedCode generatedCode = null;
            if (kind == Kind.LOCAL_VARIABLE) {
                generatedCode = generateLocalVar(varName.toString());
            } else if (kind == Kind.VARIABLE) {
                generatedCode = generateGlobalVar(varName.toString());
            }
            
            return generatedCode;
        }

        private GeneratedCode generateLocalVar(String varName) {
            final int position[] = new int[1];
            final SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
            new JavaFXTreePathScanner<Void, Void>() {

                @Override
                public Void visitIdentifier(IdentifierTree node, Void p) {
                    int startPosition = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), node);
                    if (startPosition == diagnostic.getStartPosition()) {
                        position[0] = startPosition;

                        return null;
                    }

                    return super.visitIdentifier(node, p);
                }
            }.scan(compilationInfo.getCompilationUnit(), null);
            String space = HintsUtils.calculateSpace(position[0], document);
            StringBuffer code = new StringBuffer().append("var ").append(varName).append(";\n").append(space); //NOI18N

            return new GeneratedCode(position[0], code.toString());
        }

        private GeneratedCode generateGlobalVar(final String varName) {
            final int position[] = new int[1];
            final SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
            final StringBuffer code = new StringBuffer();
            new JavaFXTreePathScanner<Void, Void>() {

                private ClassDeclarationTree currentClass;

                @Override
                public Void visitClassDeclaration(ClassDeclarationTree node, Void p) {
                    this.currentClass = node;

                    return super.visitClassDeclaration(node, p);
                }

                @Override
                public Void visitIdentifier(IdentifierTree node, Void p) {
                    int startPosition = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), node);
                    if (startPosition == diagnostic.getStartPosition()) {
                        int start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), currentClass);
                        Tree firstVar = null;
                        Iterator<Tree> iterator = currentClass.getClassMembers().iterator();
                        if (iterator.hasNext()) {
                            Tree tree = iterator.next();
                            if (tree instanceof JFXVar) {
                                firstVar = tree;
                            }
                        }
                        if (firstVar != null) {
                            position[0] = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), firstVar);
                            String space = HintsUtils.calculateSpace(position[0], document);
                            code.append("var ").append(varName).append(";\n").append(space); //NOI18N

                            return null;
                        }
                        try {
                            //TODO Line below returns 0 in same cases which means start of the node and end of the node is the same which is not true.
                            //int length = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), currentClass) - start;
                            //TODO Workaround for this problem
                            String sourceCode = document.getText(start, document.getLength() - start);
                            int index = sourceCode.indexOf("{"); //NOI18N
                            position[0] = start + index + 1;
                            String space = HintsUtils.calculateSpace(position[0], document);
                            code.append("\n").append(space).append(HintsUtils.TAB).append("var ").append(varName).append(";"); //NOI18N
                        } catch (BadLocationException ex) {
                            log.severe(ex.getMessage());
                        }

                        return null;
                    }

                    return super.visitIdentifier(node, p);
                }
            }.scan(compilationInfo.getCompilationUnit(), null);
            
            return diagnostic.getStartPosition() < position[0] ? generateLocalVar(varName) : new GeneratedCode(position[0], code.toString());
        }

        private GeneratedCode createLocalClass() {
            StringBuffer code = new StringBuffer();
            Object name = diagnostic.getArgs()[1];
           

            final int position[] = new int[1];
            final SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
            new JavaFXTreePathScanner<Void, Void>() {

                @Override
                public Void visitClassDeclaration(ClassDeclarationTree node, Void p) {
                    position[0] = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), node);
                    return null;
                }

            }.scan(compilationInfo.getCompilationUnit(), null);
            code.append("\n"); //NOI18N
            code.append("\nclass ").append(name).append(" {\n"); //NOI18N
            code.append(HintsUtils.TAB).append("//TODO Not implemented yet.\n"); //NOI18N
            code.append("}\n"); //NOI18N

            return new GeneratedCode(position[0], code.toString());
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

