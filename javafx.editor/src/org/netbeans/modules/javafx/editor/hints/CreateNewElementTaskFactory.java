/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.FunctionInvocationTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.InstantiateTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.JavaFXTreeVisitor;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javafx.tree.JFXIdent;
import com.sun.tools.javafx.tree.JavafxVisitor;
import com.sun.tools.mjavac.code.Symbol.ClassSymbol;
import com.sun.tools.mjavac.code.Type;
import com.sun.tools.mjavac.code.Type.ClassType;
import com.sun.tools.mjavac.util.JCDiagnostic;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.lang.model.element.Element;
import javax.swing.text.BadLocationException;
import javax.swing.text.JTextComponent;
import javax.tools.Diagnostic;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.Imports;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.support.EditorAwareJavaFXSourceTaskFactory;
import org.netbeans.modules.javafx.editor.JavaFXDocument;
import org.netbeans.modules.javafx.editor.format.CodeStyle;
import org.netbeans.modules.javafx.editor.format.JFXReformatTask;
import org.netbeans.spi.editor.hints.ChangeInfo;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.netbeans.spi.editor.hints.ErrorDescriptionFactory;
import org.netbeans.spi.editor.hints.Fix;
import org.netbeans.spi.editor.hints.HintsController;
import org.netbeans.spi.editor.hints.Severity;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;
import org.openide.util.NbBundle;

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
                JavaFXDocument document = (JavaFXDocument) compilationInfo.getDocument();
                final Collection<ErrorDescription> errorDescriptions = new HashSet<ErrorDescription>();

                for (final Diagnostic diagnostic : compilationInfo.getDiagnostics()) {
                    if (!isValidError(diagnostic, compilationInfo)
                            || cancel.get()
                            || !(diagnostic instanceof JCDiagnostic)) {
                        continue;
                    }
                    int position = -1;
                    String fixType = null;
                    Type type = null;
                    if (diagnostic instanceof JCDiagnostic) {
                        JCDiagnostic d = (JCDiagnostic) diagnostic;
                        Object[] args = d.getArgs();
                        if (!(args[5] instanceof ClassType)) {
                            return;
                        }
                        ClassType classType = (ClassType) args[5];
                        if (args[0].toString().equals(FUNCTION_VALUE)) {
                            fixType = FUNCTION_VALUE;
                            position = getPositonAtEnd(classType.tsym, compilationInfo, document);
                        }
                        if (args[0].toString().equals(CLASS_VALUE)) {
                            fixType = CLASS_VALUE;
                        }
                        if (args[0].toString().equals(VAR_VALUE)) {
                            fixType = VAR_VALUE;
                            position = getPositonAtStart(classType.tsym, compilationInfo, document);
                        }
                    }
                    if (position < 0) {
                        continue;
                    }
                    errorDescriptions.add(getErrorDescription(document, compilationInfo, (JCDiagnostic) diagnostic, fixType, position));
                }
                HintsController.setErrors(compilationInfo.getDocument(), HINT_IDENT, errorDescriptions);
            }
        };
    }

    private int getPositonAtEnd(Element element, CompilationInfo compilationInfo, JavaFXDocument document) {
        Tree enclosingTree = compilationInfo.getTrees().getTree(element);
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        int position = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), enclosingTree);
        if (document.isPosGuarded(position)) {
            position = -1;
        }

        return position;
    }

    private int getPositonAtStart(Element element, CompilationInfo compilationInfo, JavaFXDocument document) {
        Tree enclosingTree = compilationInfo.getTrees().getTree(element);
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        int position = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), enclosingTree);
        try {
            String code = document.getText(position, document.getLength() - position);
            int index = code.indexOf("{"); //NOI18N
            position = position + index;
        } catch (BadLocationException ex) {
            Exceptions.printStackTrace(ex);
        }
        if (document.isPosGuarded(position)) {
            position = -1;
        }

        return position + 1;
    }

    private boolean isValidError(Diagnostic diagnostic, CompilationInfo compilationInfo) {
        if (diagnostic.getCode().equals(ERROR_CODE)) {
            for (Diagnostic d : compilationInfo.getDiagnostics()) {
                if (d != diagnostic && d.getLineNumber() == diagnostic.getLineNumber()) {
                    return false;
                }
            }
            return true;
        }

        return false;
    }

    private ErrorDescription getErrorDescription(final JavaFXDocument document,
            final CompilationInfo compilationInfo,
            final JCDiagnostic diagnostic,
            final String fixType,
            final int position) {

        Fix fix = new Fix() {

            public String getText() {
                return NbBundle.getMessage(CreateNewElementTaskFactory.class, "TITLE_CREATE_ELEMENT_" + fixType.toUpperCase(), diagnostic.getArgs()[1].toString(), diagnostic.getArgs()[5].toString()); //NOI18N
            }

            public ChangeInfo implement() throws Exception {
                String space = HintsUtils.calculateSpace((int) diagnostic.getStartPosition(), document);
                StringBuffer code = new StringBuffer();
                int finalPosition = position;
                //TODO Must be redone with formating API
                JTextComponent target = HintsUtils.getEditorComponent(document);
                    if (target == null) {
                        return null;
                    }
                if (fixType.equals(FUNCTION_VALUE)) {
                    code.append(FUNCTION_VALUE).append(" ").append(diagnostic.getArgs()[1]).append("(");//NOI18N
                    final Map<String, Type> typesMap = new HashMap<String, Type>();
                    final SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();

                    JavaFXTreePathScanner vistor = new JavaFXTreePathScanner() {

                        @Override
                        public Object visitMethodInvocation(FunctionInvocationTree node, Object p) {
                            long position = sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), node);
                            if (position != diagnostic.getPosition()) {
                                return super.visitMethodInvocation(node, p);
                            }
                            for (ExpressionTree expressionTree : node.getArguments()) {
                                if (expressionTree instanceof JFXIdent) {
                                    JFXIdent argument = (JFXIdent) expressionTree;
                                    typesMap.put(argument.getName().toString(), argument.sym.asType());
                                }
                            }

                            return super.visitMethodInvocation(node, p);
                        }
                    };

                    vistor.scan(compilationInfo.getCompilationUnit(), null);
                    for (String argName : typesMap.keySet()) {
                        Type typeFQN = typesMap.get(argName);
                        code.append(argName).append(" : ").append(HintsUtils.getClassSimpleName(typeFQN.toString()));
                        if (typeFQN.toString().contains("java.lang")) { //NOI18N
                            continue;
                        }
                        Imports.addImport(target, typeFQN.toString());
                    }
                    code.append("){\n"); //NOI18N)
                    code.append("throw new UnsupportedOperationException('Not implemented yet');\n"); //NOI18N
                    code.append("}\n"); //NOI18N
                } else if (fixType.equals(CLASS_VALUE)) {
                    code.append(CLASS_VALUE).append(" ").append(diagnostic.getArgs()[5]).append(" {"); //NOI18N
                    code.append(space).append("}"); //NOI18N
                } else if (fixType.equals(VAR_VALUE)) {
                    code.append(HintsUtils.TAB).append("\n");
                    code.append("var ").append(diagnostic.getArgs()[1]).append(";\n"); //NOI18N
                }

                Imports.addImport(target, HintsUtils.EXCEPTION_UOE);
                document.insertString(finalPosition, JFXReformatTask.reformat(code.toString(), CodeStyle.getDefault(document)), null);

                return null;
            }

            class JavaFXTreePathScannerImpl extends JavaFXTreePathScanner {

                public JavaFXTreePathScannerImpl() {
                }
            }
        };
        ErrorDescription er = ErrorDescriptionFactory.createErrorDescription(Severity.HINT, "", Collections.singletonList(fix), compilationInfo.getFileObject(), (int) diagnostic.getStartPosition(), (int) diagnostic.getEndPosition());//NOI18N

        return er;
    }
}
