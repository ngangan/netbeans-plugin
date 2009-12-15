/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.FunctionInvocationTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.tools.javafx.tree.JFXBlock;
import com.sun.tools.javafx.tree.JFXClassDeclaration;
import com.sun.tools.javafx.tree.JFXIdent;
import com.sun.tools.mjavac.code.Symbol.ClassSymbol;
import com.sun.tools.mjavac.code.Type;
import com.sun.tools.mjavac.code.Type.ClassType;
import com.sun.tools.mjavac.util.JCDiagnostic;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.lang.model.element.Element;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import javax.tools.Diagnostic;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.ClasspathInfo.PathKind;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementUtilities;
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
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;
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

                    JCDiagnostic jCDiagnostic = (JCDiagnostic) diagnostic;
                    if (!(jCDiagnostic.getArgs()[5] instanceof ClassType)) {
                        continue;
                    }

                    errorDescriptions.add(getErrorDescription(document, compilationInfo, (JCDiagnostic) diagnostic));
                }
                HintsController.setErrors(compilationInfo.getDocument(), HINT_IDENT, errorDescriptions);
            }
        };
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

    private ErrorDescription getErrorDescription(JavaFXDocument document,
            final CompilationInfo compilationInfo,
            final JCDiagnostic diagnostic) {

        List<Fix> fixes = new ArrayList<Fix>();
        String fixType = diagnostic.getArgs()[0].toString();
        if (fixType.equals(VAR_VALUE)) {

            final boolean isInsideMainFunction[] = new boolean[1];
            JavaFXTreePathScanner visitor = new JavaFXTreePathScanner() {

                SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();

                @Override
                public Object visitIdentifier(IdentifierTree node, Object p) {
                    int treePosition = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), node);
                    if (treePosition == diagnostic.getPosition()) {
                        JavaFXTreePath parentPath = getCurrentPath().getParentPath();
                        if (parentPath != null && parentPath.getParentPath() != null) {
                            String functionName = parentPath.getParentPath().getLeaf().toString();
                            if (functionName.contains("_$UNUSED$_$ARGS$_")) { //NOI18N
                                isInsideMainFunction[0] = true;
                            }
                        }

                        return null;
                    }

                    return super.visitIdentifier(node, p);
                }
            };
            visitor.scan(compilationInfo.getCompilationUnit(), null);


            if (!isInsideMainFunction[0]) {
                fixes.add(new CreateElementFix(fixType, diagnostic, document, compilationInfo, false));
            }
            fixes.add(new CreateElementFix(fixType, diagnostic, document, compilationInfo, true));
        } else {
            fixes.add(new CreateElementFix(fixType, diagnostic, document, compilationInfo, false));
        }
        ErrorDescription errorDescription = ErrorDescriptionFactory.createErrorDescription(Severity.HINT, "Create missing elements", fixes, compilationInfo.getFileObject(), (int) diagnostic.getStartPosition(), (int) diagnostic.getStartPosition());

        return errorDescription;
    }

    private class CreateElementFix implements Fix {

        private String fixType;
        private JCDiagnostic diagnostic;
        private JavaFXDocument document;
        private CompilationInfo compilationInfo;
        private boolean localPosition;

        public CreateElementFix(String fixType, JCDiagnostic diagnostic, JavaFXDocument document, CompilationInfo compilationInfo, boolean localPosition) {
            this.fixType = fixType;
            this.diagnostic = diagnostic;
            this.document = document;
            this.compilationInfo = compilationInfo;
            this.localPosition = localPosition;
        }

        public String getText() {
            if (localPosition) {
                return "Local" + NbBundle.getMessage(CreateNewElementTaskFactory.class, "TITLE_CREATE_ELEMENT_" + fixType.toUpperCase(), diagnostic.getArgs()[1].toString(), diagnostic.getArgs()[5].toString()); //NOI18N
            }
            return NbBundle.getMessage(CreateNewElementTaskFactory.class, "TITLE_CREATE_ELEMENT_" + fixType.toUpperCase(), diagnostic.getArgs()[1].toString(), diagnostic.getArgs()[5].toString()); //NOI18N
        }

        public ChangeInfo implement() throws Exception {
            StringBuffer code = new StringBuffer();
            //TODO Must be redone with formating API
            JTextComponent target = HintsUtils.getEditorComponent(document);
            if (target == null) {
                return null;
            }
            Object[] args = diagnostic.getArgs();
            ClassType classType = (ClassType) args[5];
            int position = -1;
            if (fixType.equals(FUNCTION_VALUE)) {
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
                Iterator iterator = typesMap.keySet().iterator();
                while (iterator.hasNext()) {
                    String argName = (String) iterator.next();
                    Type typeFQN = typesMap.get(argName);
                    code.append(argName).append(" : ").append(HintsUtils.getClassSimpleName(typeFQN.toString()));
                    if (iterator.hasNext()) {
                        code.append(" , "); //NOI!8N
                    }
                    if (typeFQN.toString().contains("java.lang")) { //NOI18N
                        continue;
                    }
                    Imports.addImport(target, typeFQN.toString());
                }
                Imports.addImport(target, HintsUtils.EXCEPTION_UOE);
                position = getPositonAtEnd(classType.tsym, compilationInfo, document);
                String space = HintsUtils.calculateSpace(position, document);
                code.append("\n").append(space).append(FUNCTION_VALUE).append(" ").append(diagnostic.getArgs()[1]).append("(");//NOI18N
                code.append("){\n"); //NOI18N)
                code.append(space).append(HintsUtils.TAB).append("throw new UnsupportedOperationException('Not implemented yet');\n"); //NOI18N
                code.append(space).append("}\n"); //NOI18N
                //code = new StringBuffer(JFXReformatTask.reformat(code.toString(), CodeStyle.getDefault(document)));

            } else if (fixType.equals(CLASS_VALUE)) {
                code.append(CLASS_VALUE).append(" ").append(diagnostic.getArgs()[5]).append(" {"); //NOI18N
                code.append("}"); //NOI18N
                ClassPath cp = compilationInfo.getClasspathInfo().getClassPath(PathKind.SOURCE);
                FileObject root = cp.findOwnerRoot(compilationInfo.getFileObject());

                if (root == null) { //File not part of any project
                    return null;
                }

                TypeElement outer = ElementUtilities.enclosingTypeElement(classType.asElement());
                ClassSymbol classSymbol = (ClassSymbol) classType.asElement();

                PackageElement packageElement = classSymbol.packge();

                FileObject pack = FileUtil.createFolder(compilationInfo.getFileObject(), packageElement.toString()); // NOI18N
//                FileObject classTemplate/*???*/ = FileUtil.getConfigFile(template(kind));
//                DataObject classTemplateDO = DataObject.find(classTemplate);
//                DataObject od = classTemplateDO.createFromTemplate(DataFolder.findFolder(pack), simpleName);
//                FileObject target = od.getPrimaryFile();

//                JavaSource.forFileObject(target).runModificationTask(new Task<WorkingCopy>() {
//
//                    public void run(WorkingCopy parameter) throws Exception {
//                        parameter.toPhase(Phase.RESOLVED);
//
//                        ClassTree source = (ClassTree) parameter.getCompilationUnit().getTypeDecls().get(0);
//                        ClassTree nue = createConstructor(parameter, TreePath.getPath(parameter.getCompilationUnit(), source));
//
//                        parameter.rewrite(source, nue);
//                    }
//                }).commit();

                // return new ChangeInfo(target, null, null);
            } else if (fixType.equals(VAR_VALUE)) {
                code.append(HintsUtils.TAB).append("\n");
                code.append("var ").append(diagnostic.getArgs()[1]).append(";\n"); //NOI18N
                if (localPosition) {
                    position = getLocalPostion(compilationInfo, diagnostic);
                } else {
                    position = getPositonAtStart(classType.tsym, compilationInfo, diagnostic);
                }
            }
            if (position < 0) {
                return null;
            }
            document.insertString(position, code.toString(), null);

            return null;
        }
    }

    private static int getLocalPostion(CompilationInfo compilationInfo, JCDiagnostic diagnostic) {
        //Object[] args = diagnostic.getArgs();
        //ClassType classType = (ClassType) args[5];

        //Tree classElement = compilationInfo.getTrees().getTree(classType.tsym);
        JavaFXTreePath path = compilationInfo.getTreeUtilities().pathFor(diagnostic.getPosition());
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        Tree tree = path.getParentPath().getLeaf();
        path.getLeaf();
        compilationInfo.getTrees().getElement(path);
        int position = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), tree);
        if (tree instanceof JFXBlock) {
            position++;
        }
        if (diagnostic.getPosition() < position) {
            return (int) diagnostic.getPosition();
        }
        return position;
    }

    private static int getPositonAtEnd(final Element element, final CompilationInfo compilationInfo, JavaFXDocument document) {

        Tree enclosingTree = compilationInfo.getTrees().getTree(element);
        final SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        int start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), enclosingTree);
        int end = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), enclosingTree);

        if (HintsUtils.isAnnon(element)) {
            start = findPosition(compilationInfo, start, end - start, element);
        }
        if (document.isPosGuarded(start)) {
            start = -1;
        }

        return start;
    }
//
//    private static int getEnclosingPositionInside(final CompilationInfo compilationInfo, final Diagnostic diagnostic) {
//        final int[] lp = {-1};
//        JavaFXTreePathScanner visitor = new JavaFXTreePathScanner() {
//
//            SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
//
//            @Override
//            public Object visitIdentifier(IdentifierTree node, Object p) {
//                int treePosition = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), node);
//                if (treePosition == diagnostic.getPosition()) {
//                    lp[0] = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), getCurrentPath().getParentPath().getLeaf());
//
//                    return null;
//                }
//
//                return super.visitIdentifier(node, p);
//            }
//        };
//        visitor.scan(compilationInfo.getCompilationUnit(), null);
//
//        return lp[0] -1 ;
//    }
//

    private static int getEnclosingPositionBefore(final CompilationInfo compilationInfo, Element element, int errorPosition) {
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        for (Element e : element.getEnclosedElements()) {
            Tree tree = compilationInfo.getTrees().getTree(e);
            if (tree != null) {
                int startTree = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), tree);
                int endTree = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), tree);
                if (startTree <= errorPosition && endTree >= errorPosition) {
                    return startTree - 1;
                }
            }
        }

        return -1;
    }
//

    private static int getPositonAtStart(Element element, CompilationInfo compilationInfo, Diagnostic diagnostic) {
        Tree enclosingTree = compilationInfo.getTrees().getTree(element);

        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        int start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), enclosingTree);
        int end = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), enclosingTree);
        int position = findPosition(compilationInfo, start, end - start, element);
        if (position < 0 && enclosingTree instanceof JFXClassDeclaration) {
            position = getEnclosingPositionBefore(compilationInfo, element, (int) diagnostic.getPosition());
        }

        return position;
    }
//

    private static int findPosition(CompilationInfo compilationInfo, int start, int lenght, Element element) {
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
            boolean isAnon = HintsUtils.isAnnon(element);
            if (!isAnon && !isClass && !isExtends) {
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
}
