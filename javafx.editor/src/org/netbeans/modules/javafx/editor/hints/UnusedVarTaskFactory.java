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
import com.sun.javafx.api.tree.ForExpressionTree;
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javafx.tree.JFXForExpressionInClause;
import com.sun.tools.javafx.tree.JFXVar;
import java.util.Collection;
import java.util.HashSet;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.support.EditorAwareJavaSourceTaskFactory;
import org.netbeans.api.javafx.source.JavaFXSource;
import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.swing.SwingUtilities;
import javax.swing.text.AttributeSet;
import javax.swing.text.Document;
import javax.swing.text.StyleConstants;
import org.netbeans.api.editor.settings.AttributesUtilities;
import org.netbeans.api.javafx.editor.Cancellable;
import org.netbeans.api.javafx.editor.FXSourceUtils;
import org.netbeans.api.javafx.editor.SafeTokenSequence;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.TreeUtilities;
import org.netbeans.api.lexer.Token;
import org.netbeans.modules.javafx.editor.hints.HintsModel.Hint;
import org.netbeans.modules.javafx.editor.semantic.SemanticHighlighter;
import org.netbeans.spi.editor.highlighting.HighlightsSequence;
import org.netbeans.spi.editor.highlighting.support.OffsetsBag;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.openide.filesystems.FileObject;

/**
 *
 * @author karol harezlak
 */
public class UnusedVarTaskFactory extends EditorAwareJavaSourceTaskFactory {

    private Document document;

    public UnusedVarTaskFactory() {
        super(JavaFXSource.Phase.ANALYZED, JavaFXSource.Priority.LOW);
    }

    @Override
    protected CancellableTask<CompilationInfo> createTask(final FileObject file) {
        this.document = FXSourceUtils.getDocument(file);

        return new CancellableTask<CompilationInfo>() {

            public void cancel() {
            }

            public void run(final CompilationInfo compilationInfo) throws Exception {
                final Map<Element, Tree> varInit = new HashMap<Element, Tree>();
                final Map<Tree, String> varNames = new HashMap<Tree, String>();
                final Map<Element, Tree> varToRemove = new HashMap<Element, Tree>();

                JavaFXTreePathScanner<Void, HintsModel> varVisitor = new JavaFXTreePathScanner<Void, HintsModel>() {

                    @Override
                    public Void visitVariable(VariableTree node, HintsModel model) {
                        Element element = compilationInfo.getTrees().getElement(getCurrentPath());
                        if (element != null && element.getSimpleName() != null) {
                            if (element.getKind() == ElementKind.LOCAL_VARIABLE ||
                                    element.getKind() == ElementKind.FIELD) {
                                varInit.put(element, getCurrentPath().getLeaf());
                                varNames.put(node, element.getSimpleName().toString());
                            }
                        }

                        return super.visitVariable(node, model);
                    }

                    @Override
                    public Void visitIdentifier(IdentifierTree node, HintsModel p) {
                        addToRemove(node);
                        return super.visitIdentifier(node, p);
                    }

                    @Override
                    public Void visitFunctionDefinition(FunctionDefinitionTree node, HintsModel model) {
                        for (Tree tree : node.getFunctionValue().getParameters()) {
                            addToInit(tree);
                        }

                        return super.visitFunctionDefinition(node, model);
                    }

                    @Override
                    public Void visitForExpression(ForExpressionTree node, HintsModel p) {
                        for (Tree tree : node.getInClauses()) {
                            if (tree instanceof JFXForExpressionInClause) {
                                Tree variable = ((JFXForExpressionInClause) tree).getVariable();
                                addToInit(variable);
                            }
                        }

                        return super.visitForExpression(node, p);
                    }

                    @Override
                    public Void visitCatch(CatchTree node, HintsModel p) {
                        addToInit(node.getParameter());
                        return super.visitCatch(node, p);
                    }


                    private void addToInit(Tree node) {
                        if (node == null) {
                            return;
                        }
                        JavaFXTreePath path = compilationInfo.getTrees().getPath(compilationInfo.getCompilationUnit(), node);
                        Element element = compilationInfo.getTrees().getElement(path);
                        if (element == null || element.getSimpleName() == null) {
                            return;
                        }
                        varInit.put(element, node);
                        varNames.put(node, element.getSimpleName().toString());
                    }

                    private void addToRemove(Tree node) {
                        if (node == null) {
                            return;
                        }
                        JavaFXTreePath path = compilationInfo.getTrees().getPath(compilationInfo.getCompilationUnit(), node);
                        Element element = compilationInfo.getTrees().getElement(path);
                        varToRemove.put(element, node);
                    }
                };

                HintsModel model = new HintsModel(compilationInfo);
                varVisitor.scan(compilationInfo.getCompilationUnit(), model);
                for (Element element : varInit.keySet()) {
                    Tree tree = varInit.get(element);
                    if (tree instanceof JFXVar) {
                        if (((JFXVar) varInit.get(element)).isBound()) {
                            varToRemove.put(element, tree);
                            varNames.put(tree, element.getSimpleName().toString());
                        }
                    }
                }
                for (Element element : varToRemove.keySet()) {
                    if (varInit.containsKey(element)) {
                        varInit.remove(element);
                    }
                }
                for (Tree tree : varInit.values()) {
                    SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
                    long start = sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), tree);
                    long end = sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), tree);
                    if (start < 0 || end < 0) {
                        continue;
                    }
                    TreeUtilities tu = compilationInfo.getTreeUtilities();
                    //TODO this is dumb cancelable, need to be replaced with real one
                    Cancellable cancellable = new Cancellable() {

                        public boolean isCancelled() {
                            return false;
                        }

                        public void cancell() {
                        }
                    };
                    SafeTokenSequence<JFXTokenId> tokenSequence = new SafeTokenSequence<JFXTokenId>(tu.tokensFor(tree), document, cancellable);
                    while (tokenSequence.moveNext()) {
                        Token token = tokenSequence.token();
                        if (token.toString().equals(varNames.get(tree))) {
                            start = tokenSequence.offset();
                            end = start + token.length() - 1;
                        }
                    }
                    model.addHint(tree, varNames.get(tree), (int) start, (int) end);
                }
                if (model.getHints() != null) {
                    Collection<ErrorDescription> errors = new HashSet<ErrorDescription>();
                    for (final Hint hint : model.getHints()) {
                        SwingUtilities.invokeLater(new Runnable() {

                            public void run() {
                                updateEditor(compilationInfo, hint);
                            }
                        });
                        
                        //errors.add(getErrorDescription(file, hint, compilationInfo)); //NOI18N
                    }
                //HintsController.setErrors(document, "Non init var", errors); //NOI18N
                }
            }
        };
    }

//    private ErrorDescription getErrorDescription(FileObject file, Hint hint, CompilationInfo compilationInfo) {
//        int start = hint.getStartPosition();
//        int end = hint.getLength() + hint.getStartPosition();
//        ErrorDescription ed = ErrorDescriptionFactory.createErrorDescription(Severity.HINT, "Not init var", Collections.EMPTY_LIST, file, start, end);
//        return ed;
//    }

    private void updateEditor(CompilationInfo compilationInfo, Hint hint) {
        int start = hint.getStartPosition();
        int end = hint.getLength() + hint.getStartPosition();

        HighlightsSequence hs = getBag(document).getHighlights(start, end);
        List<AttributeSet> as = new ArrayList<AttributeSet>();
        while (hs.moveNext()) {
            as.add(hs.getAttributes());
        }
        as.add(AttributesUtilities.createImmutable(StyleConstants.Underline, Color.LIGHT_GRAY));
        AttributeSet[] array = as.toArray(new AttributeSet[as.size()]);
        AttributeSet asfinal = AttributesUtilities.createImmutable(array);
        getBag(document).addHighlight(start, end, asfinal);
    }

    private static OffsetsBag getBag(Document doc) {
        OffsetsBag bag = (OffsetsBag) doc.getProperty(SemanticHighlighter.class);
        if (bag == null) {
            doc.putProperty(SemanticHighlighter.class, bag = new OffsetsBag(doc));
        }

        return bag;
    }
}
