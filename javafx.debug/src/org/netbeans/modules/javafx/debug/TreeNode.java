/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
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
package org.netbeans.modules.javafx.debug;

import com.sun.javafx.api.tree.*;
import com.sun.tools.javafx.tree.JFXErroneous;
import com.sun.tools.javafx.tree.JFXTree;
import com.sun.tools.javafx.tree.JavafxPretty;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import javax.lang.model.type.TypeMirror;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.openide.nodes.AbstractNode;
import org.openide.nodes.Children;
import org.openide.nodes.Node;
import org.openide.util.NbBundle;

/**
 *
 * @author Jan Lahoda
 */
public class TreeNode extends AbstractNode implements OffsetProvider {
    
    private JavaFXTreePath tree;
    private CompilationInfo info;
    private boolean synthetic;
    
    public static Node getTree(CompilationInfo info, JavaFXTreePath tree) {
        List<Node> result = new ArrayList<Node>();
        
        new FindChildrenTreeVisitor(info).scan(tree, result);
        
        return result.get(0);
    }
    
    private static String treeToString(CompilationInfo info, JavaFXTreePath tp) {
        Tree t = tp.getLeaf();
        StringWriter s = new StringWriter();
        try {
            new JavafxPretty(s, false).printExpr((JFXTree)t);
        } catch (Exception e) {
            Logger.getLogger(TreeNode.class.getName()).log(Level.FINE, "Unable to pretty print " + t.getJavaFXKind(), e); // NOI18N
        }
        String res = t.getJavaFXKind().toString();

        SourcePositions pos = info.getTrees().getSourcePositions();
        res = res + '[' + pos.getStartPosition(tp.getCompilationUnit(), t) + ',' +  // NOI18N
                pos.getEndPosition(tp.getCompilationUnit(), t) + "]:" + s.toString(); // NOI18N
        return res;
    }

    /** Creates a new instance of TreeNode */
    public TreeNode(CompilationInfo info, JavaFXTreePath tree, List<Node> nodes) {
        super(nodes.isEmpty() ? Children.LEAF: new NodeChilren(nodes));
        this.tree = tree;
        this.info = info;
        // TODO:
//        this.synthetic = info.getTreeUtilities().isSynthetic(tree);
        setDisplayName(treeToString(info, tree)); //NOI18N
        setIconBaseWithExtension("org/netbeans/modules/java/debug/resources/tree.png"); //NOI18N
    }

    @Override
    public String getHtmlDisplayName() {
        if (synthetic) {
            return "<html><font color='#808080'>" + translate(getDisplayName()); //NOI18N
        }
        
        return null;
    }
            
    private static String[] c = new String[] {"&", "<", ">", "\""}; // NOI18N
    private static String[] tags = new String[] {"&amp;", "&lt;", "&gt;", "&quot;"}; // NOI18N
    
    private String translate(String input) {
        for (int cntr = 0; cntr < c.length; cntr++) {
            input = input.replaceAll(c[cntr], tags[cntr]);
        }
        
        return input;
    }
    
    public int getStart() {
        return (int)info.getTrees().getSourcePositions().getStartPosition(tree.getCompilationUnit(), tree.getLeaf());
    }

    public int getEnd() {
        return (int)info.getTrees().getSourcePositions().getEndPosition(tree.getCompilationUnit(), tree.getLeaf());
    }

    public int getPreferredPosition() {
        return -1;
    }
    
    private static final class NodeChilren extends Children.Keys {
        
        public NodeChilren(List<Node> nodes) {
            setKeys(nodes);
        }
        
        protected Node[] createNodes(Object key) {
            return new Node[] {(Node) key};
        }
        
    }
    
    private static class FindChildrenTreeVisitor extends JavaFXTreePathScanner<Void, List<Node>> {

        private CompilationInfo info;
        
        public FindChildrenTreeVisitor(CompilationInfo info) {
            this.info = info;
        }

        @Override
        public Void visitBlockExpression(BlockExpressionTree tree, List<Node> d) {
                        List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitBlockExpression(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitClassDeclaration(ClassDeclarationTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingElement(below);
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitClassDeclaration(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitForExpression(ForExpressionTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitForExpression(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitForExpressionInClause(ForExpressionInClauseTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitForExpressionInClause(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitFunctionDefinition(FunctionDefinitionTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingElement(below);
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitFunctionDefinition(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitFunctionValue(FunctionValueTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitFunctionValue(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitIndexof(IndexofTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitIndexof(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitInitDefinition(InitDefinitionTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingElement(below);
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitInitDefinition(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitInterpolateValue(InterpolateValueTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitInterpolateValue(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitMissingExpression(ExpressionTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();

            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitMissingExpression(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitObjectLiteralPart(ObjectLiteralPartTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingElement(below);
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitObjectLiteralPart(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitOnReplace(OnReplaceTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitOnReplace(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitPostInitDefinition(InitDefinitionTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitPostInitDefinition(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitSequenceDelete(SequenceDeleteTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitSequenceDelete(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitSequenceEmpty(SequenceEmptyTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitSequenceEmpty(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitSequenceExplicit(SequenceExplicitTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitSequenceExplicit(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitSequenceIndexed(SequenceIndexedTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitSequenceIndexed(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitSequenceInsert(SequenceInsertTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitSequenceInsert(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitSequenceRange(SequenceRangeTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitSequenceRange(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitSequenceSlice(SequenceSliceTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitSequenceSlice(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitStringExpression(StringExpressionTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitStringExpression(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitTimeLiteral(TimeLiteralTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitTimeLiteral(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitTrigger(TriggerTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitTrigger(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitTypeAny(TypeAnyTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitTypeAny(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitTypeClass(TypeClassTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingElement(below);
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitTypeClass(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitTypeFunctional(TypeFunctionalTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitTypeFunctional(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitTypeUnknown(TypeUnknownTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitTypeUnknown(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitAssignment(AssignmentTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitAssignment(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitCompoundAssignment(CompoundAssignmentTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitCompoundAssignment(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitBinary(BinaryTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitBinary(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitBreak(BreakTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitBreak(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitCatch(CatchTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitCatch(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        /*@Override
        public Void visitClass(ClassTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingElement(below);
            addCorrespondingType(below);
            addCorrespondingComments(below);
            
            super.visitClass(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }*/
        
        @Override
        public Void visitConditionalExpression(ConditionalExpressionTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitConditionalExpression(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitInstantiate(InstantiateTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitInstantiate(tree, below);

            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitContinue(ContinueTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitContinue(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitErroneous(ErroneousTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            scan(((JFXErroneous)tree).errs, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitIdentifier(IdentifierTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingElement(below);
            addCorrespondingType(below);
            addCorrespondingComments(below);
            
            super.visitIdentifier(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitImport(ImportTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitImport(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitLiteral(LiteralTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitLiteral(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitMethodInvocation(FunctionInvocationTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingElement(below);
            addCorrespondingType(below);
            addCorrespondingComments(below);
            
            super.visitMethodInvocation(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitModifiers(ModifiersTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitModifiers(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitParenthesized(ParenthesizedTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitParenthesized(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitReturn(ReturnTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitReturn(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitMemberSelect(MemberSelectTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingElement(below);
            addCorrespondingType(below);
            addCorrespondingComments(below);
            
            super.visitMemberSelect(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitEmptyStatement(EmptyStatementTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitEmptyStatement(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitThrow(ThrowTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitThrow(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitCompilationUnit(UnitTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingElement(below);
            addCorrespondingType(below);
            addCorrespondingComments(below);
            
            super.visitCompilationUnit(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitTry(TryTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitTry(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitTypeCast(TypeCastTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitTypeCast(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitInstanceOf(InstanceOfTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitInstanceOf(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitUnary(UnaryTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitUnary(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitVariable(VariableTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingElement(below);
            addCorrespondingType(below);
            addCorrespondingComments(below);
            
            super.visitVariable(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitWhileLoop(WhileLoopTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitWhileLoop(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        @Override
        public Void visitKeyFrameLiteral(KeyFrameLiteralTree tree, List<Node> d) {
            List<Node> below = new ArrayList<Node>();
            
            addCorrespondingType(below);
            addCorrespondingComments(below);
            super.visitKeyFrameLiteral(tree, below);
            
            d.add(new TreeNode(info, getCurrentPath(), below));
            return null;
        }

        private void addCorrespondingElement(List<Node> below) {
            JavaFXTreePath tp = getCurrentPath();
            try {
                Element el = info.getTrees().getElement(tp);

                if (el != null) {
                    below.add(new ElementNode(info, el, Collections.EMPTY_LIST));
                } else {
                    below.add(new NotFoundElementNode(NbBundle.getMessage(TreeNode.class, "Cannot_Resolve_Element"))); // NOI18N
                }
            } catch (Exception e) {
                Logger.getLogger(TreeNode.class.getName()).log(Level.WARNING, treeToString(info, tp), e);
                below.add(new NotFoundElementNode(NbBundle.getMessage(TreeNode.class, "Cannot_Resolve_Element"))); // NOI18N
            }
        }

        private void addCorrespondingType(List<Node> below) {
            TypeMirror tm = info.getTrees().getTypeMirror(getCurrentPath());
            
            if (tm != null) {
                below.add(new TypeNode(tm));
            } else {
                below.add(new NotFoundTypeNode(NbBundle.getMessage(TreeNode.class, "Cannot_Resolve_Type"))); // NOI18N
            }
        }
        
        private void addCorrespondingComments(List<Node> below) {
// TODO:
//            below.add(new CommentsNode(NbBundle.getMessage(TreeNode.class, "NM_Preceding_Comments"), info.getTreeUtilities().getComments(getCurrentPath().getLeaf(), true)));
//            below.add(new CommentsNode(NbBundle.getMessage(TreeNode.class, "NM_Trailing_Comments"), info.getTreeUtilities().getComments(getCurrentPath().getLeaf(), false)));
        }
    }
    
    private static class NotFoundElementNode extends AbstractNode {
        
        public NotFoundElementNode(String name) {
            super(Children.LEAF);
            setName(name);
            setDisplayName(name);
            setIconBaseWithExtension("org/netbeans/modules/java/debug/resources/element.png"); //NOI18N
        }
        
    }
    
    private static class TypeNode extends AbstractNode {
        
        public TypeNode(TypeMirror type) {
            super(Children.LEAF);
            setDisplayName(type.getKind().toString() + ":" + type.toString()); //NOI18N
            setIconBaseWithExtension("org/netbeans/modules/java/debug/resources/type.png"); //NOI18N
        }
        
    }
    
    private static class NotFoundTypeNode extends AbstractNode {
        
        public NotFoundTypeNode(String name) {
            super(Children.LEAF);
            setName(name);
            setDisplayName(name);
            setIconBaseWithExtension("org/netbeans/modules/java/debug/resources/type.png"); //NOI18N
        }
        
    }    
}
