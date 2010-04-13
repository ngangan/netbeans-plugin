/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.bindspy;

import com.sun.javafx.api.JavafxBindStatus;
import com.sun.javafx.api.tree.ForExpressionInClauseTree;
import com.sun.javafx.api.tree.ForExpressionTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.InstantiateTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.ObjectLiteralPartTree;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javafx.tree.JFXForExpression;
import com.sun.tools.javafx.tree.JFXForExpressionInClause;
import javax.swing.text.Document;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.modules.javafx.bindspy.BindsModel.BindVariable;
import org.openide.loaders.DataObject;

/**
 *
 * @author Michal Skvor <michal.skvor at sun.com>
 */
public class BindsWalker extends JavaFXTreePathScanner<Void, BindsModel> {

    final private CompilationInfo ci;
    final private ClassIndex index;
    final private SourcePositions sp;
    final private boolean resolve;
    private Document doc;

    private static int NONE = 0;
    private static int BIND = 1;

    private int status = NONE;

    private DataObject dataObject;

    public BindsWalker( CompilationInfo info, ClassIndex index, boolean resolve, DataObject dataObject ) {
        this.ci = info;
        this.index = index;
        this.resolve = resolve;
        this.sp = info.getTrees().getSourcePositions();
        this.dataObject = dataObject;
    }

    public BindsWalker( CompilationInfo info, ClassIndex index, DataObject dataObject ) {
        this( info, index, true, dataObject );
    }

    public BindsWalker( CompilationInfo info, DataObject dataObject ) {
        this( info, null, false, dataObject );
    }

//    @Override
//    public Void visitAssignment(AssignmentTree node, BindsModel p) {
//        System.out.println(" - assignment : " + node.toString());
//        return super.visitAssignment(node, p);
//    }

    @Override
    public Void visitVariable( VariableTree node, BindsModel model ) {
        if( node != null && node.getName() != null ) {
//            System.out.println(" - variable: " + node.getName().toString() + ": " + node.getType().toString());

            SourcePositions sourcePositions = ci.getTrees().getSourcePositions();
            long start = sourcePositions.getStartPosition( ci.getCompilationUnit(), node );
            long end = sourcePositions.getEndPosition( ci.getCompilationUnit(), node );

            BindVariable bindVariable = new BindVariable( node.getName().toString(),
                    BindVariable.NONE );
            bindVariable.setURL( ci.getFileObject().getPath());
            bindVariable.setStartPosition( start );
            bindVariable.setEndPosition( end );
            model.addVariable( bindVariable );

            final JavafxBindStatus bindStatus = node.getBindStatus();
            // is bind
            if( bindStatus.isBound()) {
                status = BIND;

                bindVariable.setDirection( bindStatus.isUnidiBind() ? 1 : 2 );
                scan( node.getInitializer(), model );
                status = NONE;
            }
        }
        return super.visitVariable( node, model );
    }

    @Override
    public Void visitForExpression(ForExpressionTree node, BindsModel p) {
        Object o = node.getInClauses().get( 0 );
        if( o instanceof JFXForExpressionInClause ) {
            JFXForExpressionInClause ex = (JFXForExpressionInClause)o;
            scan( ex.getSequenceExpression(), p );
        }
        return super.visitForExpression(node, p);
    }

    @Override
    public Void visitForExpressionInClause(ForExpressionInClauseTree node, BindsModel p) {
        return super.visitForExpressionInClause(node, p);
    }


//    @Override
//    public Void visitBinary(BinaryTree node, BindsModel p) {
//        if( node.getLeftOperand().getJavaFXKind() == BinaryTree.JavaFXKind.IDENTIFIER ) {
//            System.out.println(" - binary : identifier = " + node.getLeftOperand().toString());
//        }
//        if( node.getRightOperand().getJavaFXKind() == BinaryTree.JavaFXKind.IDENTIFIER ) {
//            System.out.println(" - binary : identifier = " + node.getRightOperand().toString());
//        }
//        return super.visitBinary(node, p);
//    }

//    @Override
//    public Void visitUnary(UnaryTree node, BindsModel p) {
//        if( node.getExpression().getJavaFXKind() == UnaryTree.JavaFXKind.IDENTIFIER ) {
//            System.out.println(" - unary : identifier = " + node.toString());
//        }
//        return super.visitUnary(node, p);
//    }

    @Override
    public Void visitIdentifier( IdentifierTree node, BindsModel model ) {
        if( status == BIND ) {
            if( model.variableExists( node.getName().toString())) {
                model.addConnection( node.getName().toString());
            }
        }
        return super.visitIdentifier(node, model);
    }

    @Override
    public Void visitInstantiate(InstantiateTree node, BindsModel p) {
//        System.out.println(" - instantiate: " + node.getIdentifier().toString());
        return super.visitInstantiate(node, p);
    }

    @Override
    public Void visitObjectLiteralPart(ObjectLiteralPartTree node, BindsModel p) {
        if( node.getBindStatus().isBound()) {
            System.out.println(" - object literal : " + node.getName().toString());
            System.out.println("    - " + node.getExpression().toString());
            System.out.println("    - " + node.getExpression().getJavaFXKind());
        }
        return super.visitObjectLiteralPart(node, p);
    }
}
