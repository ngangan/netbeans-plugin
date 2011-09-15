/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
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


package org.netbeans.modules.visage.bindspy;

import com.sun.visage.api.JavafxBindStatus;
import com.sun.visage.api.tree.ForExpressionInClauseTree;
import com.sun.visage.api.tree.ForExpressionTree;
import com.sun.visage.api.tree.IdentifierTree;
import com.sun.visage.api.tree.InstantiateTree;
import com.sun.visage.api.tree.VisageTreePathScanner;
import com.sun.visage.api.tree.ObjectLiteralPartTree;
import com.sun.visage.api.tree.SourcePositions;
import com.sun.visage.api.tree.VariableTree;
import com.sun.tools.visage.tree.VSGForExpression;
import com.sun.tools.visage.tree.VSGForExpressionInClause;
import javax.swing.text.Document;
import org.netbeans.api.visage.source.ClassIndex;
import org.netbeans.api.visage.source.CompilationInfo;
import org.netbeans.modules.visage.bindspy.BindsModel.BindVariable;
import org.openide.loaders.DataObject;

/**
 *
 * @author Michal Skvor <michal.skvor at sun.com>
 */
public class BindsWalker extends VisageTreePathScanner<Void, BindsModel> {

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
        if( o instanceof VSGForExpressionInClause ) {
            VSGForExpressionInClause ex = (VSGForExpressionInClause)o;
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
//        if( node.getLeftOperand().getVisageKind() == BinaryTree.VisageKind.IDENTIFIER ) {
//            System.out.println(" - binary : identifier = " + node.getLeftOperand().toString());
//        }
//        if( node.getRightOperand().getVisageKind() == BinaryTree.VisageKind.IDENTIFIER ) {
//            System.out.println(" - binary : identifier = " + node.getRightOperand().toString());
//        }
//        return super.visitBinary(node, p);
//    }

//    @Override
//    public Void visitUnary(UnaryTree node, BindsModel p) {
//        if( node.getExpression().getVisageKind() == UnaryTree.VisageKind.IDENTIFIER ) {
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
            System.out.println("    - " + node.getExpression().getVisageKind());
        }
        return super.visitObjectLiteralPart(node, p);
    }
}
