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
 * Software is Sun Micro//S ystems, Inc. Portions Copyright 1997-2007 Sun
 * Micro//S ystems, Inc. All Rights Reserved.
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

package org.netbeans.modules.debugger.javafx.expr;

import com.sun.javafx.api.tree.Tree;
import com.sun.jdi.ArrayReference;
import com.sun.jdi.Value;
import com.sun.jdi.InvocationException;

import java.util.*;
import java.text.MessageFormat;

import org.openide.util.NbBundle;

/**
 * This class is a runtime exception because it integrates better with the generated code and
 * it also prevents unnecessary code bloat.
 *
 * @author Maros Sandor
 */
public class EvaluationException2 extends RuntimeException {

    private Tree      node;
    private String    reason;
    private Object[]  params;

    private String    message;

    public EvaluationException2(Tree node, String reason, Object[] params) {
        this.node = node;
        this.reason = reason;
        this.params = params;
    }

    public String getMessage() {
        try {
            return getMessageImpl();
        } catch (Exception e) {
            return message = formatMessage("CTL_EvalError_unknownInternalError", new String[] {e.getMessage()});
        }
    }

    public String getMessageImpl() {
        if (message != null) return message;

        String [] msgParams = null;

        if (reason.equals("internalError"))	//NOI18N
            msgParams = new String [] { null };
        else if (reason.equals("invalidArrayInitializer"))	//NOI18N
            msgParams = new String [] { params[0] == null ? null : params[0].toString() };
        else if (reason.equals("arraySizeBadType"))	//NOI18N
            msgParams = new String [] { params[0] == null ? null : params[0].toString() };
        else if (reason.equals("arrayCreateError"))	//NOI18N
            msgParams = new String [] { params[0] == null ? null : params[0].toString() };
        else if (reason.equals("instantiateInterface"))	//NOI18N
            msgParams = new String [] { params[0] == null ? null : params[0].toString() };
        else if (reason.equals("castToBooleanRequired"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("castFromBooleanRequired"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("castError"))	//NOI18N
            msgParams = new String [] { params[0].toString(), params[1].toString() };
        else if (reason.equals("badOperandForPostfixOperator"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("postfixOperatorEvaluationError"))	//NOI18N
            msgParams = new String [] { params[1].toString() };
        else if (reason.equals("badOperandForPrefixOperator"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("prefixOperatorEvaluationError"))	//NOI18N
            msgParams = new String [] { params[1].toString() };
        else if (reason.equals("badOperandForUnaryOperator"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("unaryOperatorEvaluationError"))	//NOI18N
            msgParams = new String [] { params[1].toString() };
        else if (reason.equals("unknownType"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("internalErrorResolvingType"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("instanceOfLeftOperandNotAReference"))	//NOI18N
            msgParams = new String [] { ((Value)params[0]).type().name() };
        else if (reason.equals("conditionalOrAndBooleanOperandRequired"))	//NOI18N
            msgParams = new String [] { ((Value)params[0]).type().name() };
        else if (reason.equals("conditionalQuestionMarkBooleanOperandRequired"))	//NOI18N
            msgParams = new String [] { ((Value)params[0]).type().name() };
        else if (reason.equals("thisObjectUnavailable"))	//NOI18N
            msgParams = null;
        else if (reason.equals("objectReferenceRequiredOnDereference"))	//NOI18N
            msgParams = new String [] { ((Value)params[0]).type().name() };
        else if (reason.equals("badArgument"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("argumentsBadSyntax"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("ambigousMethod"))	//NOI18N
            msgParams = new String [] { params[0].toString(),  params[1].toString() };
        else if (reason.equals("noSuchMethod"))	//NOI18N
            msgParams = new String [] { (String) params[0], (String) params[1] };
        else if (reason.equals("noSuchMethodWithArgs"))	//NOI18N
            msgParams = new String [] { (String) params[0], (String) params[1], (String) params[2] };
        else if (reason.equals("noSuchConstructorWithArgs"))	//NOI18N
            msgParams = new String [] { (String) params[0], (String) params[1] };
        else if (reason.equals("callException"))	//NOI18N
            msgParams = new String [] { params[1].toString(), params[0].toString() };
        else if (reason.equals("calleeException"))	//NOI18N
            msgParams = new String [] { ((Identifier)params[1]).typeContext.name(),  ((Identifier)params[1]).identifier,
                                        ((InvocationException)(params[0])).exception().toString() };
        else if (reason.equals("identifierNotAReference"))	//NOI18N
            msgParams = new String [] { ((Value)params[0]).type().name() };
        else if (reason.equals("notarray"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("arrayIndexNAN"))	//NOI18N
            msgParams = new String [] { params[1].toString() };
        else if (reason.equals("arrayIndexOutOfBounds"))	//NOI18N
            msgParams = new String [] { params[1].toString(), Integer.toString(((ArrayReference)params[0]).length() - 1) };
        else if (reason.equals("unknownVariable"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("integerLiteralTooBig"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("badFormatOfIntegerLiteral"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("unknownLiteralType"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("evaluateError"))	//NOI18N
//            return Assert.error(node, "evaluateError", value, ((Token) operators[i-1]).image, next);
            msgParams = new String [] { params[1].toString(), params[0].toString(), params[2].toString() };
        else if (reason.equals("notEnclosingType"))	//NOI18N
            msgParams = new String [] { ((Identifier)params[0]).typeContext.name(),  ((Identifier)params[0]).superQualifier };
        else if (reason.equals("accessInstanceVariableFromStaticContext"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("methodCallOnNull"))	//NOI18N
            msgParams = new String[] { params[0].toString() };
        else if (reason.equals("fieldOnNull"))	//NOI18N
            msgParams = new String[] { params[0].toString() };
        else if (reason.equals("arrayIsNull"))	//NOI18N
            msgParams = new String[] { params[0].toString() };
        else if (reason.equals("unsupported"))	//NOI18N
            msgParams = new String[] { node.toString() };
        else if (reason.equals("errorneous"))	//NOI18N
            msgParams = new String[] { node.toString() };
        else if (reason.equals("unknownField"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("unknownOuterClass"))	//NOI18N
            msgParams = new String [] { params[0].toString() };
        else if (reason.equals("notExpression"))	//NOI18N
            msgParams = new String [] {  };
        else if (reason.equals("methOnArray"))	//NOI18N
            msgParams = new String [] {  };
        else {
            msgParams = new String [] { reason };
            reason = "unknownInternalError";	//NOI18N
        }

        message = formatMessage("CTL_EvalError_" + reason, msgParams);	//NOI18N
        //message = formatMessage("CTL_EvalErrorExpr", new String[] { node.toString(), message });

        return message;
    }

    private String formatMessage(String msg, String [] params) {
        ResourceBundle bundle = NbBundle.getBundle (EvaluationException.class);
        msg = bundle.getString(msg);
        return MessageFormat.format(msg, (Object[]) params);
    }
}
