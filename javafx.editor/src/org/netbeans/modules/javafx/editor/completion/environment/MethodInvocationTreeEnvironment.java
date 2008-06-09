/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 2008 Sun Microsystems, Inc. All rights reserved.
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
 * 
 * Contributor(s):
 * 
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.editor.completion.environment;

import com.sun.source.tree.BlockTree;
import com.sun.source.tree.ExpressionTree;
import com.sun.source.tree.IdentifierTree;
import com.sun.source.tree.MemberSelectTree;
import com.sun.source.tree.MethodInvocationTree;
import com.sun.source.tree.Tree;
import com.sun.source.util.SourcePositions;
import com.sun.source.util.TreePath;
import com.sun.tools.javafx.api.JavafxcScope;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.ExecutableType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.Types;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.ElementUtilities;
import org.netbeans.api.javafx.source.JavaFXSource.Phase;
import org.netbeans.api.javafx.source.TreeUtilities;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.javafx.editor.completion.JavaFXCompletionEnvironment;

/**
 *
 * @author David Strupl
 */
public class MethodInvocationTreeEnvironment extends JavaFXCompletionEnvironment<MethodInvocationTree> {

    private static final Logger logger = Logger.getLogger(MethodInvocationTreeEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);
    
    private static final String THIS_KEYWORD = "this";
    private static final String SUPER_KEYWORD = "super";
    private static final String INIT = "<init>";

    @Override
    protected void inside(MethodInvocationTree t) throws IOException {
        log("inside MethodInvocationTree " + t);
        MethodInvocationTree mi = t;
        TokenSequence<JFXTokenId> ts = findLastNonWhitespaceToken(mi, offset);
        if (ts == null || (ts.token().id() != JFXTokenId.LPAREN && ts.token().id() != JFXTokenId.COMMA)) {
            SourcePositions sp = getSourcePositions();
            int lastTokenEndOffset = ts.offset() + ts.token().length();
            for (ExpressionTree arg : mi.getArguments()) {
                int pos = (int) sp.getEndPosition(root, arg);
                if (lastTokenEndOffset == pos) {
                    insideExpression(new TreePath(path, arg));
                    break;
                }
                if (offset <= pos) {
                    break;
                }
            }
            return;
        }
        if (prefix == null || prefix.length() == 0) {
            addMethodArguments(mi);
        }
        addLocalMembersAndVars();
        addValueKeywords();
    }

    @Override
    public Set<? extends TypeMirror> getSmartTypes(MethodInvocationTree mi) throws IOException {
        controller.toPhase(Phase.ANALYZED);

        List<Tree> argTypes = getArgumentsUpToPos(mi.getArguments(), (int) sourcePositions.getEndPosition(root, mi.getMethodSelect()), offset);
        if (argTypes != null) {
            TypeMirror[] args = new TypeMirror[argTypes.size()];
            int j = 0;
            for (Tree t : argTypes) {
                args[j++] = controller.getTrees().getTypeMirror(new TreePath(path, t));
            }
            Tree mid = mi.getMethodSelect();
            path = new TreePath(path, mid);
            final TreePath p = new TreePath(root);
            final JavafxcScope scope = controller.getTrees().getScope(p);
            switch (mid.getKind()) {
                case MEMBER_SELECT: {
                    String name = ((MemberSelectTree) mid).getIdentifier().toString();
                    ExpressionTree exp = ((MemberSelectTree) mid).getExpression();
                    path = new TreePath(path, exp);
                    final TypeMirror tm = controller.getTrees().getTypeMirror(path);
                    final Element el = controller.getTrees().getElement(path);
                    final TreeUtilities tu = controller.getTreeUtilities();
                    if (el != null && tm.getKind() == TypeKind.DECLARED) {
                        final boolean isStatic = el.getKind().isClass() || el.getKind().isInterface() || el.getKind() == ElementKind.TYPE_PARAMETER;
                        final boolean isSuperCall = el != null && el.getKind().isField() && el.getSimpleName().contentEquals(SUPER_KEYWORD);
                        TypeElement enclClass = scope.getEnclosingClass();
                        final TypeMirror enclType = enclClass != null ? enclClass.asType() : null;
                        ElementUtilities.ElementAcceptor acceptor = new ElementUtilities.ElementAcceptor() {

                            public boolean accept(Element e, TypeMirror t) {
                                return e.getKind() == ElementKind.METHOD && (!isStatic || e.getModifiers().contains(Modifier.STATIC)) && tu.isAccessible(scope, e, isSuperCall && enclType != null ? enclType : t);
                            }
                        };
                        return getMatchingArgumentTypes(tm, controller.getElementUtilities().getMembers(tm, acceptor), name, args, controller.getTypes());
                    }
                    return null;
                }
                case IDENTIFIER: {
                    String name = ((IdentifierTree) mid).getName().toString();
                    final TreeUtilities tu = controller.getTreeUtilities();
                    final TypeElement enclClass = scope.getEnclosingClass();
                    final boolean isStatic = enclClass != null ? (tu.isStaticContext(scope) || (path.getLeaf().getKind() == Tree.Kind.BLOCK && ((BlockTree) path.getLeaf()).isStatic())) : false;
                    if (SUPER_KEYWORD.equals(name) && enclClass != null) {
                        ElementUtilities.ElementAcceptor acceptor = new ElementUtilities.ElementAcceptor() {

                            public boolean accept(Element e, TypeMirror t) {
                                return e.getKind() == ElementKind.CONSTRUCTOR && tu.isAccessible(scope, e, t);
                            }
                        };
                        TypeMirror superclass = enclClass.getSuperclass();
                        return getMatchingArgumentTypes(superclass, controller.getElementUtilities().getMembers(superclass, acceptor), INIT, args, controller.getTypes());
                    }
                    ElementUtilities.ElementAcceptor acceptor = new ElementUtilities.ElementAcceptor() {

                        public boolean accept(Element e, TypeMirror t) {
                            return e.getKind() == ElementKind.METHOD && (!isStatic || e.getModifiers().contains(Modifier.STATIC)) && tu.isAccessible(scope, e, t);
                        }
                    };
                    return getMatchingArgumentTypes(enclClass != null ? enclClass.asType() : null, controller.getElementUtilities().getLocalMembersAndVars(scope, acceptor), THIS_KEYWORD.equals(name) ? INIT : name, args, controller.getTypes());
                }
            }
        }
        return null;
    }

    private Set<TypeMirror> getMatchingArgumentTypes(TypeMirror type, Iterable<? extends Element> elements, String name, TypeMirror[] argTypes, Types types) {
        Set<TypeMirror> ret = new HashSet<TypeMirror>();
        for (Element e : elements) {
            if ((e.getKind() == ElementKind.CONSTRUCTOR || e.getKind() == ElementKind.METHOD) && name.contentEquals(e.getSimpleName())) {
                int i = 0;
                Collection<? extends VariableElement> params = ((ExecutableElement) e).getParameters();
                if (params.size() <= argTypes.length) {
                    continue;
                }
                for (TypeMirror param : ((ExecutableType) asMemberOf(e, type, types)).getParameterTypes()) {
                    if (i == argTypes.length) {
                        ret.add(param);
                        break;
                    }
                    if (argTypes[i] == null || !types.isAssignable(argTypes[i++], param)) {
                        break;
                    }
                }
            }
        }
        return ret.isEmpty() ? null : ret;
    }

    private static void log(String s) {
        if (LOGGABLE) {
            logger.fine(s);
        }
    }
}
