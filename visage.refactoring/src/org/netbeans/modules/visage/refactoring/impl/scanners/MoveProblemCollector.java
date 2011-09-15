/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 *  Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 *  Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 *  Other names may be trademarks of their respective owners.
 *
 *  The contents of this file are subject to the terms of either the GNU
 *  General Public License Version 2 only ("GPL") or the Common
 *  Development and Distribution License("CDDL") (collectively, the
 *  "License"). You may not use this file except in compliance with the
 *  License. You can obtain a copy of the License at
 *  http://www.netbeans.org/cddl-gplv2.html
 *  or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 *  specific language governing permissions and limitations under the
 *  License.  When distributing the software, include this License Header
 *  Notice in each file and include the License file at
 *  nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Oracle in the GPL Version 2 section of the License file that
 *  accompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 * 
 *  Contributor(s):
 * 
 *  The Original Software is NetBeans. The Initial Developer of the Original
 *  Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 *  Microsystems, Inc. All Rights Reserved.
 * 
 *  If you wish your version of this file to be governed by only the CDDL
 *  or only the GPL Version 2, indicate your decision by adding
 *  "[Contributor] elects to include this software in this distribution
 *  under the [CDDL or GPL Version 2] license." If you do not indicate a
 *  single choice of license, a recipient has the option to distribute
 *  your version of this file under either the CDDL, the GPL Version 2 or
 *  to extend the choice of license to its licensees as provided above.
 *  However, if you add GPL Version 2 code and therefore, elected the GPL
 *  Version 2 license, then the option applies only if the new code is
 *  made subject to such option by the copyright holder.
 */

package org.netbeans.modules.visage.refactoring.impl.scanners;

import com.sun.visage.api.tree.ClassDeclarationTree;
import com.sun.visage.api.tree.ExpressionTree;
import com.sun.visage.api.tree.FunctionInvocationTree;
import com.sun.visage.api.tree.InstantiateTree;
import com.sun.visage.api.tree.VisageTreePathScanner;
import com.sun.visage.api.tree.MemberSelectTree;
import com.sun.visage.api.tree.ObjectLiteralPartTree;
import com.sun.visage.api.tree.TypeClassTree;
import com.sun.visage.api.tree.UnitTree;
import com.sun.tools.mjavac.code.Flags;
import com.sun.tools.mjavac.code.Symbol;
import com.sun.tools.visage.code.JavafxFlags;
import com.sun.tools.visage.tree.VSGInstanciate;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import org.netbeans.api.visage.source.CompilationController;
import org.netbeans.modules.visage.refactoring.impl.plugins.MoveRefactoringPlugin;
import org.netbeans.modules.refactoring.api.Problem;
import org.openide.util.NbBundle;

/**
 *
 * @author Jaroslav Bachorik <jaroslav.bachorik@sun.com>
 */
public class MoveProblemCollector<R, P> extends VisageTreePathScanner<R, P> {
    private interface MoveProblemCallback {
        Problem createProblem(String oldPkgName, String newPkgName, String srcTypeName, String targetTypeName, String feature, boolean outgoing);
    }

    private String myPkgName;
    private Problem problem = null;

    private CompilationController cc;
    private Set<String> movedClasses;
    private Map<String, String> renameMap;

    public <R, P> MoveProblemCollector(CompilationController cc, Set<String> movedClasses, Map<String, String> renameMap) {
        this.cc = cc;
        this.movedClasses = movedClasses;
        this.renameMap = renameMap;
    }

    final protected String getPackageName() {
        return myPkgName;
    }

    final protected TypeMirror getClassType() {
        return currentClass;
    }

    final public Problem getProblem() {
        return problem;
    }

    @Override
    public R visitCompilationUnit(UnitTree node, P p) {
        ExpressionTree packageNameTree = node.getPackageName();
        myPkgName = packageNameTree != null ? packageNameTree.toString() : ""; // NOI18N
        return super.visitCompilationUnit(node, p);
    }

    private TypeMirror currentClass = null;
    final private Deque<TypeMirror> currentClassStack = new ArrayDeque<TypeMirror>();

    @Override
    public R visitClassDeclaration(ClassDeclarationTree node, P p) {
        TypeElement te = (TypeElement)cc.getTrees().getElement(getCurrentPath());
        if (te != null && !cc.getElementUtilities().isSynthetic(te)) {
            currentClass = te.asType();
        } else {
            return null;
        }
        return super.visitClassDeclaration(node, p);
    }

    @Override
    public R visitTypeClass(TypeClassTree node, P p) {
        Element e = cc.getTrees().getElement(getCurrentPath());

        problem = chainProblems(problem, checkVisibilty(e, new MoveProblemCallback() {

            public Problem createProblem(String oldPkgName, String newPkgName, String srcTypeName, String targetTypeName, String feature, boolean outgoing) {
                return new Problem(false, NbBundle.getMessage(MoveRefactoringPlugin.class, "ERR_AccessesPackagePrivateClass" + (outgoing ? "" : "1"), new String[]{srcTypeName, targetTypeName, newPkgName})); // NOI18N
            }
        }));

        return super.visitTypeClass(node, p);
    }

    @Override
    public R visitInstantiate(InstantiateTree node, P p) {
        TypeElement clzElement = (TypeElement)((VSGInstanciate)node).type.tsym;
        problem = chainProblems(problem,
            checkVisibilty(clzElement, new MoveProblemCallback() {

                public Problem createProblem(String oldPkgName, String newPkgName, String srcTypeName, String targetTypeName, String feature, boolean outgoing) {
                    return new Problem(false, NbBundle.getMessage(MoveRefactoringPlugin.class, "ERR_AccessesPackagePrivateClass"  + (outgoing ? "" : "1"), new String[]{srcTypeName, targetTypeName, newPkgName})); // NOI18N
                }
            })
        );
        return super.visitInstantiate(node, p);
    }

    @Override
    public R visitObjectLiteralPart(ObjectLiteralPartTree node, P p) {
        Element e = cc.getTrees().getElement(getCurrentPath());
        problem = chainProblems(problem, checkVisibilty(e, new MoveProblemCallback() {
            public Problem createProblem(String oldPkgName, String newPkgName, String srcTypeName, String targetTypeName, String feature, boolean outgoing) {
                return new Problem(false, NbBundle.getMessage(MoveRefactoringPlugin.class, "ERR_AccessesPackagePrivateFeature"  + (outgoing ? "" : "1"), new String[]{srcTypeName, feature, targetTypeName})); // NOI18N
            }
        }));
        return super.visitObjectLiteralPart(node, p);
    }

    @Override
    public R visitMemberSelect(MemberSelectTree node, P p) {
        Element e = cc.getTrees().getElement(getCurrentPath());
        if (e != null && e.getKind() == ElementKind.FIELD) {
            problem = chainProblems(problem, checkVisibilty(e, new MoveProblemCallback() {
                public Problem createProblem(String oldPkgName, String newPkgName, String srcTypeName, String targetTypeName, String feature, boolean outgoing) {
                    return new Problem(false, NbBundle.getMessage(MoveRefactoringPlugin.class, "ERR_AccessesPackagePrivateFeature"  + (outgoing ? "" : "1"), new String[]{srcTypeName, feature, targetTypeName}));
                }
            }));
        }
        return super.visitMemberSelect(node, p);
    }

    @Override
    public R visitMethodInvocation(FunctionInvocationTree node, P p) {
        Element e = cc.getTrees().getElement(getCurrentPath());
        problem = chainProblems(problem, checkVisibilty(e, new MoveProblemCallback() {

            public Problem createProblem(String oldPkgName, String newPkgName, String srcTypeName, String targetTypeName, String feature, boolean outgoing) {
                return new Problem(false, NbBundle.getMessage(MoveRefactoringPlugin.class, "ERR_AccessesPackagePrivateFeature"  + (outgoing ? "" : "1"), new String[]{srcTypeName, feature, targetTypeName})); // NOI18N
            }
        }));
        return super.visitMethodInvocation(node, p);
    }

    private Problem checkVisibilty(Element e, MoveProblemCallback callback) {
        if (e == null) return null;
        if (isAffected(e)) {
            boolean packageAccess = true;

            String feature = e.toString();
            String ownerTypeName = null;
            Element targetElement = e;
            while (e != null && e.getKind() != ElementKind.PACKAGE) {
                if (ownerTypeName == null && (e.getKind().isInterface() || e.getKind().isClass())) {
                    ownerTypeName = e.asType().toString();
                    if (isProtected(targetElement) && cc.getTypes().isSubtype(currentClass, e.asType())) {
                        packageAccess = false;
                    }
                }
                e = e.getEnclosingElement();
            }
            if (ownerTypeName.equals(currentClass.toString())) {
                // the owner of the element is the same class as the currently processed one
                // no need to check visibility
                return null;
            }

            Element pkgElement = e;
            if (pkgElement != null) {
                String ownerPkgName = ((PackageElement)pkgElement).getQualifiedName().toString();
                String newOwnerPkgName = movedClasses.contains(ownerTypeName) ? renameMap.get(ownerPkgName) : ownerPkgName;
                String myNewPkgName = movedClasses.contains(currentClass.toString()) ? renameMap.get(myPkgName) : myPkgName;
                if (packageAccess && !myNewPkgName.equals(newOwnerPkgName)) {
                    boolean outgoing = !ownerPkgName.equals(newOwnerPkgName);
                    return callback.createProblem(newOwnerPkgName, myNewPkgName, currentClass.toString(), ownerTypeName, feature, outgoing);
                }
            }
        }
        return null;
    }

    private boolean isProtected(Element e) {
        return (((Symbol)e).flags_field & Flags.PROTECTED) == 0L;
    }

    private boolean isAffected(Element e) {
        long flags =((Symbol)e).flags_field;
        return (flags & (JavafxFlags.PACKAGE_ACCESS | Flags.PROTECTED)) != 0L &&
               (flags & (JavafxFlags.PUBLIC_INIT | JavafxFlags.PUBLIC_READ)) == 0L; // for simplicity just check for any public modifier that can be used in addition with the protected modifier
    }

    private static Problem chainProblems(Problem p,Problem p1) {
        Problem problem;

        if (p==null) return p1;
        if (p1==null) return p;
        problem=p;
        while(problem.getNext()!=null) {
            problem=problem.getNext();
        }
        problem.setNext(p1);
        return p;
    }
}
