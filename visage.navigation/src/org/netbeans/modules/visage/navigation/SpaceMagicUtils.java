/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Oracle and/or its affiliates. All rights reserved.
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
package org.netbeans.modules.visage.navigation;

import com.sun.tools.mjavac.code.Symbol;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.lang.model.element.Element;
import org.netbeans.api.visage.source.CompilationController;
import org.netbeans.api.visage.source.CompilationInfo;
import org.netbeans.api.visage.source.ElementHandle;
import org.netbeans.api.visage.source.Task;
import org.openide.util.Exceptions;
import org.visage.api.tree.ExpressionTree;
import org.visage.api.tree.Tree;
import org.visage.api.tree.Tree.VisageKind;
import org.visage.api.tree.UnitTree;
import org.visage.api.tree.VisageTreePath;
import org.visage.tools.api.VisagecTrees;
import org.visage.tools.tree.VisageClassDeclaration;
import org.visage.tools.tree.VisageFunctionDefinition;

/**
 * Only quintessentially pure space magic is here.
 * 
 * @author Anton Chechel The Wizard
 */
public final class SpaceMagicUtils {

    public static final String MAGIC_FUNCTION = "visage$run$"; // NOI18N

    private SpaceMagicUtils() {
    }

    public static boolean hasSpiritualInvocation(final ElementHandle<? extends Element> elementHandle, CompilationInfo compilationInfo) {
        final Element[] element = new Element[1];
        try {
            ((CompilationController)compilationInfo).runUserActionTask(new Task<CompilationController>() {
                public void run(CompilationController cc) throws Exception {
                    try {
                        element[0] = elementHandle.resolve(cc);
                    } catch (Exception e) {
                        Exceptions.printStackTrace(e);
                        // can't convert to element (incomplete element)
                    }
                }
            });
        } catch (IOException ex) {
            Exceptions.printStackTrace(ex);
        }
        return element[0] != null ? hasSpiritualInvocation(element[0]) : false;
    }

    /**
     * Determines whether a given element has spiritual invocation of magic function.
     * In other words whether element belongs to visage$run$
     * 
     * @param element to check
     * @return whether a given element belongs to visage$run$
     */
    public static boolean hasSpiritualInvocation(Element element) {
        if (element instanceof Symbol.VarSymbol) {
            Symbol.VarSymbol var = (Symbol.VarSymbol) element;
            String ownerName = var.owner.name.toString();
            return MAGIC_FUNCTION.equals(ownerName);
        }
        return false;
    }

    /**
     * Determines whether a given element is a magic function.
     * In other words whether element belongs to visage$run$
     * 
     * @param element to check
     * @return whether a given element is a magic function visage$run$
     */
    public static boolean isSpiritualMethod(Element element) {
        if (element instanceof Symbol.MethodSymbol) {
            Symbol.MethodSymbol method = (Symbol.MethodSymbol) element;
            String ownerName = method.name.toString();
            return MAGIC_FUNCTION.equals(ownerName);
        }
        return false;
    }

    /**
     * Gets all members from magic function
     * In other words whether element belongs to visage$run$
     * 
     * @param info CompilationInfo
     * @return whether a given element is a magic function visage$run$
     */
    public static List<Element> getSpiritualMembers(final CompilationInfo info) {
        final List<Element> elements = new ArrayList<Element>();
        final VisagecTrees trees = info.getTrees();
        final UnitTree cut = info.getCompilationUnit();

        for (Tree tt : cut.getTypeDecls()) {
            VisageKind kk = tt.getVisageKind();
            if (kk == VisageKind.CLASS_DECLARATION) {
                VisageClassDeclaration cd = (VisageClassDeclaration) tt;

                for (Tree jct : cd.getClassMembers()) {
                    VisageKind k = jct.getVisageKind();
                    if (k == VisageKind.FUNCTION_DEFINITION) {
                        VisageFunctionDefinition fdt = (VisageFunctionDefinition) jct;
                        if (MAGIC_FUNCTION.equals(fdt.name.toString())) {

                            for (ExpressionTree st : fdt.getBodyExpression().getStatements()) {
                                VisageTreePath path = trees.getPath(cut, fdt);
                                VisageTreePath expPath = new VisageTreePath(path, st);
                                Element element = trees.getElement(expPath);
                                if (element != null) {
                                    elements.add(element);
                                }
                            }
                        }
                    }
                }
            }
        }

        return Collections.unmodifiableList(elements);

    // This is too advanced magic at the moment, maybe will work later on

//        VisagecTrees trees = info.getTrees();
//        TreePath path = trees.getPath(e);
//        VisageFunctionDefinition tree = (VisageFunctionDefinition) trees.getTree(e);
//        VisageBlockExpression bodyExpression = tree.getBodyExpression();
//        for (StatementTree st : bodyExpression.getStatements()) {
//            if (canceled.get()) {
//                return;
//            }
//            TreePath expPath = new TreePath(path, st);
//            Element el = trees.getElement(expPath);
//            if (el == null) {
//                continue;
//            }
//            Description d = element2description(el, e, parentDescription.isInherited, info, pos);
//            if (null != d) {
//                parentDescription.subs.add(d);
//                if (el instanceof TypeElement && !d.isInherited) {
//                    addMembers((TypeElement) el, d, info, pos);
//                }
//            }
//        }

//        Symbol.MethodSymbol sym = (Symbol.MethodSymbol) e;
//        Iterable<Symbol> elements = sym.getEnclosedElements();
//        for (Symbol m : elements) {
//            if (canceled.get()) {
//                return;
//            }
//            Description d = element2description(m, e, parentDescription.isInherited, info, pos);
//            if (null != d) {
//                parentDescription.subs.add(d);
//                if (m instanceof TypeElement && !d.isInherited) {
//                    addMembers((TypeElement) m, d, info, pos);
//                }
//            }
//        }
    }
    
}
