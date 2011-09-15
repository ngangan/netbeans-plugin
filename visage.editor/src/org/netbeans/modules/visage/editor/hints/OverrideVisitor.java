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

package org.netbeans.modules.visage.editor.hints;

import com.sun.visage.api.tree.ClassDeclarationTree;
import com.sun.visage.api.tree.FunctionDefinitionTree;
import com.sun.visage.api.tree.VisageTreePathScanner;
import com.sun.visage.api.tree.SourcePositions;
import com.sun.tools.mjavac.code.Symbol.MethodSymbol;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.lang.model.element.Element;
import org.netbeans.api.visage.source.CompilationInfo;

/**
 *
 * @author karol harezlak
 */
final class OverrideVisitor extends VisageTreePathScanner<Void, Void> {

    private CompilationInfo compilationInfo;
    private Map<Element, List<MethodSymbol>> overriddenMethods;
    private Map<MethodSymbol, Integer> positions;
    private SourcePositions sourcePositions;

    OverrideVisitor(CompilationInfo compilationInfo,
            Map<Element, List<MethodSymbol>> overriddenMethods,
            Map<MethodSymbol, Integer> positions) {

        this.compilationInfo = compilationInfo;
        this.overriddenMethods = overriddenMethods;
        this.positions = positions;
        this.sourcePositions = compilationInfo.getTrees().getSourcePositions();
    }

    @Override
    public Void visitClassDeclaration(ClassDeclarationTree node, Void v) {
        Element currentClass = compilationInfo.getTrees().getElement(getCurrentPath());
        if (currentClass != null) {
            overriddenMethods.put(currentClass, new ArrayList<MethodSymbol>());
        }

        return super.visitClassDeclaration(node, v);
    }

    @Override
    @SuppressWarnings("element-type-mismatch")
    public Void visitFunctionDefinition(FunctionDefinitionTree node, Void v) {
//        if (node.toString().contains(" overridefunction ") || node.toString().contains(" override ")) { //NOI18N
        Element element = compilationInfo.getTrees().getElement(getCurrentPath());
        if (element != null) {
            Element currentClass = element.getEnclosingElement();
            if (element instanceof MethodSymbol) {
                List<MethodSymbol> methods = overriddenMethods.get(currentClass);
                if (!methods.contains(element)) {
                    methods.add((MethodSymbol) element);
                    positions.put((MethodSymbol) element, (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), node));
                }
                overriddenMethods.put(currentClass, methods);
            }
        }
        //}
        return super.visitFunctionDefinition(node, v);
    }
}
