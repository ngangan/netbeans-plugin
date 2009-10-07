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

import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.tools.javac.code.Symbol.MethodSymbol;
import com.sun.tools.javac.code.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.lang.model.element.Element;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.openide.text.Annotation;

final class HintsModel {

    private Map<Tree, HintsModel.Hint> hints;
    private CompilationInfo compilationInfo;

    HintsModel(CompilationInfo compilationInfo) {
        this.compilationInfo = compilationInfo;
    }

    void addHint(Tree tree) {
        addHint(null, tree);
    }

    void addHint(Tree tree, List<MethodSymbol> methods, Element element) {
        addHint(tree, methods, null, null, null, null, element);
    }

    void addHint(List<Type> thrownExceptions, Tree tree) {
        addHint(tree, null, thrownExceptions, null, null, null, null);
    }

    void addHint(Tree tree, String varName, Integer start, Integer end) {
        addHint(tree, null, null, varName, start, end, null);
    }

    private void addHint(Tree tree,
            List<MethodSymbol> methods,
            List<Type> thrownExceptions,
            String name,
            Integer start,
            Integer end,
            Element element
            ) {

        if (hints == null) {
            hints = new HashMap<Tree, Hint>();
        }
        if (hints.keySet().contains(tree)) {
            return;
        }
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        if (start == null) {
            start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), tree);
        }
        if (end == null) {
            end = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), tree);
        }
        int length = end - start;
        Hint hint = new Hint(thrownExceptions, methods, tree, length, start, name, element);
        hints.put(tree, hint);
    }
   
    void addCatchTree(Hint hint, Tree catchTree) {
        assert hints != null;
        if (!hints.values().contains(hint)) {
            throw new IllegalArgumentException("Hint does not existst in Set of added hints"); //NOI18N
        }
        hint.setCatchTree(catchTree);
    }
    
    @SuppressWarnings("unchecked") //NOI18N
    Collection<Hint> getHints() {
        if (hints == null) {
            return Collections.EMPTY_LIST;
        }
        return Collections.unmodifiableCollection(hints.values());
    }

    void removeHint(Hint hint) {
        hints.remove(hint.getTree());
    }

    void addAnnoation(Hint hint, Annotation annotation) {
        if (!hints.values().contains(hint)) {
            throw new IllegalArgumentException("Hint does not exist"); //NOI18N
        }
        hint.setAnnotation(annotation);
    }

    static final class Hint {

        private List<Type> exceptions = new ArrayList<Type>();
        private Tree tree;
        private Tree catchTree;
        private int length;
        private int start;
        private List<MethodSymbol> methods;
        private String name;
        private Element element;
        private Annotation annotation;

        private Hint() {
        }

        private Hint(List<Type> thrownExceptions,
                List<MethodSymbol> methods,
                Tree tree,
                int length,
                int start,
                String name,
                Element parentClass) {

            if (thrownExceptions != null) {
                this.exceptions = new ArrayList<Type>(thrownExceptions);
            }
            this.tree = tree;
            this.length = length + 1;
            this.start = start;
            if (methods != null) {
                this.methods = new ArrayList<MethodSymbol>(methods);
            }
            this.name = name;
            this.element = parentClass;
        }

        private void setCatchTree(Tree catchTree) {
            this.catchTree = catchTree;
        }

        private void setAnnotation(Annotation annotation) {
            this.annotation = annotation;
        }

        void addMethod(MethodSymbol method) {
            methods.add(method);
        }

        Tree getTree() {
            return tree;
        }

        Tree getCatchTree() {
            return catchTree;
        }

        List<MethodSymbol> getMethods() {
            return Collections.unmodifiableList(methods);
        }

        Collection<Type> getExceptions() {
            if (exceptions == null) {
                return null;
            }
            return Collections.unmodifiableList(exceptions);
        }

        void removeException(Type type) {
            exceptions.remove(type);
        }

        int getLength() {
            return length;
        }

        int getStartPosition() {
            return start;
        }

        String getName() {
            return name;
        }

        Element getElement() {
            return element;
        }

        Annotation getAnnotation() {
            return annotation;
        }
    }
}
