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


package org.netbeans.modules.visage.editor.imports;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeKind;
import org.netbeans.api.visage.source.ClassIndex;
import org.netbeans.api.visage.source.CompilationInfo;
import org.netbeans.api.visage.source.ElementHandle;
import org.visage.api.tree.IdentifierTree;
import org.visage.api.tree.ImportTree;
import org.visage.api.tree.SourcePositions;
import org.visage.api.tree.Tree;
import org.visage.api.tree.TriggerTree;
import org.visage.api.tree.TypeClassTree;
import org.visage.api.tree.VariableTree;
import org.visage.api.tree.VisageTreePathScanner;
import org.visage.tools.tree.VisageExpression;
import org.visage.tools.tree.VisageIdent;
import org.visage.tools.tree.VisageOverrideClassVar;
import org.visage.tools.tree.VisageTypeClass;

/**
 *
 * @author Jaroslav Bachorik
 */
final public class ImportsWalker extends VisageTreePathScanner<Void, ImportsModel> {
    final private EnumSet<ClassIndex.SearchScope> SCOPE = EnumSet.of(ClassIndex.SearchScope.DEPENDENCIES, ClassIndex.SearchScope.SOURCE);
    final private CompilationInfo ci;
    final private Set<String> variableNames = new HashSet<String>();
    final private ClassIndex index;
    final private SourcePositions sp;
    final private boolean resolve;

    final private Map<String, Set<ElementHandle<TypeElement>>> indexCache = new HashMap<String, Set<ElementHandle<TypeElement>>>();

    public ImportsWalker(CompilationInfo info, ClassIndex index, boolean resolve) {
        this.ci = info;
        this.index = index;
        this.resolve = resolve;
        this.sp = info.getTrees().getSourcePositions();
    }

    public ImportsWalker(CompilationInfo info, ClassIndex index) {
        this(info, index, true);
    }

    public ImportsWalker(CompilationInfo info) {
        this(info, null, false);
    }

    @Override
    public Void visitVariable(VariableTree node, ImportsModel model) {
        if (node != null && node.getName() != null) {
            variableNames.add(node.getName().toString());
        }
        return super.visitVariable(node, model);
    }

    @Override
    public Void visitIdentifier(IdentifierTree node, ImportsModel model) {
        if (node != null && node.getName() != null) {
            String nodeName = node.getName().toString();
            if (isResolving(nodeName) || variableNames.contains(nodeName)) {
                return null;
            }
            Element e = ci.getTrees().getElement(getCurrentPath());

            /**
             * 183679: visagec for its own reasons doesn't resolve symbol for the built-in classes (eg. java.lang.*)
             * However, this information is available so we'll just grab it (with some null-checks, of course)
             */
            if (e == null) {
                e = ((VisageIdent)node).type != null ? ((VisageIdent)node).type.tsym : null;
            }
            processItem(e, nodeName, node, model);
        }

        return super.visitIdentifier(node, model);
    }

    @Override
    public Void visitTypeClass(TypeClassTree node, ImportsModel model) {
        if (node != null && node.getClassName() != null) {
            String nodeName = node.getClassName().toString();
            if (isResolving(nodeName) || variableNames.contains(nodeName)) {
                return null;
            }

            Element e = ci.getTrees().getElement(getCurrentPath());
            /**
             * 183679: visagec for its own reasons doesn't resolve symbol for the built-in classes (eg. java.lang.*)
             * However, this information is available so we'll just grab it (with some null-checks, of course)
             */
            if (e == null) {
                VisageExpression clzName = ((VisageTypeClass)node).getClassName();
                if (clzName != null) {
                    e = clzName.type != null ? clzName.type.tsym : null;
                }
            }
            processItem(e, nodeName, node, model);
        }
        
        return super.visitTypeClass(node, model);
    }

    @Override
    public Void visitImport(ImportTree node, ImportsModel model) {
        if (node != null && node.getQualifiedIdentifier() != null) {
            long start = ci.getTrees().getSourcePositions().getStartPosition(ci.getCompilationUnit(), node);
            long end = ci.getTrees().getSourcePositions().getEndPosition(ci.getCompilationUnit(), node);
            model.addDeclaredImport(node.getQualifiedIdentifier().toString(), start, end);
        }
        return super.visitImport(node, model);
    }

    @Override
    public Void visitTrigger(TriggerTree node, ImportsModel model) {
        if (node != null) {
            Tree t = ((VisageOverrideClassVar)node).getInitializer();
            if (t != null) {
                t.accept(this, model);
            }
        }
        return super.visitTrigger(node, model);
    }

    /**
     * Will use the class index to resolve the type name and update the model
     * @param typeName The type name to resolve
     * @param t The corresponding {@linkplain Tree} instance
     * @param model The {@linkplain ImportsModel} instance to update
     */
    private void doResolve(String typeName, Tree t, ImportsModel model) {
        assert index != null;
        typeName = getMainType(typeName);
        Set<ElementHandle<TypeElement>> options = index.getDeclaredTypes(typeName, ClassIndex.NameKind.SIMPLE_NAME, SCOPE);
        indexCache.put(typeName, options);
        long pos = sp.getStartPosition(ci.getCompilationUnit(), t);
        model.addUnresolved(typeName, options, pos);
    }

    /**
     * Is the type name being already resolved?
     * @param typeName The type name
     * @return Returns true if the type name is already being resolved
     */
    private boolean isResolving(String typeName) {
        typeName = getMainType(typeName);
        return indexCache.containsKey(typeName);
    }

    /**
     * Gets the topmost outer type name
     * @param typeName The inner type name
     * @return Returns the topmost outer type name
     */
    private static String getMainType(String typeName) {
        if (!typeName.contains(".")) {
            return typeName;
        } else {
            return typeName.substring(0, typeName.indexOf("."));
        }
    }

    /**
     * Will try to walk up the enclosement hierarchy to get the containing class
     * @param clazz The current class
     * @return The containing class (eg. java.util.Map for java.util.Map.Entry)
     */
    private static Element findTopClass(Element clazz) {
        Element enclosing = clazz.getEnclosingElement();
        if (enclosing != null &&
             (enclosing.getKind() == ElementKind.CLASS ||
              enclosing.getKind() == ElementKind.INTERFACE ||
              enclosing.getKind() == ElementKind.ENUM)) {
            return findTopClass(enclosing);
        }
        return clazz;
    }

    /**
     * Will process the given item specified by its {@linkplain Element}, name and {@linkplain Tree}
     * If the item is unresolved it will try to resolve it otherwise it will update the imports usage
     *
     * @param e The associated {@linkplain Element} instance
     * @param nodeName The textual representation of the item
     * @param tree The {@linkplain Tree} isntance corresponding to the given element
     * @param model The model to update accordingly
     */
    private void processItem(Element e, String nodeName, Tree tree, ImportsModel model) {
        if (e != null) {
            if (e.asType().getKind() == TypeKind.ERROR || (e.asType().getKind() == TypeKind.PACKAGE && ci.getElements().getPackageElement(nodeName) == null)) {
                if (resolve) {
                    doResolve(nodeName, tree, model);
                }
            } else {
                String className = findTopClass(e).toString();
                if (!(e.getKind().isClass() || e.getKind().isInterface())) {
                    model.addUsage(className + "." + e.getSimpleName().toString()); // NOI18N
                }
                model.addUsage(className);
            }
        } else {
            if (resolve) {
                doResolve(nodeName, tree, model);
            }
        }
    }
}
