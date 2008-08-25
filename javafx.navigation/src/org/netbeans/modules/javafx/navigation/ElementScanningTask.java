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
package org.netbeans.modules.javafx.navigation;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.UnitTree;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javafx.api.JavafxcScope;
import com.sun.tools.javafx.api.JavafxcTrees;
import com.sun.tools.javafx.code.JavafxTypes;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.ArrayType;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.type.TypeVariable;
import javax.lang.model.type.WildcardType;
import org.netbeans.api.javafx.editor.FXSourceUtils;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.navigation.ElementNode.Description;

/** 
 *
 * @author phrebejk
 * @author Anton Chechel (space magic fixes only)
 */
public class ElementScanningTask implements CancellableTask<CompilationInfo> {

    private static final String TYPE_COLOR = "#707070";
    private static final String INHERITED_COLOR = "#7D694A";

    private ClassMemberPanelUI ui;
    private final AtomicBoolean canceled = new AtomicBoolean();

    public ElementScanningTask(ClassMemberPanelUI ui) {
        this.ui = ui;
    }

    public void cancel() {
        canceled.set(true);
    }

    public void run(CompilationInfo info) throws Exception {
        canceled.set(false); // Task shared for one file needs reset first

        Description rootDescription = new Description(ui);
        rootDescription.fileObject = info.getFileObject();
        rootDescription.subs = new HashSet<Description>();

        // Get all outerclasses in the Compilation unit
        UnitTree cuTree = info.getCompilationUnit();
        List<? extends TypeElement> elements = info.getTopLevelElements();

        final Map<Element, Long> pos = new HashMap<Element, Long>();
        if (!canceled.get()) {
            JavafxcTrees trees = info.getTrees();
            PositionVisitor posVis = new PositionVisitor(info, trees, canceled);
            posVis.scan(cuTree, pos);
        }

        if (!canceled.get()) {
            Element magicRunMethod = null;
            out:
            for (Element element : elements) {
                Symbol.ClassSymbol sym = (Symbol.ClassSymbol) element;
                for (Symbol symbol : sym.members().getElements()) {
                    if (SpaceMagicUtils.isSpiritualMethod(symbol)) {
                        magicRunMethod = symbol;
                        break out;
                    }
                }
            }

            // top level elements
            for (Element element : elements) {
                Description topLevel = element2description(element, null, false, info, pos);
                if (null != topLevel) {
                    rootDescription.subs.add(topLevel);
                    addMembers((TypeElement) element, topLevel, info, pos);
                }
            }

            // elements from magic javafx$run$ method, should be top level as well
            if (magicRunMethod != null) {
                addMembers((ExecutableElement) magicRunMethod, rootDescription, info, pos);
            }
        }

        if (!canceled.get()) {
            ui.refresh(rootDescription, info);
        }
    }

    private static class PositionVisitor extends JavaFXTreePathScanner<Void, Map<Element, Long>> {

        private final CompilationInfo info;
        private final JavafxcTrees trees;
        private final SourcePositions sourcePositions;
        private final AtomicBoolean canceled;
        private UnitTree cu;

        public PositionVisitor(final CompilationInfo info, final JavafxcTrees trees, final AtomicBoolean canceled) {
            assert trees != null;
            assert canceled != null;
            this.info = info;
            this.trees = trees;
            this.sourcePositions = trees.getSourcePositions();
            this.canceled = canceled;
        }

        @Override
        public Void visitCompilationUnit(UnitTree node, Map<Element, Long> p) {
            this.cu = node;
            return super.visitCompilationUnit(node, p);
        }

        @Override
        public Void visitClassDeclaration(ClassDeclarationTree node, Map<Element, Long> p) {
            Element e = this.trees.getElement(this.getCurrentPath());
            if (e != null) {
                long pos = this.sourcePositions.getStartPosition(cu, node);
                p.put(e, pos);
            }
            return super.visitClassDeclaration(node, p);
        }

        @Override
        public Void visitFunctionDefinition(FunctionDefinitionTree node, Map<Element, Long> p) {
            Element e = this.trees.getElement(this.getCurrentPath());
            if (e != null) {
                long pos = this.sourcePositions.getStartPosition(cu, node);
                p.put(e, pos);
                
                // guesss what? space magic! weeeee! >:-((
                if (SpaceMagicUtils.isSpiritualMethod(e)) {
                    List<Element> spiritualMembers = SpaceMagicUtils.getSpiritualMembers(info);
                    for (Element sm : spiritualMembers) {
                        JavaFXTreePath smPath = info.getPath(sm);
                        Tree smTree = smPath != null ? smPath.getLeaf() : null;

                        if (smTree != null) {
                            long smPos = this.sourcePositions.getStartPosition(cu, smTree);
                            p.put(sm, smPos);
                        }
                    }
                }
            }
            return null;
        }

        @Override
        public Void visitVariable(VariableTree node, Map<Element, Long> p) {
            Element e = this.trees.getElement(this.getCurrentPath());
            if (e != null) {
                long pos = this.sourcePositions.getStartPosition(cu, node);
                p.put(e, pos);
            }
            return null;
        }

        @Override
        public Void scan(Tree tree, Map<Element, Long> p) {
            if (!canceled.get()) {
                return super.scan(tree, p);
            } else {
                return null;
            }
        }
    }

    private void addMembers(final TypeElement e, final Description parentDescription, final CompilationInfo info, final Map<Element, Long> pos) {
        if (e == null) {
            return;
        }
        List<? extends Element> members = info.getElements().getAllMembers(e);
        for (Element m : members) {
            if (canceled.get()) {
                return;
            }
            Description d = element2description(m, e, parentDescription.isInherited, info, pos);
            if (null != d) {
                parentDescription.subs.add(d);
                if (m instanceof TypeElement && !d.isInherited) {
                    addMembers((TypeElement) m, d, info, pos);
                }
            }
        }
    }

    private void addMembers(final ExecutableElement e, final Description parentDescription, final CompilationInfo info, final Map<Element, Long> pos) {
        if (e == null) {
            return;
        }

        List<Element> spiritualMembers = SpaceMagicUtils.getSpiritualMembers(info);
        for (Element el : spiritualMembers) {
            Description d = element2description(el, e, parentDescription.isInherited, info, pos);
            if (null != d) {
                parentDescription.subs.add(d);
                if (el instanceof TypeElement && !d.isInherited) {
                    addMembers((TypeElement) el, d, info, pos);
                }
            }
        }
    }

    private Description element2description(final Element e, final Element parent,
            final boolean isParentInherited, final CompilationInfo info,
            final Map<Element, Long> pos) {

        final String name = e.getSimpleName().toString();
        final boolean spaceMagic = SpaceMagicUtils.isSpiritualMethod(e.getEnclosingElement());
        if (!spaceMagic && info.getElementUtilities().isSynthetic(e)) {
            return null;
        }

        final boolean inherited = isParentInherited || (null != parent && !parent.equals(e.getEnclosingElement()));
        final ElementKind kind = e.getKind();
        final ElementKind spaceMagicKind = kind == ElementKind.LOCAL_VARIABLE ? ElementKind.FIELD : kind;
        Description d = new Description(ui, name, ElementHandle.create(e), spaceMagicKind, inherited);
        final JavafxTypes javafxTypes = info.getJavafxTypes();

        if (e instanceof TypeElement) {
            if (null != parent) {
                final JavafxcTrees trees = info.getTrees();
                final JavafxcScope scope = trees.getScope(info.getPath(parent));
                if (!trees.isAccessible(scope, (TypeElement) e)) {
                    return null;
                }
            }

            d.subs = new HashSet<Description>();
            d.htmlHeader = createHtmlHeader((TypeElement) e, info.getElements().isDeprecated(e), d.isInherited, javafxTypes);
        } else if (e instanceof ExecutableElement) {
            if (!spaceMagic && name.contains("$")) {
                return null;
            }

            d.htmlHeader = createHtmlHeader((ExecutableElement) e, info.getElements().isDeprecated(e), d.isInherited, javafxTypes);
        } else if (e instanceof VariableElement) {
            if (!spaceMagic && kind != ElementKind.FIELD && kind != ElementKind.ENUM_CONSTANT) {
                return null;
            }
            d.htmlHeader = createHtmlHeader((VariableElement) e, info.getElements().isDeprecated(e), d.isInherited, javafxTypes);
        }

        d.modifiers = e.getModifiers();
        d.pos = getPosition(e, pos);

        return d;
    }

    private long getPosition(final Element e, final Map<Element, Long> pos) {
        Long res = pos.get(e);
        if (res == null) {
            return -1;
        }
        return res.longValue();
    }

    /** Creates HTML display name of the Executable element */
    private String createHtmlHeader(ExecutableElement e, boolean isDeprecated, boolean isInherited, JavafxTypes types) {

        StringBuilder sb = new StringBuilder();
        if (isDeprecated) {
            sb.append("<s>"); // NOI18N
        }
        if (isInherited) {
            sb.append("<font color=" + INHERITED_COLOR + ">"); // NOI18N
        }
        if (e.getKind() == ElementKind.CONSTRUCTOR) {
            sb.append(e.getEnclosingElement().getSimpleName());
        } else {
            sb.append(e.getSimpleName());
        }
        if (isDeprecated) {
            sb.append("</s>"); // NOI18N
        }

        sb.append("("); // NOI18N

        List<? extends VariableElement> params = e.getParameters();
        for (Iterator<? extends VariableElement> it = params.iterator(); it.hasNext();) {
            VariableElement param = it.next();
            sb.append("<font color=" + TYPE_COLOR + ">"); // NOI18N
            sb.append(print(types, param.asType()));
            sb.append("</font>"); // NOI18N
            sb.append(" "); // NOI18N
            sb.append(param.getSimpleName());
            if (it.hasNext()) {
                sb.append(", "); // NOI18N
            }
        }


        sb.append(")"); // NOI18N

        if (e.getKind() != ElementKind.CONSTRUCTOR) {
            TypeMirror rt = e.getReturnType();
            if (rt.getKind() != TypeKind.VOID) {
                sb.append(" : "); // NOI18N     
                sb.append("<font color=" + TYPE_COLOR + ">"); // NOI18N
                sb.append(print(types, e.getReturnType()));
                sb.append("</font>"); // NOI18N                    
            }
        }

        return sb.toString();
    }

    private String createHtmlHeader(VariableElement e, boolean isDeprecated, boolean isInherited, JavafxTypes types) {

        StringBuilder sb = new StringBuilder();

        if (isDeprecated) {
            sb.append("<s>"); // NOI18N
        }
        if (isInherited) {
            sb.append("<font color=" + INHERITED_COLOR + ">"); // NOI18N
        }
        sb.append(e.getSimpleName());
        if (isDeprecated) {
            sb.append("</s>"); // NOI18N
        }

        if (e.getKind() != ElementKind.ENUM_CONSTANT) {
            sb.append(" : "); // NOI18N
            sb.append("<font color=" + TYPE_COLOR + ">"); // NOI18N
            sb.append(print(types, e.asType()));
            sb.append("</font>"); // NOI18N
        }

        return sb.toString();
    }

    private String createHtmlHeader(TypeElement e, boolean isDeprecated, boolean isInherited, JavafxTypes types) {

        StringBuilder sb = new StringBuilder();
        if (isDeprecated) {
            sb.append("<s>"); // NOI18N
        }
        if (isInherited) {
            sb.append("<font color=" + INHERITED_COLOR + ">"); // NOI18N
        }
        sb.append(e.getSimpleName());
        if (isDeprecated) {
            sb.append("</s>"); // NOI18N
        }
        // sb.append(print(e.asType()));            
        List<? extends TypeParameterElement> typeParams = e.getTypeParameters();

        if (typeParams != null && !typeParams.isEmpty()) {
            sb.append("&lt;"); // NOI18N

            for (Iterator<? extends TypeParameterElement> it = typeParams.iterator(); it.hasNext();) {
                TypeParameterElement tp = it.next();
                sb.append(tp.getSimpleName());
                try { // XXX Verry ugly -> file a bug against Javac?
                    List<? extends TypeMirror> bounds = tp.getBounds();
                    if (!bounds.isEmpty()) {
                        sb.append(printBounds(bounds, types));
                    }
                } catch (NullPointerException npe) {
                    System.err.println("El " + e);
                    npe.printStackTrace();
                }
                if (it.hasNext()) {
                    sb.append(", "); // NOI18N
                }
            }

            sb.append("&gt;"); // NOI18N
        }

        // Add superclass and implemented interfaces

        TypeMirror sc = e.getSuperclass();
        String scName = print(types, sc);

        if (sc == null ||
                e.getKind() == ElementKind.ENUM ||
                e.getKind() == ElementKind.ANNOTATION_TYPE ||
                "Object".equals(scName) || // NOI18N
                "<none>".equals(scName)) { // NOI18N
            scName = null;
        }

        List<? extends TypeMirror> ifaces = e.getInterfaces();

        if ((scName != null || !ifaces.isEmpty()) &&
                e.getKind() != ElementKind.ANNOTATION_TYPE) {
            sb.append(" :: "); // NOI18N
            if (scName != null) {
                sb.append("<font color=" + TYPE_COLOR + ">"); // NOI18N                
                sb.append(scName);
                sb.append("</font>"); // NOI18N
            }
            if (!ifaces.isEmpty()) {
                if (scName != null) {
                    sb.append(" : "); // NOI18N
                }
                for (Iterator<? extends TypeMirror> it = ifaces.iterator(); it.hasNext();) {
                    TypeMirror typeMirror = it.next();
                    sb.append("<font color=" + TYPE_COLOR + ">"); // NOI18N                
                    sb.append(print(types, typeMirror));
                    sb.append("</font>"); // NOI18N
                    if (it.hasNext()) {
                        sb.append(", "); // NOI18N
                    }
                }

            }
        }

        return sb.toString();
    }

    private String printBounds(List<? extends TypeMirror> bounds, JavafxTypes types) {
        if (bounds.size() == 1 && "java.lang.Object".equals(bounds.get(0).toString())) {
            return "";
        }

        StringBuilder sb = new StringBuilder();

        sb.append(" extends "); // NOI18N

        for (Iterator<? extends TypeMirror> it = bounds.iterator(); it.hasNext();) {
            TypeMirror bound = it.next();
            sb.append(print(types, bound));
            if (it.hasNext()) {
                sb.append(" & "); // NOI18N
            }

        }

        return sb.toString();
    }

    private String print(JavafxTypes types, TypeMirror tm) {
        StringBuilder sb;

        switch (tm.getKind()) {
            case DECLARED:
                DeclaredType dt = (DeclaredType) tm;
                sb = new StringBuilder(dt.asElement().getSimpleName().toString());
                List<? extends TypeMirror> typeArgs = dt.getTypeArguments();
                if (!typeArgs.isEmpty()) {
                    sb.append("&lt;");

                    for (Iterator<? extends TypeMirror> it = typeArgs.iterator(); it.hasNext();) {
                        TypeMirror ta = it.next();
                        sb.append(print(types, ta));
                        if (it.hasNext()) {
                            sb.append(", ");
                        }
                    }
                    sb.append("&gt;");
                }

                return sb.toString();
            case TYPEVAR:
                TypeVariable tv = (TypeVariable) tm;
                sb = new StringBuilder(tv.asElement().getSimpleName().toString());
                return sb.toString();
            case ARRAY:
                ArrayType at = (ArrayType) tm;
                sb = new StringBuilder(print(types, at.getComponentType()));
                sb.append("[]");
                return sb.toString();
            case WILDCARD:
                WildcardType wt = (WildcardType) tm;
                sb = new StringBuilder("?");
                if (wt.getExtendsBound() != null) {
                    sb.append(" extends "); // NOI18N
                    sb.append(print(types, wt.getExtendsBound()));
                }
                if (wt.getSuperBound() != null) {
                    sb.append(" super "); // NOI18N
                    sb.append(print(types, wt.getSuperBound()));
                }
                return sb.toString();
            default:
                if (tm instanceof Type) {
                    // another space magic
                    return FXSourceUtils.typeToString(types, (Type) tm);
                }
                return tm.toString();
        }
    }
}
