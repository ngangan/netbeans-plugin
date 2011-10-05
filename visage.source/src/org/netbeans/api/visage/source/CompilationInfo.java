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
package org.netbeans.api.visage.source;

import com.sun.source.tree.CompilationUnitTree;
import org.netbeans.modules.visage.source.CompilationInfoImpl;
import com.sun.tools.mjavac.code.Symbol;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;
import javax.lang.model.util.Types;
import javax.swing.text.Document;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import org.netbeans.api.visage.source.VisageSource.Phase;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.modules.parsing.api.Snapshot;
import org.openide.filesystems.FileObject;
import org.visage.api.tree.Tree;
import org.visage.api.tree.UnitTree;
import org.visage.api.tree.VisageTreePath;
import org.visage.api.tree.VisageTreeScanner;
import org.visage.tools.api.VisagecTrees;
import org.visage.tools.code.VisageTypes;
import org.visage.tools.comp.VisageEnter;
import org.visage.tools.comp.VisageEnv;
import org.visage.tools.tree.VisageClassDeclaration;
import org.visage.tools.tree.VisageFunctionDefinition;
import org.visage.tools.tree.VisageScript;
import org.visage.tools.tree.VisageTree;
import org.visage.tools.tree.VisageVar;

/**
 *
 * @author nenik
 */
public class CompilationInfo {

    private final CompilationInfoImpl impl;
    private ElementUtilities elementUtilities;
    private TreeUtilities treeUtilities;

    public ElementUtilities getElementUtilities() {
        if (null == elementUtilities) {
            elementUtilities = new ElementUtilities(this);
        }
        return elementUtilities;
    }

    public Snapshot getSnapshot() {
        return impl.getSnapshot();
    }

    public FileObject getFileObject() {
        return getSnapshot().getSource().getFileObject();
    }

    public Document getDocument() {
        return getSnapshot().getSource().getDocument(true);
    }

    public CharSequence getText() {
        return impl().getSnapshot().getText();
    }

    public TreeUtilities getTreeUtilities() {
        if (null == treeUtilities) {
            treeUtilities = new TreeUtilities(this);
        }
        return treeUtilities;
    }

    public CompilationInfo(CompilationInfoImpl impl) {
        this.impl = impl;
    }

    public VisageSource.Phase getPhase() {
        return impl.getPhase();
    }
    
    public VisageSource.Phase moveToPhase(Phase phase) throws IOException {
        return impl.toPhase(phase);
    }
    
    /**
     * Return the {@link com.sun.tools.visage.api.VisagecTrees} service of the visagec represented by this {@link CompilationInfo}.
     * @return visagec Trees service
     */
    public VisagecTrees getTrees() {
        return VisagecTrees.instance(impl.getVisagecTaskImpl());
    }

    public boolean isErrors() {
        return impl.isErrors();
    }

    // XXX: hack around lack of support in compiler
    public VisageTreePath getPath(Element e) {
        VisageTree tree = (VisageTree)getTree(e);
        return tree == null ? null : getTrees().getPath(getCompilationUnit(), tree);
    }

    public Tree getTree(Element e) {
        Symbol sym = (Symbol) e;
        VisageEnter enter = VisageEnter.instance(impl.getContext());
        VisageEnv env = enter.getEnv(sym.enclClass());
        if (env == null) {
            return null;
        }
        return declarationFor(sym, env.tree);
    }

    private static VisageTree declarationFor(final Symbol sym, final VisageTree tree) {

        class DeclScanner extends VisageTreeScanner {

            VisageTree result = null;

/*            public void scan(VisageTree tree) {
                if (tree != null && result == null) {
                    tree.accept(this,sym);
                }
            }
*/
/*            
            public void visitScript( VisageScript that) {
                if (that.packge == sym) {
                    result = that;
                } else {
                    super.visitScript(that);
                }
            }
*/
            public void visitClassDeclaration( VisageClassDeclaration that) {
                if (that.sym == sym) {
                    result = that;
                } else {
                    super.visitClassDeclaration(that,sym);
                }
            }

            public void visitFunctionDefinition( VisageFunctionDefinition that) {
                if (that.sym == sym) {
                    result = that;
                } else {
                    super.visitFunctionDefinition(that,sym);
                }
            }

/*
            public void visitVariable( VisageVar that) {
                if (that.sym == sym) {
                    result = that;
                } else {
                    super.visitVariable(that);
                }
            }
*/

        }
        DeclScanner s = new DeclScanner();
        tree.accept(s, sym);
        return s.result;
    }

    public Types getTypes() {
        return impl.getVisagecTaskImpl().getTypes();
    }

    public VisageTypes getVisageTypes() {
        return VisageTypes.instance(impl.getContext());
    }

    public Elements getElements() {
        return impl.getVisagecTaskImpl().getElements();
    }

    /**
     * Returns the visagec tree representing the source file.
     * @return {@link CompilationUnitTree} the compilation unit cantaining the top level classes contained in the,
     * visage source file.
     * 
     * @throws java.lang.IllegalStateException  when the phase is less than {@link VisageSource.Phase#PARSED}
     */
    public UnitTree getCompilationUnit() {
        return impl.getCompilationUnit();
    }

    public ClasspathInfo getClasspathInfo() {
        return impl.getClasspathInfo();
    }
    
    public Iterable <? extends JavaFileObject> getClassBytes() {
        return impl.getClassBytes();
    }

    public TokenHierarchy getTokenHierarchy() {
        return impl.getTokenHierarchy();
    }

    public List<Diagnostic> getDiagnostics() {
        return this.impl.getDiagnostics();
    }

    public List<? extends TypeElement> getTopLevelElements() {
//        checkConfinement();
//        if (this.impl.getPositionConverter() == null) {
//            throw new IllegalStateException ();
//        }
        final List<TypeElement> result = new ArrayList<TypeElement>();
//        final JavaSource javaSource = this.impl.getJavaSource();
//        if (javaSource.isClassFile()) {
//            Elements elements = getElements();
//            assert elements != null;
//            assert javaSource.rootFo != null;
//            String name = FileObjects.convertFolder2Package(FileObjects.stripExtension(FileUtil.getRelativePath(javaSource.rootFo, getFileObject())));
//            TypeElement e = ((JavacElements)elements).getTypeElementByBinaryName(name);
//            if (e != null) {                
//                result.add (e);
//            }
//        }
//        else {
        UnitTree cu = getCompilationUnit();
        if (cu == null) {
            return null;
        }

        final VisagecTrees trees = getTrees();
        assert trees != null;
        List<? extends Tree> typeDecls = cu.getTypeDecls();
        VisageTreePath cuPath = new VisageTreePath(cu);
        for (Tree t : typeDecls) {
            if (t == null) {
                continue;
            }
            VisageTreePath p = new VisageTreePath(cuPath, t);
            Element e = trees.getElement(p);
            if (e != null && (e.getKind().isClass() || e.getKind().isInterface())) {
                result.add((TypeElement) e);
            }
        }
//        }
        return Collections.unmodifiableList(result);
    }

    CompilationInfoImpl impl() {
        return impl;
    }

}
