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
package org.netbeans.api.javafx.source;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.UnitTree;
import com.sun.tools.javafx.api.JavafxcTrees;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.Elements;
import javax.lang.model.util.Types;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.queries.SourceForBinaryQuery;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.lexer.TokenSequence;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;

/**
 *
 * @author answer
 */
public class JavaFXSourceUtils {

    public static TokenSequence<JFXTokenId> getJavaTokenSequence(final TokenHierarchy hierarchy, final int offset) {
        if (hierarchy != null) {
            TokenSequence<?> ts = hierarchy.tokenSequence();
            while(ts != null && (offset == 0 || ts.moveNext())) {
                ts.move(offset);
                if (ts.language() == JFXTokenId.language())
                    return (TokenSequence<JFXTokenId>)ts;
                if (!ts.moveNext() && !ts.movePrevious())
                    return null;
                ts = ts.embedded();
            }
        }
        return null;
    }

    public static boolean isJavaFXApplet(final FileObject file) {
        if (file == null) {
            return false;
        }

        JavaFXSource js = JavaFXSource.forFileObject(file);
        if (js == null) {
            return false;
        }
        final boolean[] result = new boolean[]{false};
        try {
            js.runUserActionTask(new CancellableTask<CompilationController>() {

                public void run(CompilationController control) throws Exception {
                    if (!control.toPhase(JavaFXSource.Phase.ANALYZED).lessThan(JavaFXSource.Phase.ANALYZED)) {

                        Elements elements = control.getElements();
                        JavafxcTrees trees = control.getTrees();
                        Types types = control.getTypes();
                        TypeElement fxapplet = elements.getTypeElement("javafx.ui.Applet");     //NOI18N
                        TypeElement applet = elements.getTypeElement("java.applet.Applet");     //NOI18N
                        TypeElement japplet = elements.getTypeElement("javax.swing.JApplet");   //NOI18N
                        UnitTree cu = control.getCompilationUnit();
                        List<? extends Tree> topLevels = cu.getTypeDecls();
                        for (Tree topLevel : topLevels) {
                            if (topLevel.getJavaFXKind() == Tree.JavaFXKind.CLASS_DECLARATION) {
                                TypeElement type = (TypeElement) trees.getElement(JavaFXTreePath.getPath(cu, topLevel));
                                if (type != null) {
                                    Set<Modifier> modifiers = type.getModifiers();
                                    if (modifiers.contains(Modifier.PUBLIC) &&
                                            ((applet != null && types.isSubtype(type.asType(), applet.asType())) || (fxapplet != null && types.isSubtype(type.asType(), fxapplet.asType())) || (japplet != null && types.isSubtype(type.asType(), japplet.asType())))) {
                                        result[0] = true;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }

                public void cancel() {
                }
            }, true);
        } catch (IOException ioe) {
            Exceptions.printStackTrace(ioe);
        }
        return result[0];

    }

    public static TypeElement getEnclosingTypeElement(Element element) throws IllegalArgumentException {
        Element param = element;

        if (element.getKind() == ElementKind.PACKAGE) {
            throw new IllegalArgumentException();
        }

        if (element.getEnclosingElement().getKind() == ElementKind.PACKAGE) {
            //element is a top level class, returning null according to the contract:
            return null;
        }

        while (element != null && !(element.getEnclosingElement().getKind().isClass() ||
                element.getEnclosingElement().getKind().isInterface())) {
            element = element.getEnclosingElement();
        }

        if (element == null) {
            //#130505:
            StringBuilder sb = new StringBuilder();

            while (param != null) {
                sb.append(param.getKind());
                sb.append(':'); // NOI18N
                sb.append(param.toString());
                sb.append('/'); // NOI18N
                param = param.getEnclosingElement();
            }

            NullPointerException npe = new NullPointerException();

            throw Exceptions.attachMessage(npe, sb.toString());
        }

        return (TypeElement) element.getEnclosingElement(); // Wrong
    }

    public static TypeElement getOutermostEnclosingTypeElement( Element element ) {

	Element ec =  getEnclosingTypeElement( element );
	if (ec == null) {
	    ec = element;
	}

	while( ec.getEnclosingElement().getKind().isClass() ||
	       ec.getEnclosingElement().getKind().isInterface() ) {

	    ec = ec.getEnclosingElement();
	}

	return (TypeElement)ec;
    }

    public static FileObject getFile(Element element, final ClasspathInfo cpInfo) {
        final ElementHandle<? extends Element> handle = ElementHandle.create(element);
        return getFile(handle, cpInfo);
    }
    
    public static FileObject getFile(final ElementHandle<? extends Element> handle, final ClasspathInfo cpInfo) {
        if (handle == null || cpInfo == null) {
            throw new IllegalArgumentException("Cannot pass null as an argument of the JavaFXSourceUtils.getFile");  //NOI18N
        }

        try {
            String[] signature = handle.getSignatures();
            assert signature.length >= 1;

            String baseCls = signature[0];
            int dollar = baseCls.indexOf('$'); // NOI18N
            if (dollar >= 0) baseCls = baseCls.substring(0, dollar);
            String name = baseCls.replace('.', '/') + ".fx"; // NOI18N

            ClassPath[] all = new ClassPath[]{
                cpInfo.getClassPath(ClasspathInfo.PathKind.BOOT),
                cpInfo.getClassPath(ClasspathInfo.PathKind.COMPILE),
                cpInfo.getClassPath(ClasspathInfo.PathKind.SOURCE)
            };

            for (ClassPath cp : all) { // cp never null
                for (FileObject binRoot : cp.getRoots()) {
                    FileObject fo = binRoot.getFileObject(name);
                    if (fo != null) {
                        return fo;
                    }
                    SourceForBinaryQuery.Result res = SourceForBinaryQuery.findSourceRoots(binRoot.getURL());
                    for (FileObject srcRoot : res.getRoots()) {
                        fo = srcRoot.getFileObject(name);
                        if (fo != null) {
                            return fo;
                        }
                    }
                }
            }

        } catch (IOException e) {
            Exceptions.printStackTrace(e);
        }
        return null;
    }

    public static Collection<ElementHandle<TypeElement>> getMainClasses(FileObject fo) {
        JavaFXSource jfxs = JavaFXSource.forFileObject(fo);
        final Collection<ElementHandle<TypeElement>> result = new HashSet<ElementHandle<TypeElement>>();
        if (jfxs != null) {
            try {
                jfxs.runUserActionTask(new Task<CompilationController>() {

                    public void run(final CompilationController cc) throws Exception {
                        new JavaFXTreePathScanner<Void, Collection<ElementHandle<TypeElement>>>() {

                            @Override
                            public Void visitClassDeclaration(ClassDeclarationTree node, Collection<ElementHandle<TypeElement>> p) {
                                TypeElement te = (TypeElement) cc.getTrees().getElement(getCurrentPath());
                                if (te.getNestingKind() == NestingKind.TOP_LEVEL) {
                                    p.add(ElementHandle.create(te));
                                }
                                return super.visitClassDeclaration(node, p);
                            }
                        }.scan(cc.getCompilationUnit(), result);
                    }
                }, true);
            } catch (IOException iOException) {
                Exceptions.printStackTrace(iOException);
            }
        }
        return result;
    }

    public static Collection<ElementHandle<TypeElement>> getClasses(FileObject fo) {
        return getClasses(fo, null);
    }

    public static Collection<ElementHandle<TypeElement>> getClasses(FileObject fo, final ElementHandle<TypeElement> superTypeHandle) {
        JavaFXSource jfxs = JavaFXSource.forFileObject(fo);
        final Collection<ElementHandle<TypeElement>> result = new HashSet<ElementHandle<TypeElement>>();
        if (jfxs != null) {
            try {
                jfxs.runUserActionTask(new Task<CompilationController>() {

                    public void run(final CompilationController cc) throws Exception {
                        final String superTypeQN = superTypeHandle != null ? superTypeHandle.getQualifiedName() : null;
                        new JavaFXTreePathScanner<Void, Collection<ElementHandle<TypeElement>>>() {

                            @Override
                            public Void visitClassDeclaration(ClassDeclarationTree node, Collection<ElementHandle<TypeElement>> p) {
                                TypeElement te = (TypeElement) cc.getTrees().getElement(getCurrentPath());
                                if (superTypeQN != null) {
                                    if (te.getSuperclass().toString().equals(superTypeQN)) {
                                        p.add(ElementHandle.create(te));
                                    } else {
                                        for(TypeMirror tm : te.getInterfaces()) {
                                            if (tm.toString().equals(superTypeQN)) {
                                                p.add(ElementHandle.create(te));
                                            }
                                        }
                                    }
                                } else {
                                    p.add(ElementHandle.create(te));
                                }
                                return super.visitClassDeclaration(node, p);
                            }
                        }.scan(cc.getCompilationUnit(), result);
                    }
                }, true);
            } catch (IOException iOException) {
                Exceptions.printStackTrace(iOException);
            }
        }
        return result;
    }
}
