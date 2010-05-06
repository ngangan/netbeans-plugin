/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2008-2010 Sun Microsystems, Inc. All rights reserved.
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
package org.netbeans.modules.javafx.source.indexing;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.FunctionInvocationTree;
import com.sun.javafx.api.tree.IdentifierTree;
import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.InstantiateTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.MemberSelectTree;
import com.sun.javafx.api.tree.ObjectLiteralPartTree;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.TypeClassTree;
import com.sun.javafx.api.tree.UnitTree;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.mjavac.code.Symbol;
import com.sun.tools.mjavac.code.Symbol.TypeSymbol;
import com.sun.tools.mjavac.code.Type;
import com.sun.tools.javafx.api.JavafxcTrees;
import com.sun.tools.javafx.tree.JFXIdent;
import com.sun.tools.javafx.tree.JFXTree;
import com.sun.tools.javafx.tree.JavafxTreeInfo;
import com.sun.tools.mjavac.util.Abort;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.Name;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeKind;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClassIndex.SearchKind;
import org.netbeans.api.javafx.source.ClassIndex.SearchScope;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.JavaFXSourceUtils;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.modules.javafx.source.ApiSourcePackageAccessor;
import org.netbeans.modules.parsing.spi.indexing.Context;
import org.netbeans.modules.parsing.spi.indexing.CustomIndexer;
import org.netbeans.modules.parsing.spi.indexing.CustomIndexerFactory;
import org.netbeans.modules.parsing.spi.indexing.ErrorsCache;
import org.netbeans.modules.parsing.spi.indexing.ErrorsCache.Convertor;
import org.netbeans.modules.parsing.spi.indexing.ErrorsCache.ErrorKind;
import org.netbeans.modules.parsing.spi.indexing.Indexable;
import org.netbeans.modules.parsing.spi.indexing.PathRecognizerRegistration;
import org.netbeans.modules.parsing.spi.indexing.support.IndexDocument;
import org.netbeans.modules.parsing.spi.indexing.support.IndexingSupport;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.NbBundle;

/**
 *
 * @author Jaroslav Bachorik
 */
public class JavaFXIndexer extends CustomIndexer {

    final private static java.util.logging.Logger LOG = java.util.logging.Logger.getLogger(JavaFXIndexer.class.getName());
    final private static boolean DEBUG = LOG.isLoggable(Level.FINEST);
    final public static String NAME = "fx";
    final public static int VERSION = 5;

    final private static String SYNTHETIC_CLASS_POO = "$ObjLit$";

    static final Convertor<Diagnostic<?>> DIAG_ERROR_CONVERTOR = new Convertor<Diagnostic<?>>() {
        public ErrorKind getKind(Diagnostic<?> t) {
            return t.getKind() == Diagnostic.Kind.ERROR ? ErrorKind.ERROR : ErrorKind.WARNING;
        }
        public int getLineNumber(Diagnostic<?> t) {
            return (int) t.getLineNumber();
        }
        public String getMessage(Diagnostic<?> t) {
            return t.getMessage(null);
        }
    };

    static final Convertor<Exception> EXCEPTION_ERROR_CONVERTOR = new Convertor<Exception>() {
        public ErrorKind getKind(Exception e) {
            return ErrorKind.ERROR;
        }
        public int getLineNumber(Exception t) {
            return 1;
        }
        public String getMessage(Exception e) {
            return e.getLocalizedMessage();
        }
    };

    static final Convertor<String> TEXT_ERROR_CONVERTOR = new Convertor<String>() {
        public ErrorKind getKind(String e) {
            return ErrorKind.ERROR;
        }
        public int getLineNumber(String t) {
            return 1;
        }
        public String getMessage(String txt) {
            return txt;
        }
    };

    private class IndexingVisitor extends JavaFXTreePathScanner<Void, IndexDocument> {
        private CompilationController cc;
        private FileObject fo;
        public IndexingVisitor(CompilationController cc) {
            this.cc = cc;
            this.fo = cc.getFileObject();
        }

        @Override
        public Void visitCompilationUnit(UnitTree node, IndexDocument document) {
            String indexVal = node.getPackageName() != null ? node.getPackageName().toString() : "<default>"; // NOI18N
            index(document, IndexKey.PACKAGE_NAME, indexVal);
            return super.visitCompilationUnit(node, document);
        }

        @Override
        public Void visitImport(ImportTree node, IndexDocument document) {
            Tree jfxIdent = node.getQualifiedIdentifier();
            if (jfxIdent == null) {
                if (DEBUG) {
                    LOG.log(Level.FINEST, "Error resolving import statment: {0}", node);
                }
                return super.visitImport(node, document);
            }
            Element e = null;
            while (e == null) {
                e = JavafxTreeInfo.symbolFor((JFXTree)jfxIdent);
                if (e == null && jfxIdent.getJavaFXKind() == Tree.JavaFXKind.MEMBER_SELECT) {
                    jfxIdent = ((MemberSelectTree)jfxIdent).getExpression();
                } else {
                    break;
                }
            }

            String indexVal = e != null ? jfxIdent.toString() : null;
            if (indexVal != null) {
                if (DEBUG) {
                    LOG.log(Level.FINEST, "Indexing import type reference {0}", indexVal);
                }
                index(document, IndexKey.TYPE_REF, indexVal);
            } else {
                LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node);
            }

            return super.visitImport(node, document);
        }

        @Override
        public Void visitClassDeclaration(ClassDeclarationTree node, IndexDocument document) {
            if (!node.getModifiers().getFlags().contains(Modifier.PRIVATE)) {
                Element e = cc.getTrees().getElement(getCurrentPath());
                if (e == null) {
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Error resolving element of {0}", node);
                    }
                    return super.visitClassDeclaration(node, document);
                }
                if (e.getSimpleName().toString().contains(SYNTHETIC_CLASS_POO)) {
                    // don't even try to index synthetically generated classes -
                    //   they throw exceptions on you when trying to access their inner workings
                    return super.visitClassDeclaration(node, document);
                }

                TypeElement type = (TypeElement) e;
                if (DEBUG) {
                    LOG.log(Level.FINEST, "Indexing {0}:", type.getQualifiedName());
                    LOG.log(Level.FINEST, "  Simple: {0}", node.getSimpleName());
                    LOG.log(Level.FINEST, "  Case insensitive: {0}", node.getSimpleName().toString().toLowerCase());
                }
                String fqn = type.getQualifiedName().toString();

                List<ExpressionTree> superTypes = node.getSupertypeList();
                if (superTypes != null) {
                    for (ExpressionTree et : superTypes) {
                        JavaFXTreePath tp = JavafxcTrees.getPath(getCurrentPath(), et);
                        if (tp != null) {
                            TypeElement supr = (TypeElement) cc.getTrees().getElement(tp);
                            if (supr != null) {
                                if (DEBUG) {
                                    LOG.log(Level.FINEST, "Indexing {0} as a supertype of {1}:", new Object[]{supr.getQualifiedName(), type.getQualifiedName()});
                                }
                                index(document, IndexKey.TYPE_IMPL, supr.getQualifiedName().toString());
                                index(document, IndexKey.TYPE_REF, supr.getQualifiedName().toString());
                            }
                        }
                    }
                }
                index(document, IndexKey.CLASS_NAME_SIMPLE, node.getSimpleName().toString());
                index(document, IndexKey.CLASS_NAME_INSENSITIVE, node.getSimpleName().toString().toLowerCase());
                index(document, IndexKey.CLASS_FQN, fqn);
                try {
                    if (type.getNestingKind() == NestingKind.TOP_LEVEL) {
                        int pkgLen = fqn.lastIndexOf(".");
                        if (pkgLen > -1) {
                            index(document, IndexKey.PACKAGE_NAME, fqn.substring(0, pkgLen));
                        } else {
                            index(document, IndexKey.PACKAGE_NAME, IndexingUtilities.DEFAULT_PACKAGE);
                        }
                    }
                } catch (NullPointerException npe) {
                    // #183260: javafxc throwing a NPE for certaing uncompilable sources
                    LOG.log(Level.FINE, fqn, npe);
                } catch (Abort a) {
                    LOG.log(Level.FINE, fo.getPath() + " : " + fqn, a);
                }
            }
            return super.visitClassDeclaration(node, document);
        }

        @Override
        public Void visitVariable(VariableTree node, IndexDocument document) {
            if (!node.getModifiers().getFlags().contains(Modifier.PRIVATE)) {
                VariableElement e = (VariableElement) cc.getTrees().getElement(getCurrentPath());
                if (e == null) {
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Error resolving element of {0}", node);
                    }
                    return super.visitVariable(node, document);
                }
                if (e.getKind() == ElementKind.FIELD) { // can handle only fields for now
                    ElementHandle eh = ElementHandle.create(e);
                    if (eh == null) {
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Error while processing variable: {0}\n({1})", new Object[]{node.toString(), fo.getPath()}); // NOI18N
                        }
                        return super.visitVariable(node, document);
                    }

                    String indexVal = IndexingUtilities.getIndexValue(eh);
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Indexing variable {0} as {1}\n", new String[]{node.toString(), indexVal}); // NOI18N
                    }
                    index(document, IndexKey.FIELD_DEF, indexVal);
                    indexVal = e.asType() != null ? e.asType().toString() : null;
                    if (indexVal != null) {
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Indexing variable type reference {0}\n", new String[]{indexVal}); // NOI18N
                        }
                        index(document, IndexKey.TYPE_REF, indexVal);
                    } else {
                        LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node.getInitializer());
                    }

                }
            }
            return super.visitVariable(node, document);
        }

        @Override
        public Void visitFunctionDefinition(FunctionDefinitionTree node, IndexDocument document) {
            if (!node.getModifiers().getFlags().contains(Modifier.PRIVATE)) {
                Element el = cc.getTrees().getElement(getCurrentPath());
                if (el == null) {
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Error resolving element of {0}", node);
                    }
                    return super.visitFunctionDefinition(node, document);
                }
                if (el.getKind() == ElementKind.METHOD) {
                    ExecutableElement e = (ExecutableElement) el;
                    if (e.asType() == null) {
                        return super.visitFunctionDefinition(node, document); // workaround for NPE in Symbol$MethodSymbol
                    }
                    if (e.getReturnType() != null && e.getReturnType().getKind() != TypeKind.OTHER && !e.getSimpleName().contentEquals("javafx$run$")) { // skip the synthetic "$javafx$run$" method generated for javafx scripts
                        ElementHandle eh = ElementHandle.create(e);
                        if (eh == null) {
                            if (DEBUG) {
                                LOG.log(Level.FINEST, "Error while processing function definition: {0}\n({1})", new Object[]{node.toString(), fo.getPath()}); // NOI18N
                            }
                            return super.visitFunctionDefinition(node, document);
                        }
                        String indexVal = IndexingUtilities.getIndexValue(eh);
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Indexing function definition {0} as {1}\n", new String[]{node.toString(), indexVal});
                        }
                        index(document, IndexKey.FUNCTION_DEF, indexVal);
                        indexVal = e.asType() != null ? e.asType().toString() : null;
                        if (indexVal != null) {
                            if (DEBUG) {
                                LOG.log(Level.FINEST, "Indexing function def type reference {0}\n", new String[]{indexVal});
                            }
                            index(document, IndexKey.TYPE_REF, indexVal);
                        } else {
                            LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node);
                        }
                    }
                }
            }
            return super.visitFunctionDefinition(node, document);
        }

        @Override
        public Void visitMethodInvocation(FunctionInvocationTree node, IndexDocument document) {
            Element el = cc.getTrees().getElement(getCurrentPath());
            if (el == null) {
                if (DEBUG) {
                    LOG.log(Level.FINEST, "Error resolving element of {0}", node);
                }
                return super.visitMethodInvocation(node, document);
            }
            if (el.getKind() == ElementKind.METHOD) {
                ExecutableElement e = (ExecutableElement) el;
                Collection<ExecutableElement> overridenMethods = JavaFXSourceUtils.getOverridenMethods(e, cc);
                Collection<ExecutableElement> methods = new ArrayList<ExecutableElement>();

                methods.add(e);
                methods.addAll(overridenMethods);
                for (ExecutableElement ee : methods) {
                    ElementHandle eh = ElementHandle.create(ee);
                    if (eh == null) {
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Error while processing method invocation: {0}\n({1})", new Object[]{node.toString(), fo.getPath()}); // NOI18N
                        }
                        return super.visitMethodInvocation(node, document);
                    }
                    String indexVal = IndexingUtilities.getIndexValue(eh);
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Indexing method invocation {0} as {1}\n", new String[]{node.toString(), indexVal});
                    }
                    index(document, IndexKey.FUNCTION_INV, indexVal);
                    indexVal = e.getEnclosingElement() != null ? IndexingUtilities.getIndexValue(ElementHandle.create(e.getEnclosingElement())) : null;
                    if (indexVal != null) {
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Indexing function inv owner type reference {0}\n", new String[]{indexVal});
                        }
                        index(document, IndexKey.TYPE_REF, indexVal);
                    } else {
                        LOG.log(Level.FINE, "Can not determine function owner inv type for: {0}", node != null ? node.getJavaFXKind() : "null");
                    }
                    indexVal = e.asType() != null ? e.asType().toString() : null;
                    if (indexVal != null) {
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Indexing function inv return type reference {0}\n", new String[]{indexVal});
                        }
                        index(document, IndexKey.TYPE_REF, indexVal);
                    } else {
                        LOG.log(Level.FINE, "Can not determine function inv return type for: {0}", node != null ? node.getJavaFXKind() : "null");
                    }
                }
            }
            return super.visitMethodInvocation(node, document);
        }

        @Override
        public Void visitTypeClass(TypeClassTree node, IndexDocument document) {
            Element el = cc.getTrees().getElement(getCurrentPath());
            if (el == null) {
                if (DEBUG) {
                    LOG.log(Level.FINEST, "Error resolving element of {0}", node);
                }
                return super.visitTypeClass(node, document);
            }
            if (el.getKind().isClass() || el.getKind().isInterface()) {
                ElementHandle eh = ElementHandle.create(el);
                if (eh == null) {
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Error while processing type class: {0}\n({1})", new Object[]{node.toString(), fo.getPath()}); // NOI18N
                    }
                    return super.visitTypeClass(node, document);
                }
                String indexVal = IndexingUtilities.getIndexValue(eh);
                if (indexVal != null) {
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Indexing type reference {0} as {1}\n", new String[]{node.toString(), indexVal});
                    }
                    index(document, IndexKey.TYPE_REF, indexVal);
                } else {
                    LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node);
                }
            }
            return super.visitTypeClass(node, document);
        }

        @Override
        public Void visitMemberSelect(MemberSelectTree node, IndexDocument document) {
            ExpressionTree expression = node.getExpression();
            if (expression instanceof JFXIdent) {
                Name memberName = node.getIdentifier();
                Type type = ((JFXIdent) expression).type;
                if (type == null) {
                    return super.visitMemberSelect(node, document);
                }
                TypeSymbol ts = type.asElement();
                if (ts == null) {
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Error resolving element of {0}", node);
                    }
                    return super.visitMemberSelect(node, document);
                }
                if (!ts.getKind().isClass() && !ts.getKind().isInterface()) {
                    return super.visitMemberSelect(node, document);
                }
                /**
                 * Workaround for NPEs thrown from the javac when calling ts.getEnclosedElements()
                 * The NPE is the result of the source not being compilable
                 *
                 * Unfortunately, we can not check for compilation errors before indexing because due to http://javafx-jira.kenai.com/browse/JFXC-3468
                 * many compilable sources are falsly marked as non-compilable
                 *
                 * Just ignore the exception - nothing else to do than may be log it
                 */
                try {
                    for (Symbol sy : ts.getEnclosedElements()) {
                        if (sy.getKind() == ElementKind.FIELD) {
                            if (sy.getSimpleName().equals(memberName)) {
                                ElementHandle eh = ElementHandle.create(sy);
                                if (eh == null) {
                                    if (DEBUG) {
                                        LOG.log(Level.FINEST, "Error while processing member select: {0}\n({1})", new Object[]{node.toString(), fo.getPath()}); // NOI18N
                                    }
                                    return super.visitMemberSelect(node, document);
                                }
                                String indexVal = IndexingUtilities.getIndexValue(ElementHandle.create(ts));
                                if (indexVal != null) {
                                    if (DEBUG) {
                                        LOG.log(Level.FINEST, "Indexing type reference {0} as {1}\n", new String[]{node.toString(), indexVal});
                                    }
                                    index(document, IndexKey.TYPE_REF, indexVal);
                                } else {
                                    LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node.getExpression());
                                }
                                indexVal = IndexingUtilities.getIndexValue(ElementHandle.create(sy));
                                if (indexVal != null) {
                                    if (DEBUG) {
                                        LOG.log(Level.FINEST, "Indexing field reference {0} as {1}\n", new String[]{node.toString(), indexVal});
                                    }
                                    index(document, IndexKey.FIELD_REF, indexVal);
                                } else {
                                    LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node);
                                }
                            }
                        }
                    }
                } catch (NullPointerException e) {
                    LOG.log(Level.INFO, "Trying to index non-compilable file {0}. Giving up.", fo.getPath());
                }
            }

            return super.visitMemberSelect(node, document);
        }

        @Override
        public Void visitObjectLiteralPart(ObjectLiteralPartTree node, IndexDocument document) {
            Element el = cc.getTrees().getElement(getCurrentPath());
            if (el == null) {
                if (DEBUG) {
                    LOG.log(Level.FINEST, "Error resolving element of {0}", node);
                }
                return super.visitObjectLiteralPart(node, document);
            }
            switch (el.getKind()) {
                case FIELD: {
                    ElementHandle eh = ElementHandle.create(el);
                    if (eh == null) {
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Error while processing object literal part: {0}\n({1})", new Object[]{node.toString(), fo.getPath()}); // NOI18N
                        }
                        return super.visitObjectLiteralPart(node, document);
                    }
                    String indexVal = IndexingUtilities.getIndexValue(eh);
                    if (indexVal != null) {
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Indexing field reference {0} as {1}\n", new String[]{node.toString(), indexVal});
                        }
                        index(document, IndexKey.FIELD_REF, indexVal);
                    } else {
                        LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node);
                    }
                }
            }
            return super.visitObjectLiteralPart(node, document);
        }

        @Override
        public Void visitIdentifier(IdentifierTree node, IndexDocument document) {
            Element el = cc.getTrees().getElement(getCurrentPath());
            if (el == null) {
                return super.visitIdentifier(node, document);
            }
            switch (el.getKind()) {
                case FIELD: {
                    ElementHandle eh = ElementHandle.create(el);
                    if (eh == null) {
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Error while processing identifier: {0}\n({1})", new Object[]{node.toString(), fo.getPath()}); // NOI18N
                        }
                        return super.visitIdentifier(node, document);
                    }
                    String indexVal = IndexingUtilities.getIndexValue(eh);
                    if (indexVal != null) {
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Indexing field reference {0} as {1}\n", new String[]{node.toString(), indexVal});
                        }
                        index(document, IndexKey.TYPE_REF, indexVal);
                    } else {
                        LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node);
                    }
                    break;
                }
            }
            return super.visitIdentifier(node, document);
        }

        @Override
        public Void visitInstantiate(InstantiateTree node, IndexDocument document) {
            Element el = cc.getTrees().getElement(JavafxcTrees.getPath(getCurrentPath(), node.getIdentifier()));
            if (el == null) {
                if (DEBUG) {
                    LOG.log(Level.FINEST, "Error resolving element of {0}", node);
                }
                return super.visitInstantiate(node, document);
            }
            if (el.getKind().isClass() || el.getKind().isInterface()) {
                ElementHandle eh = ElementHandle.create(el);
                if (eh == null) {
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Error while processing instantiation: {0}\n({1})", new Object[]{node.toString(), fo.getPath()}); // NOI18N
                    }
                    return super.visitInstantiate(node, document);
                }
                String indexVal = IndexingUtilities.getIndexValue(ElementHandle.create(el));
                if (indexVal != null) {
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Indexing type reference {0} as {1}\n", new String[]{node.toString(), indexVal});
                    }
                    index(document, IndexKey.TYPE_REF, indexVal);
                } else {
                    LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node);
                }
            }
            return super.visitInstantiate(node, document);
        }
    };

    public enum IndexKey {

        PACKAGE_NAME,
        CLASS_FQN, CLASS_NAME_SIMPLE, CLASS_NAME_INSENSITIVE,
        FUNCTION_DEF, FUNCTION_INV, FIELD_DEF, FIELD_REF,
        TYPE_REF, TYPE_IMPL,
        NOT_INDEXED
    }

    // <editor-fold defaultstate="collapsed" desc="Indexer Factory">
    @PathRecognizerRegistration(sourcePathIds={ClasspathInfo.FX_SOURCE}, mimeTypes={"text/x-fx"})
    public static class Factory extends CustomIndexerFactory {
        private static AtomicBoolean javafxTaskFactoriesInitialized = new AtomicBoolean(false);

        public Factory() {
            if (!javafxTaskFactoriesInitialized.getAndSet(true)) {
                ApiSourcePackageAccessor.get().registerSourceTaskFactoryManager();
            }
        }

        @Override
        public CustomIndexer createIndexer() {
            return new JavaFXIndexer();
        }

        @Override
        public boolean supportsEmbeddedIndexers() {
            return true;
        }

        @Override
        public void filesDeleted(Iterable<? extends Indexable> itrbl, Context cntxt) {
            for (Indexable ixbl : itrbl) {
                try {
                    IndexingSupport.getInstance(cntxt).removeDocuments(ixbl);
                } catch (IOException e) {
                    LOG.log(Level.WARNING, null, e);
                }
            }
        }

        @Override
        public void filesDirty(Iterable<? extends Indexable> itrbl, Context cntxt) {
            for (Indexable ixbl : itrbl) {
                try {
                    IndexingSupport.getInstance(cntxt).markDirtyDocuments(ixbl);
                } catch (IOException e) {
                    LOG.log(Level.WARNING, null, e);
                }
            }
        }

        @Override
        public int getIndexVersion() {
            return VERSION;
        }

        @Override
        public String getIndexerName() {
            return NAME;
        }
    }// </editor-fold>

    final private AtomicBoolean cancelled = new AtomicBoolean();

    @Override
    protected void index(Iterable<? extends Indexable> itrbl, final Context cntxt) {
        if (!JavaFXSourceUtils.isPlatformOk(cntxt.getRoot())) return; // don't try to index files in a project with broken javafx platform
        
        final Map<FileObject, Indexable> indexables = new HashMap<FileObject, Indexable>();

        cancelled.set(false);

        Iterator<? extends Indexable> iterator = itrbl.iterator();
        while(iterator.hasNext()) {
            try {
                Indexable ix = iterator.next();
                URI uri = ix.getURL().toURI();
                File f = new File(uri.normalize());
                FileObject fo = FileUtil.toFileObject(f);
                indexables.put(fo, ix);
            } catch (URISyntaxException e) {
            }
        }

        if (cntxt.getRoot() != null) {
            ClasspathInfo cpInfo = ClasspathInfo.create(cntxt.getRoot());
            final ClassIndex ci = cpInfo.getClassIndex();
            JavaFXSource jfxs = JavaFXSource.create(cpInfo, indexables.keySet());
            if (jfxs == null) return; // #185591: Can happen when indexing kicks in in the middle of deleting a project
            try {
                jfxs.runUserActionTask(new CancellableTask<CompilationController>() {
                    private List<Diagnostic<? extends JavaFileObject>> getDiagnostics(CompilationController cc) {
                        List<Diagnostic<? extends JavaFileObject>> dg = new ArrayList<Diagnostic<? extends JavaFileObject>>();
                        for(Diagnostic d : cc.getDiagnostics()) {
                            dg.add(d);
                        }
                        return dg;
                    }

                    public void cancel() {
                        cancelled.set(true);
                    }

                    public void run(CompilationController cc) throws Exception {
                        Indexable ix = indexables.get(cc.getFileObject());
                        if (cc.toPhase(JavaFXSource.Phase.ANALYZED).lessThan(JavaFXSource.Phase.ANALYZED)) {
                            Project p = FileOwnerQuery.getOwner(cc.getFileObject());
                            ErrorsCache.setErrors(
                                cntxt.getRootURI(), ix,
                                Collections.singleton(NbBundle.getMessage(JavaFXIndexer.class, "TEXT_BrokenPlatform",
                                    ProjectUtils.getInformation(p).getDisplayName())
                                ),
                                TEXT_ERROR_CONVERTOR
                            ); // NOI18N
                        } else {
                            try {
                                if (ix != null) {
                                    ErrorsCache.setErrors(cntxt.getRootURI(), ix, getDiagnostics(cc), DIAG_ERROR_CONVERTOR);
                                }
                                if (!cntxt.isSupplementaryFilesIndexing()) {
                                    IndexingSupport support = IndexingSupport.getInstance(cntxt);
                                    IndexDocument document = support.createDocument(cc.getFileObject());
                                    IndexingVisitor visitor = new IndexingVisitor(cc);
                                    visitor.scan(cc.getCompilationUnit(), document);
                                    support.addDocument(document);
                                    for(TypeElement tte : cc.getTopLevelElements()) {
                                        if (cancelled.get()) return;
                                        reindexDependables(
                                            ci.getResources(ElementHandle.create(tte), EnumSet.of(SearchKind.TYPE_REFERENCES), EnumSet.allOf(SearchScope.class)),
                                            cntxt
                                        );
                                    }
                                }
                            } catch (Exception e) {
                                ErrorsCache.setErrors(cntxt.getRootURI(), ix, Collections.singleton(e), EXCEPTION_ERROR_CONVERTOR);
                            }
                        }
                    }
                }, false);
            } catch (IOException e) {
                LOG.log(Level.WARNING, "Error indexing", e);
            }
        }
    }

    private void reindexDependables(Set<FileObject> dependables, Context cntxt) throws IOException {
        Map<URL, Collection<URL>> supplementary = new HashMap<URL, Collection<URL>>();
        for(FileObject fo : dependables) {
            if (cancelled.get()) return;
            URL root = getSrcRoot(fo);
            if (root != null) {
                Collection<URL> files = supplementary.get(root);
                if (files == null) {
                    files = new LinkedList<URL>();
                    supplementary.put(root, files);
                }
                files.add(fo.getURL());
            }
        }

        for(Map.Entry<URL, Collection<URL>> entry : supplementary.entrySet()) {
            if (cancelled.get()) return;
            cntxt.addSupplementaryFiles(entry.getKey(), entry.getValue());
        }
    }

    private URL getSrcRoot(FileObject fo) throws IOException {
        if (fo != null) {
            ClassPath cp = ClassPath.getClassPath(fo, ClassPath.SOURCE);
            if (cp != null) {
                FileObject root = cp.findOwnerRoot(fo);
                return root.getURL();
            }
        }
        return null;
    }

    private void index(IndexDocument document, IndexKey key, String value) {
        document.addPair(key.toString(), value, true, true);
    }
}
