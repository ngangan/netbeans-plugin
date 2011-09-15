/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2008-2010 Oracle and/or its affiliates. All rights reserved.
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
package org.netbeans.modules.visage.source.indexing;

import com.sun.visage.api.JavafxcTask;
import com.sun.visage.api.tree.ClassDeclarationTree;
import com.sun.visage.api.tree.ExpressionTree;
import com.sun.visage.api.tree.FunctionDefinitionTree;
import com.sun.visage.api.tree.FunctionInvocationTree;
import com.sun.visage.api.tree.IdentifierTree;
import com.sun.visage.api.tree.ImportTree;
import com.sun.visage.api.tree.InstantiateTree;
import com.sun.visage.api.tree.VisageTreePath;
import com.sun.visage.api.tree.VisageTreePathScanner;
import com.sun.visage.api.tree.MemberSelectTree;
import com.sun.visage.api.tree.ObjectLiteralPartTree;
import com.sun.visage.api.tree.Tree;
import com.sun.visage.api.tree.TypeClassTree;
import com.sun.visage.api.tree.UnitTree;
import com.sun.visage.api.tree.VariableTree;
import com.sun.tools.visage.api.JavafxcTaskImpl;
import com.sun.tools.visage.api.JavafxcTool;
import com.sun.tools.mjavac.code.Symbol;
import com.sun.tools.mjavac.code.Symbol.TypeSymbol;
import com.sun.tools.mjavac.code.Type;
import com.sun.tools.visage.api.JavafxcTrees;
import com.sun.tools.visage.tree.VSGIdent;
import com.sun.tools.visage.tree.VSGTree;
import com.sun.tools.visage.tree.JavafxTreeInfo;
import com.sun.tools.mjavac.util.Abort;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
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
import javax.lang.model.util.Elements;
import javax.tools.Diagnostic;
import javax.tools.DiagnosticListener;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.StandardLocation;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.visage.source.ClassIndex;
import org.netbeans.api.visage.source.ClassIndex.SearchKind;
import org.netbeans.api.visage.source.ClassIndex.SearchScope;
import org.netbeans.api.visage.source.ClasspathInfo;
import org.netbeans.api.visage.source.ElementHandle;
import org.netbeans.api.visage.source.VisageSourceUtils;
import org.netbeans.modules.visage.source.ApiSourcePackageAccessor;
import org.netbeans.modules.visage.source.classpath.SourceFileObject;
import org.netbeans.modules.visage.source.parsing.VisageParser;
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

/**
 *
 * @author Jaroslav Bachorik
 */
public class VisageIndexer extends CustomIndexer {

    final private static java.util.logging.Logger LOG = java.util.logging.Logger.getLogger(VisageIndexer.class.getName());
    final private static boolean DEBUG = LOG.isLoggable(Level.FINEST);
    final public static String NAME = "fx";
    final public static int VERSION = 6;

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

    private class IndexingVisitor extends VisageTreePathScanner<Void, IndexDocument> {
        final private JavafxcTrees trees;
        final private Elements elements;
        final private FileObject fo;
        public IndexingVisitor(JavafxcTrees trees, Elements elements, FileObject src) {
            this.trees = trees;
            this.elements = elements;
            this.fo = src;
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
                    LOG.log(Level.FINEST, "Error resolving import statment: {0}", node); // NOI18N
                }
                return super.visitImport(node, document);
            }
            Element e = null;
            while (e == null) {
                e = JavafxTreeInfo.symbolFor((VSGTree)jfxIdent);
                if (e == null && jfxIdent.getVisageKind() == Tree.VisageKind.MEMBER_SELECT) {
                    jfxIdent = ((MemberSelectTree)jfxIdent).getExpression();
                } else {
                    break;
                }
            }

            String indexVal = e != null ? jfxIdent.toString() : null;
            if (indexVal != null) {
                if (DEBUG) {
                    LOG.log(Level.FINEST, "Indexing import type reference {0}", indexVal); //NOI18N
                }
                index(document, IndexKey.TYPE_REF, indexVal);
            } else {
                LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node); //NOI18N
            }

            return super.visitImport(node, document);
        }

        @Override
        public Void visitClassDeclaration(ClassDeclarationTree node, IndexDocument document) {
            if (!node.getModifiers().getFlags().contains(Modifier.PRIVATE)) {
                Element e = trees.getElement(getCurrentPath());
                if (e == null) {
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Error resolving element of {0}", node); //NOI18N
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
                    LOG.log(Level.FINEST, "Indexing {0}:", type.getQualifiedName()); //NOI18N
                    LOG.log(Level.FINEST, "  Simple: {0}", node.getSimpleName()); //NOI18N
                    LOG.log(Level.FINEST, "  Case insensitive: {0}", node.getSimpleName().toString().toLowerCase()); //NOI18N
                }
                String fqn = type.getQualifiedName().toString();

                List<ExpressionTree> superTypes = node.getSupertypeList();
                if (superTypes != null) {
                    for (ExpressionTree et : superTypes) {
                        VisageTreePath tp = JavafxcTrees.getPath(getCurrentPath(), et);
                        if (tp != null) {
                            TypeElement supr = (TypeElement) trees.getElement(tp);
                            if (supr != null) {
                                if (DEBUG) {
                                    LOG.log(Level.FINEST, "Indexing {0} as a supertype of {1}:", new Object[]{supr.getQualifiedName(), type.getQualifiedName()}); //NOI18N
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
                    // #183260: visagec throwing a NPE for certaing uncompilable sources
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
                VariableElement e = (VariableElement) trees.getElement(getCurrentPath());
                if (e == null) {
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Error resolving element of {0}", node); //NOI18N
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
                        LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node.getInitializer()); //NOI18N
                    }

                }
            }
            return super.visitVariable(node, document);
        }

        @Override
        public Void visitFunctionDefinition(FunctionDefinitionTree node, IndexDocument document) {
            if (!node.getModifiers().getFlags().contains(Modifier.PRIVATE)) {
                Element el = trees.getElement(getCurrentPath());
                if (el == null) {
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Error resolving element of {0}", node); //NOI18N
                    }
                    return super.visitFunctionDefinition(node, document);
                }
                if (el.getKind() == ElementKind.METHOD) {
                    ExecutableElement e = (ExecutableElement) el;
                    if (e.asType() == null) {
                        return super.visitFunctionDefinition(node, document); // workaround for NPE in Symbol$MethodSymbol
                    }
                    // skip the synthetic "$visage$run$" method generated for visage scripts
                    if (e.getReturnType() != null && e.getReturnType().getKind() != TypeKind.OTHER && !e.getSimpleName().contentEquals("visage$run$")) { //NOI18N
                        ElementHandle eh = ElementHandle.create(e);
                        if (eh == null) {
                            if (DEBUG) {
                                LOG.log(Level.FINEST, "Error while processing function definition: {0}\n({1})", new Object[]{node.toString(), fo.getPath()}); // NOI18N
                            }
                            return super.visitFunctionDefinition(node, document);
                        }
                        String indexVal = IndexingUtilities.getIndexValue(eh);
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Indexing function definition {0} as {1}\n", new String[]{node.toString(), indexVal}); //NOI18N
                        }
                        index(document, IndexKey.FUNCTION_DEF, indexVal);
                        indexVal = e.asType() != null ? e.asType().toString() : null;
                        if (indexVal != null) {
                            if (DEBUG) {
                                LOG.log(Level.FINEST, "Indexing function def type reference {0}\n", new String[]{indexVal}); //NOI18N
                            }
                            index(document, IndexKey.TYPE_REF, indexVal);
                        } else {
                            LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node); //NOI18N
                        }
                    }
                }
            }
            return super.visitFunctionDefinition(node, document);
        }

        @Override
        public Void visitMethodInvocation(FunctionInvocationTree node, IndexDocument document) {
            Element el = trees.getElement(getCurrentPath());
            if (el == null) {
                if (DEBUG) {
                    LOG.log(Level.FINEST, "Error resolving element of {0}", node); //NOI18N
                }
                return super.visitMethodInvocation(node, document);
            }
            if (el.getKind() == ElementKind.METHOD) {
                ExecutableElement e = (ExecutableElement) el;
                Collection<ExecutableElement> overridenMethods = VisageSourceUtils.getOverridenMethods(e, elements);
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
                        LOG.log(Level.FINEST, "Indexing method invocation {0} as {1}\n", new String[]{node.toString(), indexVal}); //NOI18N
                    }
                    index(document, IndexKey.FUNCTION_INV, indexVal);
                    indexVal = e.getEnclosingElement() != null ? IndexingUtilities.getIndexValue(ElementHandle.create(e.getEnclosingElement())) : null;
                    if (indexVal != null) {
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Indexing function inv owner type reference {0}\n", new String[]{indexVal}); //NOI18N
                        }
                        index(document, IndexKey.TYPE_REF, indexVal);
                    } else {
                        LOG.log(Level.FINE, "Can not determine function owner inv type for: {0}", node != null ? node.getVisageKind() : "null"); //NOI18N
                    }
                    indexVal = e.asType() != null ? e.asType().toString() : null;
                    if (indexVal != null) {
                        if (DEBUG) {
                            LOG.log(Level.FINEST, "Indexing function inv return type reference {0}\n", new String[]{indexVal}); //NOI18N
                        }
                        index(document, IndexKey.TYPE_REF, indexVal);
                    } else {
                        LOG.log(Level.FINE, "Can not determine function inv return type for: {0}", node != null ? node.getVisageKind() : "null"); //NOI18N
                    }
                }
            }
            return super.visitMethodInvocation(node, document);
        }

        @Override
        public Void visitTypeClass(TypeClassTree node, IndexDocument document) {
            Element el = trees.getElement(getCurrentPath());
            if (el == null) {
                if (DEBUG) {
                    LOG.log(Level.FINEST, "Error resolving element of {0}", node); //NOI18N
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
                        LOG.log(Level.FINEST, "Indexing type reference {0} as {1}\n", new String[]{node.toString(), indexVal}); //NOI18N
                    }
                    index(document, IndexKey.TYPE_REF, indexVal);
                } else {
                    LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node); //NOI18N
                }
            }
            return super.visitTypeClass(node, document);
        }

        @Override
        public Void visitMemberSelect(MemberSelectTree node, IndexDocument document) {
            ExpressionTree expression = node.getExpression();
            if (expression instanceof VSGIdent) {
                Name memberName = node.getIdentifier();
                Type type = ((VSGIdent) expression).type;
                if (type == null) {
                    return super.visitMemberSelect(node, document);
                }
                TypeSymbol ts = type.asElement();
                if (ts == null) {
                    if (DEBUG) {
                        LOG.log(Level.FINEST, "Error resolving element of {0}", node); //NOI18N
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
                 * Unfortunately, we can not check for compilation errors before indexing because due to http://visage-jira.kenai.com/browse/VSGC-3468
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
                                        LOG.log(Level.FINEST, "Indexing type reference {0} as {1}\n", new String[]{node.toString(), indexVal}); //NOI18N
                                    }
                                    index(document, IndexKey.TYPE_REF, indexVal);
                                } else {
                                    LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node.getExpression()); //NOI18N
                                }
                                indexVal = IndexingUtilities.getIndexValue(ElementHandle.create(sy));
                                if (indexVal != null) {
                                    if (DEBUG) {
                                        LOG.log(Level.FINEST, "Indexing field reference {0} as {1}\n", new String[]{node.toString(), indexVal}); //NOI18N
                                    }
                                    index(document, IndexKey.FIELD_REF, indexVal);
                                } else {
                                    LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node); //NOI18N
                                }
                            }
                        }
                    }
                } catch (NullPointerException e) {
                    LOG.log(Level.INFO, "Trying to index non-compilable file {0}. Giving up.", fo.getPath()); //NOI18N
                }
            }

            return super.visitMemberSelect(node, document);
        }

        @Override
        public Void visitObjectLiteralPart(ObjectLiteralPartTree node, IndexDocument document) {
            Element el = trees.getElement(getCurrentPath());
            if (el == null) {
                if (DEBUG) {
                    LOG.log(Level.FINEST, "Error resolving element of {0}", node); //NOI18N
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
                            LOG.log(Level.FINEST, "Indexing field reference {0} as {1}\n", new String[]{node.toString(), indexVal}); //NOI18N
                        }
                        index(document, IndexKey.FIELD_REF, indexVal);
                    } else {
                        LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node); //NOI18N
                    }
                }
            }
            return super.visitObjectLiteralPart(node, document);
        }

        @Override
        public Void visitIdentifier(IdentifierTree node, IndexDocument document) {
            Element el = trees.getElement(getCurrentPath());
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
                            LOG.log(Level.FINEST, "Indexing field reference {0} as {1}\n", new String[]{node.toString(), indexVal}); //NOI18N
                        }
                        index(document, IndexKey.TYPE_REF, indexVal);
                    } else {
                        LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node); //NOI18N
                    }
                    break;
                }
            }
            return super.visitIdentifier(node, document);
        }

        @Override
        public Void visitInstantiate(InstantiateTree node, IndexDocument document) {
            Element el = trees.getElement(JavafxcTrees.getPath(getCurrentPath(), node.getIdentifier()));
            if (el == null) {
                if (DEBUG) {
                    LOG.log(Level.FINEST, "Error resolving element of {0}", node); //NOI18N
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
                        LOG.log(Level.FINEST, "Indexing type reference {0} as {1}\n", new String[]{node.toString(), indexVal}); //NOI18N
                    }
                    index(document, IndexKey.TYPE_REF, indexVal);
                } else {
                    LOG.log(Level.FINE, "Can not determine indexing value for: {0}", node); //NOI18N
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
        private static AtomicBoolean visageTaskFactoriesInitialized = new AtomicBoolean(false);

        public Factory() {
            if (!visageTaskFactoriesInitialized.getAndSet(true)) {
                ApiSourcePackageAccessor.get().registerSourceTaskFactoryManager();
            }
        }

        @Override
        public CustomIndexer createIndexer() {
            return new VisageIndexer();
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
    private DiagnosticListenerImpl diagnosticListener;

    @SuppressWarnings("unchecked")
    @Override
    protected void index(Iterable<? extends Indexable> itrbl, final Context cntxt) {
        if (cntxt == null || cntxt.getRoot() == null) return; // #187177: No idea why but this can happen
        if (!VisageSourceUtils.isPlatformOk(cntxt.getRoot())) return; // don't try to index files in a project with broken visage platform

        cancelled.set(false);

        if (cntxt.getRoot() != null) {

            ClasspathInfo cpInfo = ClasspathInfo.create(cntxt.getRoot());
            final ClassIndex ci = cpInfo.getClassIndex();

            final Map<JavaFileObject, Indexable> indexables = new HashMap<JavaFileObject, Indexable>();
            Iterator<? extends Indexable> iterator = itrbl.iterator();
            while(iterator.hasNext()) {
                try {
                    Indexable ix = iterator.next();
                    URI uri = ix.getURL().toURI();
                    File f = new File(uri.normalize());
                    FileObject fo = FileUtil.toFileObject(f);
                    if (fo != null) { // #187413: Don't try to create SourceFileObject for NULL; not sure when this happens but it does
                        indexables.put(SourceFileObject.create(fo, null), ix);
                    }
                } catch (URISyntaxException e) {
                }
            }

            JavafxcTask task = createJavafxcTaskImpl(cpInfo, indexables);
            if (task != null) {
                try {
                    Iterable<? extends UnitTree> units = null;
                    try {
                        task.parse();
                        units = task.analyze();
                    } catch (Throwable t) {
                        // catch all compiler madness
                    }
                    for(Indexable ix : indexables.values()) {
                        URI ixUri = ix.getURL().toURI();
                        Iterable<Diagnostic<?>> errs = (Iterable<Diagnostic<?>>)diagnosticListener.errors.get(ixUri);
                        ErrorsCache.setErrors(cntxt.getRootURI(), ix, errs != null ? errs : Collections.EMPTY_SET, DIAG_ERROR_CONVERTOR);
                    }
                    if (units != null && !cntxt.isSupplementaryFilesIndexing()) {
                        IndexingSupport support = IndexingSupport.getInstance(cntxt);
                        for(UnitTree ut : units) {
                            URI srcUri = ut.getSourceFile().toUri();
                            FileObject srcFo = FileUtil.toFileObject(new File(srcUri));
                            IndexDocument document = support.createDocument(srcFo);
                            JavafxcTrees trees = JavafxcTrees.instance(task);
                            IndexingVisitor visitor = new IndexingVisitor(trees, task.getElements(), srcFo);
                            visitor.scan(ut, document);
                            support.addDocument(document);
                            for(TypeElement tte : getTopLevelElements(ut, trees)) {
                                if (cancelled.get()) return;
                                reindexDependables(
                                    ci.getResources(ElementHandle.create(tte), EnumSet.of(SearchKind.TYPE_REFERENCES), EnumSet.allOf(SearchScope.class)),
                                    cntxt
                                );
                            }
                        }
                    }
                } catch (Exception e) {
                    LOG.log(Level.WARNING, "Error indexing", e); //NOI18N
                }
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

    // the following methods and classes are copies from VisageSource

    /**
     * Copied over from {@linkplain  VisageParser} class<br/>
     * <p>Need to bypass the parsing api for performance sakes - creating {@linkplain VisageSource} instances
     * for each indexable file sequentially is terribly slow and the standard parsing.api way allows for no
     * other option</p>
     * @see VisageParser#createJavafxcTaskImpl() 
     */
    private JavafxcTaskImpl createJavafxcTaskImpl(ClasspathInfo cpInfo, Map<JavaFileObject, Indexable> indexables) {
        boolean brokenPlatform = false;
        JavafxcTool tool = JavafxcTool.create();
        JavaFileManager fileManager = ApiSourcePackageAccessor.get().getFileManager(cpInfo, tool);

        try {
            if (fileManager.getFileForInput(StandardLocation.PLATFORM_CLASS_PATH, "java.lang", "Object.class") == null) { // NOI18N
                // not able to retrieve java.lang.Object => broken platform
                brokenPlatform = true;
            }
        } catch (IOException e) {
            LOG.log(Level.WARNING, null, e);
            brokenPlatform = true;
        }


        List<String> options = new ArrayList<String>();
        //options.add("-Xjcov"); //NOI18N, Make the compiler store end positions
        options.add("-XDdisableStringFolding"); //NOI18N

        // required for code formatting (and completion I believe), see VSGC-3528
        options.add("-XDpreserveTrees"); //NOI18N

        diagnosticListener = new DiagnosticListenerImpl();
        JavafxcTaskImpl task = (JavafxcTaskImpl)tool.getTask(null, fileManager, diagnosticListener, options, indexables.keySet());
        return brokenPlatform ? null : task;
    }

    static class DiagnosticListenerImpl implements DiagnosticListener<JavaFileObject> {

        final HashMap<URI,Collection<Diagnostic<?>>> errors;

        public DiagnosticListenerImpl() {
            this.errors = new HashMap<URI,Collection<Diagnostic<?>>>();
        }

        public void report(Diagnostic<? extends JavaFileObject> message) {
            URI srcUri = ((JavaFileObject)message.getSource()).toUri();
            Collection<Diagnostic<?>> localErrors = errors.get(srcUri);
            if (localErrors == null) {
                localErrors = new ArrayList<Diagnostic<?>>();
                errors.put(srcUri, localErrors);
            }
            localErrors.add(message);
        }
    }

    private List<? extends TypeElement> getTopLevelElements(UnitTree cu, JavafxcTrees trees) {
        final List<TypeElement> result = new ArrayList<TypeElement>();
        if (cu == null) {
            return null;
        }

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
        return Collections.unmodifiableList(result);
    }
}
