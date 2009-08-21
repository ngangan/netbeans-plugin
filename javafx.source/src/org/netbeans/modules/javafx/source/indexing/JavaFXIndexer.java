/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.source.indexing;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.FunctionInvocationTree;
import com.sun.javafx.api.tree.InstantiateTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.TypeClassTree;
import com.sun.javafx.api.tree.VariableTree;
import java.io.IOException;
import java.util.logging.Level;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.javafx.source.JavaFXParserResult;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.parsing.api.Snapshot;
import org.netbeans.modules.parsing.spi.Parser.Result;
import org.netbeans.modules.parsing.spi.indexing.Context;
import org.netbeans.modules.parsing.spi.indexing.EmbeddingIndexer;
import org.netbeans.modules.parsing.spi.indexing.EmbeddingIndexerFactory;
import org.netbeans.modules.parsing.spi.indexing.Indexable;
import org.netbeans.modules.parsing.spi.indexing.support.IndexDocument;
import org.netbeans.modules.parsing.spi.indexing.support.IndexingSupport;

/**
 *
 * @author Jaroslav Bachorik
 */
public class JavaFXIndexer extends EmbeddingIndexer {
    final private static java.util.logging.Logger LOG = java.util.logging.Logger.getLogger(JavaFXIndexer.class.getName());
    final private static boolean LOG_FINEST = LOG.isLoggable(Level.FINEST);

    final public static String NAME = "fx";
    final public static int VERSION = 1;

    public enum IndexKey {
        PACKAGE_NAME,
        CLASS_FQN, CLASS_NAME_SIMPLE, CLASS_NAME_INSENSITIVE,
        FUNCTION_DEF, FUNCTION_INV, FIELD_DEF,
        TYPE_REF, TYPE_IMPL,
        NOT_INDEXED
    }


    // <editor-fold defaultstate="collapsed" desc="Indexer Factory">
    public static class Factory extends EmbeddingIndexerFactory {

        @Override
        public EmbeddingIndexer createIndexer(Indexable indexable, Snapshot snapshot) {
            return new JavaFXIndexer();
        }

        @Override
        public void filesDeleted(Iterable<? extends Indexable> itrbl, Context cntxt) {
            for(Indexable ixbl : itrbl) {
                try {
                    IndexingSupport.getInstance(cntxt).removeDocuments(ixbl);
                } catch (IOException e) {
                    LOG.log(Level.WARNING, null, e);
                }
            }
        }

        @Override
        public void filesDirty(Iterable<? extends Indexable> itrbl, Context cntxt) {
            for(Indexable ixbl : itrbl) {
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
    @Override
    protected void index(Indexable indexable, Result result, Context context) {
        final JavaFXParserResult fxresult = (JavaFXParserResult)result;
        if (LOG_FINEST) {
            LOG.log(Level.FINEST,"Indexing {0}", indexable.toString());
            LOG.log(Level.FINEST, "Tree: {0}", fxresult.getCompilationUnit());
        }

        IndexingSupport support;
        try {
            support = IndexingSupport.getInstance(context);
        } catch (IOException ioe) {
            LOG.log(Level.WARNING, null, ioe);
            return;
        }
        IndexDocument document = support.createDocument(indexable);

        JavaFXTreePathScanner<Void, IndexDocument> visitor = new JavaFXTreePathScanner<Void, IndexDocument>() {
            private TypeElement lastSeenClass = null;
            @Override
            public Void visitClassDeclaration(ClassDeclarationTree node, IndexDocument document) {
                TypeElement type = (TypeElement)fxresult.getTrees().getElement(getCurrentPath());
                if (LOG_FINEST) {
                    LOG.log(Level.FINEST, "Indexing {0}:", type.getQualifiedName());
                    LOG.log(Level.FINEST, "  Simple: {0}", node.getSimpleName());
                    LOG.log(Level.FINEST, "  Case insensitive: {0}", node.getSimpleName().toString().toLowerCase());
                }
                index(document, IndexKey.CLASS_NAME_SIMPLE, node.getSimpleName().toString());
                index(document, IndexKey.CLASS_NAME_INSENSITIVE, node.getSimpleName().toString().toLowerCase());
                String fqn = type.getQualifiedName().toString();
                index(document, IndexKey.CLASS_FQN, fqn);
                if (type.getNestingKind() == NestingKind.TOP_LEVEL) {
                    int pkgLen = fqn.lastIndexOf(".");
                    if (pkgLen > -1) {
                        index(document, IndexKey.PACKAGE_NAME, fqn.substring(0, pkgLen));
                    }
                }
                lastSeenClass = type;
                return super.visitClassDeclaration(node, document);
            }

            @Override
            public Void visitVariable(VariableTree node, IndexDocument document) {
                Element e = fxresult.getTrees().getElement(getCurrentPath());
                if (e.getKind() == ElementKind.FIELD) { // can handle only fields for now
                    String indexVal = IndexingUtilities.getIndexValue(ElementHandle.create(e)) + IndexingUtilities.INDEX_SEPARATOR + lastSeenClass.getQualifiedName().toString();
                    if (LOG_FINEST) {
                        LOG.log(Level.FINEST, "Indexing variable {0} as {1}\n", new String[]{node.toString(), indexVal});
                    }
                    index(document, IndexKey.FIELD_DEF, indexVal);
                }
                return super.visitVariable(node, document);
            }

            @Override
            public Void visitFunctionDefinition(FunctionDefinitionTree node, IndexDocument document) {
                Element el = fxresult.getTrees().getElement(getCurrentPath());
                if (el.getKind() == ElementKind.METHOD) {
                    ExecutableElement e = (ExecutableElement)el;
                    if (!e.getSimpleName().contentEquals("javafx$run$")) { // skip the synthetic "$javafx$run$" method generated for javafx scripts
                        String indexVal = IndexingUtilities.getIndexValue(ElementHandle.create(e)) + IndexingUtilities.INDEX_SEPARATOR + lastSeenClass.getQualifiedName().toString();
                        if (LOG_FINEST) {
                            LOG.log(Level.FINEST, "Indexing function definition {0} as {1}\n", new String[]{node.toString(), indexVal});
                        }
                        index(document, IndexKey.FUNCTION_DEF, indexVal);
                    }
                }
                return super.visitFunctionDefinition(node, document);
            }

            @Override
            public Void visitMethodInvocation(FunctionInvocationTree node, IndexDocument document) {
                Element el = fxresult.getTrees().getElement(getCurrentPath());
                if (el.getKind() == ElementKind.METHOD) {
                    ExecutableElement e = (ExecutableElement)el;
                    String indexVal = IndexingUtilities.getIndexValue(ElementHandle.create(e)) + IndexingUtilities.INDEX_SEPARATOR + lastSeenClass.getQualifiedName().toString();
                    if (LOG_FINEST) {
                        LOG.log(Level.FINEST, "Indexing method invocation {0} as {1}\n", new String[]{node.toString(), indexVal});
                    }
                    index(document, IndexKey.FUNCTION_INV, indexVal);
                }
                return super.visitMethodInvocation(node, document);
            }

            @Override
            public Void visitInstantiate(InstantiateTree node, IndexDocument p) {
                return super.visitInstantiate(node, p);
            }

            @Override
            public Void visitTypeClass(TypeClassTree node, IndexDocument document) {
                Element el = fxresult.getTrees().getElement(getCurrentPath());
                if (el.getKind() == ElementKind.CLASS || el.getKind() == ElementKind.INTERFACE) {
                    String indexVal = IndexingUtilities.getIndexValue(ElementHandle.create(el));
                    if (LOG_FINEST) {
                        LOG.log(Level.FINEST, "Indexing type reference {0} as {1}\n", new String[]{node.toString(), indexVal});
                    }
                    index(document, IndexKey.TYPE_REF, indexVal);
                }
                return super.visitTypeClass(node, document);
            }

        };
        visitor.scan(fxresult.getCompilationUnit(), document);
        support.addDocument(document);
    }

    private void index(IndexDocument document, IndexKey key, String value) {
        document.addPair(key.toString(), value, true, true);
    }
}
