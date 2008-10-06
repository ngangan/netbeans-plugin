/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.modules.javafx.editor.imports;

import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree;
import com.sun.tools.javac.code.Symbol;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.lexer.TokenId;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.editor.GuardedDocument;
import org.netbeans.modules.editor.indent.api.Reformat;
import org.netbeans.modules.javafx.editor.JFXImportManager;
import org.netbeans.modules.javafx.editor.imports.ui.FixImportsLayout;
import org.netbeans.modules.javafx.editor.imports.ui.FixItem;
import org.openide.util.NbBundle;

import javax.lang.model.element.Element;
import javax.lang.model.element.Name;
import javax.lang.model.element.TypeElement;
import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;
import java.awt.*;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.text.Collator;
import java.util.*;
import java.util.List;
import java.util.logging.Logger;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @todo documentation
 */
public final class ImportsModel {
    public static final Logger logger = Logger.getLogger(JFXImportManager.class.getName());
    private final List<ModelEntry> entries = new ArrayList<ModelEntry>(20);
    private final ClassIndex index;
    private final CompilationInfo ci;
    private final Reference<JTextComponent> tc;
    private static final Rectangle DEFAULT_POS = new Rectangle(0, 0, 10, 10);


    ImportsModel(List<? extends ImportTree> imports, ClassIndex index, CompilationInfo ci, JTextComponent tc) {
        this.tc = new WeakReference<JTextComponent>(tc);
        if (imports == null) throw new IllegalArgumentException("List of imports cannot be null.");
        for (ImportTree anImport : imports) {
            entries.add(new ModelEntry(anImport));
        }
        this.index = index;
        this.ci = ci;
    }

    public void addImport(Element e) {        
        if (e == null) return;
        if (!isImported(e)) {
            resolveEntryByClassIndex(e);
            /*TypeMirror mirror = e.asType();
            TypeKind mk = mirror.getKind();
            switch (mk) {
                case ERROR:
                case NONE:
                    resolveEntryByClassIndex(e);
                    break;
                default:
                    entries.add(new ModelEntry(mirror.toString()));
                    break;
            }*/
        }
    }

    public void addImport(String qn) {
        entries.add(new ModelEntry(qn));
    }

    private synchronized void resolveEntryByClassIndex(final Element e) {
        Set<ClassIndex.SearchScope> set = new TreeSet<ClassIndex.SearchScope>();
        set.add(ClassIndex.SearchScope.DEPENDENCIES);
        set.add(ClassIndex.SearchScope.SOURCE);
        final Set<ElementHandle<TypeElement>> result = index.getDeclaredTypes(e.getSimpleName().toString(), ClassIndex.NameKind.SIMPLE_NAME, set);
        if (result.size() == 1) {
            ElementHandle<TypeElement> handle = result.iterator().next();
            Name qname = handle.resolve(ci).getQualifiedName();
            entries.add(new ModelEntry(qname.toString()));
        } else if (!result.isEmpty()) {
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    moveCaret(e);
                    display(result, getHeaderText(e));
                }
            });
            try {
                logger.info("Waiting...");
                wait();
                logger.info("Continueing...");
            } catch (InterruptedException e1) {
                logger.severe(e1.getLocalizedMessage());
            }
        }
    }

    private String getHeaderText(Element e) {
        return NbBundle.getMessage(ImportsModel.class, "FI_IMPORT_UI_HEADER", e.getSimpleName().toString());
    }

    private void moveCaret(Element e) {
        JavaFXTreePath elpath = ci.getPath(e);
        if (elpath == null) return;
        Tree tree = elpath.getLeaf();

        if (tree != null) {
            long startPos = ci.getTrees().getSourcePositions().getStartPosition(ci.getCompilationUnit(), tree);
            JTextComponent jtc = tc.get();
            jtc.getCaret().setDot((int) startPos);

        }
    }

    private void display(final Set<ElementHandle<TypeElement>> options, String text) {
        // this code is intended to run in EDT.
        final FixImportsLayout<FixItem> fil = FixImportsLayout.create();
        fil.setEditorComponent(tc.get());
        List<FixItem> items = createItems(options, fil);
        fil.show(items, text, 0, new MyListSelectionListener(), null, null, 0);
    }

    private List<FixItem> createItems(Set<ElementHandle<TypeElement>> options, FixImportsLayout<FixItem> fil) {
        List<FixItem> result = new ArrayList<FixItem>(options.size());
        for (ElementHandle<TypeElement> option : options) {
            String qn = option.getQualifiedName();
            result.add(new FixItem(qn, this, fil));
        }
        Collections.sort(result, FixItemComparator.get());
        return result;
    }

    boolean isImported(Element e) {
        Symbol s = (Symbol) e;
        if (s.isLocal()) return true;
        for (ModelEntry entry : entries) {
            if (entry != null && entry.includes(e.asType().toString())) {
                return true;
            }
        }
        return false;
    }


    void publish(final Document doc) {
        Runnable runnable = new Runnable() {
            public void run() {
                Collections.sort(entries);
                TokenSequence<JFXTokenId> ts = getTokenSequence(doc, 0);
                final int startPos = quessImportsStart(ts);
                final int endPos = quessImportsEnd(ts, startPos);
                Reformat reformat = null;
                boolean relock = !(doc instanceof GuardedDocument);
                try {
                    Position end = doc.createPosition(endPos);
                    logger.info("Publishing following entries:");
                    doc.remove(startPos, endPos - startPos);
                    int offset = startPos;
                    boolean first = true;
                    for (ModelEntry entry : entries) {
                        logger.info("\t" + entry.toImportStatement());
                        String text = (first ? "" : "\n") + entry.toImportStatement();
                        first = false;
                        doc.insertString(offset, text, null);
                        offset += text.length();
                    }
                    reformat = Reformat.get(doc);
                    reformat.lock();
                    reformat.reformat(0, end.getOffset());
                } catch (BadLocationException e) {
                    logger.severe(e.getLocalizedMessage());
                } finally {
                    if (reformat != null) {
                        reformat.unlock();
                    }
                }
            }
        };
        if (doc instanceof GuardedDocument) {
            GuardedDocument gd = (GuardedDocument) doc;
            gd.runAtomic(runnable);
        } else {
            logger.warning("Running in non atomic fashion.");
            runnable.run();
        }

    }

    private int quessImportsEnd(TokenSequence<JFXTokenId> ts, int startPos) {
        int result = startPos;
        while (ts.moveNext()) {
            JFXTokenId tid = ts.token().id();
            switch (tid) {
                case IMPORT: {
                    moveTo(ts, JFXTokenId.SEMI);
                    result = ts.offset() + 1;
                    continue;
                }
                case WS:
                    continue;
                default: {
                    return result;
                }
            }
        }
        return result;
    }

    private int quessImportsStart(TokenSequence<JFXTokenId> ts) {
        int posibbleStart = 0;
        while (ts.moveNext()) {
            JFXTokenId tid = ts.token().id();
            switch (tid) {
                case PACKAGE: {
                    moveTo(ts, JFXTokenId.SEMI);
                    posibbleStart = ts.offset() + 1;
                    continue;
                }
                case IMPORT: {
                    posibbleStart = ts.offset();
                    moveTo(ts, JFXTokenId.SEMI);
                    return posibbleStart;
                }
                case WS:
                case COMMENT:
                case LINE_COMMENT:
                case DOC_COMMENT:
                    continue;
                default: {
                    return posibbleStart;
                }
            }
        }
        return posibbleStart;
    }

    private void moveTo(TokenSequence<JFXTokenId> ts, JFXTokenId id) {
        while (ts.moveNext()) {
            if (ts.token().id() == id) return;
        }
    }


    private static <T extends TokenId> TokenSequence<T> getTokenSequence(Document doc, int dotPos) {
        TokenHierarchy<Document> th = TokenHierarchy.get(doc);
        TokenSequence<T> seq = (TokenSequence<T>) th.tokenSequence();
        seq.move(dotPos);
        return seq;
    }


    void optimize() {

    }

    private static class FixItemComparator implements Comparator<FixItem> {
        public static final Collator collator = Collator.getInstance();
        public static final Comparator<FixItem> instance = new FixItemComparator();

        public static Comparator<FixItem> get() {
            return instance;
        }

        public int compare(FixItem o1, FixItem o2) {
            return collator.compare(o1.getElement(), o2.getElement());
        }
    }

    private static class ModelEntry implements Comparable<ModelEntry> {
        String type;
        ImportTree tree;
        boolean stared;
        boolean dStared;

        private ModelEntry(ImportTree tree) {
            this.tree = tree;
            Tree qi = tree.getQualifiedIdentifier();
            type = qi.toString();
            verifyType();
        }

        private void verifyType() {
            stared = type.endsWith(".*");
            dStared = type.endsWith(".**");
            if (stared || dStared) {
                int index = type.indexOf(".*");
                type = type.substring(0, index);
            }
        }

        private ModelEntry(String type) {
            this.type = type;
            verifyType();
        }

        boolean includes(String type) {
            if (dStared) {
                return type.startsWith(this.type);
            } else if (stared) {
                int dotIndex = type.lastIndexOf('.');
                return this.type.equals(type.substring(0, dotIndex));
            }
            return this.type.equals(type);
        }

        String toImportStatement() {
            return "import " + type + (stared ? ".*" : "") + (dStared ? ".**" : "") + ";";
        }

        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            ModelEntry that = (ModelEntry) o;

            if (dStared != that.dStared) return false;
            if (stared != that.stared) return false;
            if (tree != null ? !tree.equals(that.tree) : that.tree != null) return false;
            if (type != null ? !type.equals(that.type) : that.type != null) return false;

            return true;
        }

        public int hashCode() {
            int result;
            result = (type != null ? type.hashCode() : 0);
            result = 31 * result + (tree != null ? tree.hashCode() : 0);
            result = 31 * result + (stared ? 1 : 0);
            result = 31 * result + (dStared ? 1 : 0);
            return result;
        }

        public int compareTo(ModelEntry o) {
            return type != null ? o != null ? type.compareToIgnoreCase(o.type) : -1 : 1;
        }


        public String toString() {
            return "ModelEntry[" +
                    "type='" + type + '\'' +
                    ']';
        }
    }


    private static class MyListSelectionListener implements ListSelectionListener {
        public void valueChanged(ListSelectionEvent e) {
        }
    }
}
