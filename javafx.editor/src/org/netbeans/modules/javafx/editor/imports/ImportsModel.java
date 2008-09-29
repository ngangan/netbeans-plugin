/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.modules.javafx.editor.imports;

import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.Tree;
import com.sun.tools.javafx.tree.JFXSelect;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.editor.imports.ui.FixImportsLayout;
import org.netbeans.modules.javafx.editor.imports.ui.FixItem;

import javax.lang.model.element.Element;
import javax.lang.model.element.Name;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.Document;
import java.text.Collator;
import java.util.*;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @todo documentation
 */
final class ImportsModel {
    private final List<ModelEntry> entries = new ArrayList<ModelEntry>(20);
    private final ClassIndex index;
    private final CompilationInfo ci;


    ImportsModel(List<? extends ImportTree> imports, ClassIndex index, CompilationInfo ci) {
        if (imports == null) throw new IllegalArgumentException("List of imports cannot be null.");
        for (ImportTree anImport : imports) {
            entries.add(new ModelEntry(anImport));
        }
        this.index = index;
        this.ci = ci;
    }

    void addImport(Element e) {
        if (!isImported(e)) {
            TypeMirror mirror = e.asType();
            TypeKind mk = mirror.getKind();
            switch (mk) {
                case DECLARED:
                    entries.add(new ModelEntry(mirror.toString()));
                    break;
                case ERROR:
                case NONE:
                    entries.add(resolveEntryByClassIndex(e));
                    break;                
            }
        }

    }

    private ModelEntry resolveEntryByClassIndex(Element e) {

        Set<ClassIndex.SearchScope> set = new TreeSet<ClassIndex.SearchScope>();
        set.add(ClassIndex.SearchScope.DEPENDENCIES);
        set.add(ClassIndex.SearchScope.SOURCE);
        Set<ElementHandle<TypeElement>> result = index.getDeclaredTypes(e.getSimpleName().toString(), ClassIndex.NameKind.SIMPLE_NAME, set);
        if (result.isEmpty()) {
            return null;
        } else if (result.size() == 1) {
            ElementHandle<TypeElement> handle = result.iterator().next();
            Name qname = handle.resolve(ci).getQualifiedName();
            return new ModelEntry(qname.toString());
        } else {
            return display(result);
        }
    }

    private ModelEntry display(final Set<ElementHandle<TypeElement>> options) {
        if (!SwingUtilities.isEventDispatchThread()) {
            try {
                final ModelEntry[] mes = new ModelEntry[1];
                SwingUtilities.invokeAndWait(new Runnable() {
                    public void run() {
                        mes[0] = display(options);
                    }
                });
                return mes[0];
            } catch (Exception e) {
                e.printStackTrace(); //TODO: [RKo] log
                
            }
        }

        // this code is intended to run in EDT.
        List<FixItem> items = createItems(options);
        final FixImportsLayout<FixItem> fil = FixImportsLayout.create();
//        fil.setEditorComponent(??);
        fil.show(items, "FixImports", 0, new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
            }
        }, "", "", 0);
        FixItem selectedItem = fil.getSelectedItem();
        fil.hide();        
        return new ModelEntry(selectedItem.getElement());
    }

    private List<FixItem> createItems(Set<ElementHandle<TypeElement>> options) {
        List<FixItem> result = new ArrayList<FixItem>(options.size());
        for (ElementHandle<TypeElement> option : options) {
            String qn = option.getQualifiedName();
            result.add(new FixItem(qn));
        }
        Collections.sort(result, FixItemComparator.get());
        return result;
    }

    boolean isImported(Element e) {
        for (ModelEntry entry : entries) {
            if (entry.includes(e.asType().toString())) {
                return true;
            }
        }
        return false;
    }


    void publish(Document doc) {

    }

    void optimize() {

    }

    private static class FixItemComparator  implements Comparator<FixItem> {
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
            if (qi.getJavaFXKind() == Tree.JavaFXKind.MEMBER_SELECT) {
                JFXSelect select = (JFXSelect) tree;
                type = select.getIdentifier().toString().trim();
                verifyType();
            } else {
                throw new IllegalArgumentException("Cannot resolve argument");
            }
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
            return "import " + type + (stared ? ".*":"") + (dStared ? ".**" : "") + ";";
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

        /**
         * Compares this object with the specified object for order.  Returns a
         * negative integer, zero, or a positive integer as this object is less
         * than, equal to, or greater than the specified object.
         * <p/>
         * <p>The implementor must ensure <tt>sgn(x.compareTo(y)) ==
         * -sgn(y.compareTo(x))</tt> for all <tt>x</tt> and <tt>y</tt>.  (This
         * implies that <tt>x.compareTo(y)</tt> must throw an exception iff
         * <tt>y.compareTo(x)</tt> throws an exception.)
         * <p/>
         * <p>The implementor must also ensure that the relation is transitive:
         * <tt>(x.compareTo(y)&gt;0 &amp;&amp; y.compareTo(z)&gt;0)</tt> implies
         * <tt>x.compareTo(z)&gt;0</tt>.
         * <p/>
         * <p>Finally, the implementor must ensure that <tt>x.compareTo(y)==0</tt>
         * implies that <tt>sgn(x.compareTo(z)) == sgn(y.compareTo(z))</tt>, for
         * all <tt>z</tt>.
         * <p/>
         * <p>It is strongly recommended, but <i>not</i> strictly required that
         * <tt>(x.compareTo(y)==0) == (x.equals(y))</tt>.  Generally speaking, any
         * class that implements the <tt>Comparable</tt> interface and violates
         * this condition should clearly indicate this fact.  The recommended
         * language is "Note: this class has a natural ordering that is
         * inconsistent with equals."
         * <p/>
         * <p>In the foregoing description, the notation
         * <tt>sgn(</tt><i>expression</i><tt>)</tt> designates the mathematical
         * <i>signum</i> function, which is defined to return one of <tt>-1</tt>,
         * <tt>0</tt>, or <tt>1</tt> according to whether the value of
         * <i>expression</i> is negative, zero or positive.
         *
         * @param o the object to be compared.
         * @return a negative integer, zero, or a positive integer as this object
         *         is less than, equal to, or greater than the specified object.
         * @throws ClassCastException if the specified object's type prevents it
         *                            from being compared to this object.
         */
        public int compareTo(ModelEntry o) {
            return 0;
        }
    }


}
