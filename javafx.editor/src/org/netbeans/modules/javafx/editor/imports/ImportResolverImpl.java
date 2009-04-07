/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
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
 * Portions Copyrighted 1997-2008 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.editor.imports;

import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.editor.imports.ui.FixImportsLayout;
import org.netbeans.modules.javafx.editor.imports.ui.FixItem;
import org.openide.util.NbBundle;

import javax.lang.model.element.TypeElement;
import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.lang.ref.SoftReference;
import java.text.Collator;
import java.util.*;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.text.Document;
import org.netbeans.editor.GuardedDocument;
import org.openide.util.RequestProcessor;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @author Jaroslav Bachorik
 * @todo documentation
 * @todo Use RequestProcessor if possible.
 */
final class ImportResolverImpl {
    private SoftReference<JTextComponent> component;
    private final Object LOCK = new Object();
    private static Logger log = Logger.getLogger(ImportResolverImpl.class.getName());
    private static final TypesComparator TYPES_COMPARATOR = new TypesComparator();
    private static final ResourceBundle BUNDLE = ResourceBundle.getBundle("org/netbeans/modules/javafx/editor/imports/Bundle");
    private CompilationInfo ci;
    private int caret;

    /**
     * The resolver will use the imformation from the given {@linkplain ImportsModel} instance
     * to resolve missing imports. In case a missing import can be resolved to more than one
     * actual import the user is presented with a popup list of all options which he
     * can choose the correct one from.
     *
     * The resolution process is run asynchronously.
     */
    private ImportResolverImpl(CompilationInfo ci, JTextComponent component) {
        this.ci = ci;
        this.component = new SoftReference<JTextComponent>(component);
        this.caret = component.getCaret().getDot();
    }

    /**
     * Factory method
     * @param ci {@linkplain CompilationInfo} to use for resolution
     * @param component {@linkplain JTextComponent} holding the document
     * @return Returns a new instance of {@linkplain ImportResolverImpl}
     */
    public static ImportResolverImpl create(CompilationInfo ci, JTextComponent component) {
        return new ImportResolverImpl(ci, component);
    }

    /**
     * Will resolve the unresolved imports from the given {@linkplain ImportsModel}
     * In case of ambiguity it will give the user a chance to specify the resolution
     *
     * Runs asynchronously.
     * 
     * @param model The {@linkplain ImportsModel} instance holding the info about unresolved imports
     */
    public void resolve(final ImportsModel model) {
        RequestProcessor.getDefault().post(new Runnable() {

            public void run() {
                doResolve(model);
            }
        });
    }

    /**
     * @see ImportResolverImpl#resolve(org.netbeans.modules.javafx.editor.imports.ImportsModel)
     */
    private void doResolve(ImportsModel model) {
        for(final ImportsModel.Unresolved unresolved : model.getUnresolved()) {
            if (unresolved.getOptions().size() == 0) continue;
            if (unresolved.getOptions().size() == 1) {
                unresolved.resolve(unresolved.getOptions().iterator().next().getQualifiedName());
                continue;
            }
            try {
                final FixImportsLayout<FixItem> fil = FixImportsLayout.create(LOCK);
                final JTextComponent comp = component.get();
                SwingUtilities.invokeLater(new Runnable() {

                    public void run() {
                        long startPos = unresolved.getElementPos();

                        fil.setEditorComponent(comp);
                        try {
                            Rectangle rectangle = comp.modelToView((int)startPos);
                            comp.scrollRectToVisible(rectangle);
                            comp.setCaretPosition((int)startPos);
                            if (log.isLoggable(Level.INFO)) {
                                log.log(Level.INFO, "Showing dialog..."); // NOI18N
                            }
                            fil.show(convert(unresolved.getOptions()), getHeaderText(unresolved.getUnresolvedElement().toString()), (int)startPos, new DummySelectionListener(), null, null, -1);
                        } catch (BadLocationException e) {}
                    }
                });
                synchronized (LOCK) {
                    LOCK.wait();
                    if (log.isLoggable(Level.INFO)) {
                        log.log(Level.INFO, "... closing dialog."); // NOI18N
                    }
                }
                unresolved.resolve(fil.getSelectedItem() != null ? fil.getSelectedItem().getElement() : null);
                fil.finish();
                if (fil.isCancelled()) {
                    SwingUtilities.invokeLater(new Runnable() {
                        public void run() {
                            component.get().getCaret().setDot(caret);
                        }
                   });
                    return;
                }
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
            }
        }
        Document doc = component.get().getDocument();
        Publisher publisher = new Publisher(doc, model, component.get().getCaret(), caret);
        if (doc instanceof GuardedDocument) {
            GuardedDocument gd = (GuardedDocument) doc;
            gd.runAtomic(publisher);
        } else {
            log.warning(BUNDLE.getString("Running_in_non_atomic_fashion.")); // NOI18N
            doc.render(publisher);
        }
    }

    private String getHeaderText(String typeName) {
        return NbBundle.getMessage(ImportResolverImpl.class, "FI_IMPORT_UI_HEADER", typeName); // NOI18N
    }

    private List<FixItem> convert(Set<ElementHandle<TypeElement>> types) {
        List<FixItem> result = new ArrayList<FixItem>(types.size());
        for (ElementHandle<TypeElement> option : types) {
            String qn = option.getQualifiedName();
            result.add(new FixItem(qn, LOCK));
        }
        Collections.sort(result, FixItemComparator.get());
        return result;
    }

    private Set<ElementHandle<TypeElement>> filterResult(Set<ElementHandle<TypeElement>> types) {
        Set<ElementHandle<TypeElement>> set = new TreeSet<ElementHandle<TypeElement>>(TYPES_COMPARATOR);
//        Iterator<ElementHandle<TypeElement>> handleIterator = types.iterator();
//        while (handleIterator.hasNext()) {
//            ElementHandle<TypeElement> handle = handleIterator.next();
//            switch (handle.getKind()) {
//                case CLASS:
//                case INTERFACE:
//                    continue;
//                default:
//                    handleIterator.remove();
//            }
//        }
        set.addAll(types);
        return set;
    }

    private static class DummySelectionListener implements ListSelectionListener {
        public void valueChanged(ListSelectionEvent e) {
        }
    }

    private static class FixItemComparator implements Comparator<FixItem> {
        public static final Collator collator = Collator.getInstance();
        public static final Comparator<FixItem> instance = new FixItemComparator();

        public static Comparator<FixItem> get() {
            return instance;
        }

        public int compare(FixItem o1, FixItem o2) {
            if (o1.getSortPriority() == o2.getSortPriority()) {
                return collator.compare(o1.getElement(), o2.getElement());
            }
            return o1.getSortPriority() - o2.getSortPriority();
        }
    }

    private static class TypesComparator implements Comparator<ElementHandle<TypeElement>> {
        public int compare(ElementHandle<TypeElement> o1, ElementHandle<TypeElement> o2) {
            String s1 = o1.getQualifiedName();
            return s1 != null ? s1.compareTo(o2.getQualifiedName()) : 1;
        }
    }
}
