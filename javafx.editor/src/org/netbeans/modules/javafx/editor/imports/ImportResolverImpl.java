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

import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.editor.GuardedDocument;
import org.netbeans.modules.javafx.editor.imports.ui.FixImportsLayout;
import org.netbeans.modules.javafx.editor.imports.ui.FixItem;
import org.openide.util.NbBundle;

import javax.lang.model.element.Element;
import javax.lang.model.element.Name;
import javax.lang.model.element.TypeElement;
import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.lang.ref.SoftReference;
import java.text.Collator;
import java.util.*;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @todo documentation
 * @todo Use RequestProcessor if possible.
 */
final class ImportResolverImpl extends Thread implements ImportResolver, FocusListener {
    private ClassIndex index;
    //    private CompilationInfo ci;
    private SoftReference<JTextComponent> component;
    private static final EnumSet<ClassIndex.SearchScope> SCOPE = EnumSet.of(ClassIndex.SearchScope.DEPENDENCIES, ClassIndex.SearchScope.SOURCE);
    private final Object LOCK = new Object();
    private ImportsModel model;
    private Queue<Element> queue = new ConcurrentLinkedQueue<Element>();
    private static Logger log = Logger.getLogger(ImportResolverImpl.class.getName());
    private static final TypesComparator TYPES_COMPARATOR = new TypesComparator();
    private boolean interupt = false;

    /**
     * Allocates a new <code>Thread</code> object. This constructor has
     * the same effect as <code>Thread(null, null,</code>
     * <i>gname</i><code>)</code>, where <b><i>gname</i></b> is
     * a newly generated name. Automatically generated names are of the
     * form <code>"Thread-"+</code><i>n</i>, where <i>n</i> is an integer.
     *
     * @param index     ClassIndex instance to use for resolving.
     * @param component holding editor.
     * @see #Thread(ThreadGroup, Runnable, String)
     */
    private ImportResolverImpl(ClassIndex index, /*CompilationInfo ci,*/ JTextComponent component) {
        this.index = index;
//        this.ci = ci;
        this.component = new SoftReference<JTextComponent>(component);
        component.addFocusListener(this);
    }

    public static ImportResolver create(ClassIndex index, /*CompilationInfo ci,*/ JTextComponent component) {
        return new ImportResolverImpl(index, /*ci,*/ component);
    }

    public synchronized void resolve(ImportsModel model, Element e) {
        resolve(model, Collections.singletonList(e));
    }

    public synchronized void resolve(ImportsModel model, Collection<Element> elements) {
        if (this.model != null && this.model != model) {
            throw new IllegalStateException("Illegal model. Create new instance of resolver first.");
        } else if (this.model == null) {
            this.model = model;
        }
        offerToQueue(elements);
        ensureStart();
    }

    private void ensureStart() {
        if (!this.isAlive()) {
            if (log.isLoggable(Level.INFO)) {
                log.log(Level.INFO, "Starting resolving thread.");
            }
            start();
        }
    }

    private void offerToQueue(Collection<Element> elements) {
        for (Element element : elements) {
            if (element != null) {
                queue.offer(element);
            }
        }
    }

    /**
     * If this thread was constructed using a separate
     * <code>Runnable</code> run object, then that
     * <code>Runnable</code> object's <code>run</code> method is called;
     * otherwise, this method does nothing and returns.
     * <p/>
     * Subclasses of <code>Thread</code> should override this method.
     *
     * @see #start()
     * @see #stop()
     * @see #Thread(ThreadGroup, Runnable, String)
     */
    @Override
    public void run() {
        super.run();
        Element element;
        while ((element = queue.poll()) != null && !interupt) {
            resolveImpl(model, element);
        }
        if (interupt) {
            log.info("Fix imports has been interupted because focus has been lost.");
        }
        if (log.isLoggable(Level.INFO)) {
            log.log(Level.INFO, "Resolving finished.");
            log.log(Level.INFO, "Publishing entries...");
        }
        Runnable runnable = new Runnable() {
            public void run() {
                JTextComponent comp = component.get();
                publish(comp.getDocument());
                comp.requestFocusInWindow();
                if (log.isLoggable(Level.INFO)) {
                    log.log(Level.INFO, "Fix Imports done.");
                }
            }
        };
        SwingUtilities.invokeLater(runnable);
        component.get().removeFocusListener(this);
    }

    private void publish(final Document doc) {
        Runnable runnable = createPublisher(doc);
        if (doc instanceof GuardedDocument) {
            GuardedDocument gd = (GuardedDocument) doc;
            gd.runAtomic(runnable);
        } else {
            log.warning("Running in non atomic fashion.");
            doc.render(runnable);
        }
    }

    private Runnable createPublisher(final Document doc) {
        return new Publisher(doc, model);
    }


    private void resolveImpl(ImportsModel model, Element e) {
        if (model.isImported(e)) {
            return;
        }
        if (log.isLoggable(Level.INFO)) {
            log.log(Level.INFO, "Resolving element. " + e.getSimpleName());
        }
        String name = toSimpleName(e);
        if (name == null) return;
        Set<ElementHandle<TypeElement>> types = index.getDeclaredTypes(name, ClassIndex.NameKind.SIMPLE_NAME, SCOPE);
        types = filterResult(types);
        if (types.size() == 1) {
            model.append(createEntry(types.iterator().next()));
        } else if (!types.isEmpty()) {
            askUser(model, e, types);
        }
        if (log.isLoggable(Level.INFO)) {
            log.log(Level.INFO, "Element resolved");
        }

    }

    private String getHeaderText(Element e) {
        return NbBundle.getMessage(ImportsModel.class, "FI_IMPORT_UI_HEADER", e.getSimpleName().toString());
    }

    private void askUser(final ImportsModel model, final Element e, final Set<ElementHandle<TypeElement>> types) {
        final FixImportsLayout<FixItem> fil = FixImportsLayout.create();
        final JTextComponent comp = component.get();
        fil.setEditorComponent(comp);
        Runnable runnable = new Runnable() {
            public void run() {
                if (log.isLoggable(Level.INFO)) {
                    log.log(Level.INFO, "Showing dialog...");
                }
                fil.show(convert(types), getHeaderText(e), comp.getCaretPosition(), new MyListSelectionListener(), null, null, -1);
            }
        };
        try {
            SwingUtilities.invokeAndWait(runnable);
        } catch (Exception e1) {
            e1.printStackTrace();
            return;
        }
        synchronized (LOCK) {
            try {
                LOCK.wait();
                if (log.isLoggable(Level.INFO)) {
                    log.log(Level.INFO, "... closing dialog.");
                }
            } catch (InterruptedException e1) {
                e1.printStackTrace();
                return;
            }
        }
        FixItem selection = fil.getSelectedItem();
        if (selection != null) {
            model.append(new ImportsModel.ModelEntry(selection.getElement()));
        }
        runnable = new Runnable() {
            public void run() {
                fil.finish();
            }
        };
        SwingUtilities.invokeLater(runnable);
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

    private ImportsModel.ModelEntry createEntry(ElementHandle<TypeElement> handle) {
        String qname = handle.getQualifiedName();
        return new ImportsModel.ModelEntry(qname);
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

    private String toSimpleName(Element e) {
        Name simpleName = e.getSimpleName();
        if (simpleName == null) return null;
        return simpleName.toString();
    }

    /**
     * Invoked when a component gains the keyboard focus.
     */
    public void focusGained(FocusEvent e) {
    }

    /**
     * Invoked when a component loses the keyboard focus.
     */
    public void focusLost(FocusEvent e) {
        log.info("Loosing focus in prospect of " + e.getOppositeComponent());
        component.get().removeFocusListener(this);
        interupt = true;
    }

    private static class MyListSelectionListener implements ListSelectionListener {
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
