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
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2008 Sun
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

package org.netbeans.modules.javafx.editor.imports;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.editor.JFXImportManager;
import static org.netbeans.modules.javafx.editor.imports.ImportsModel.ModelEntry;
import org.netbeans.modules.javafx.editor.imports.ui.FixImportsLayout;
import org.netbeans.modules.javafx.editor.imports.ui.FixItem;
import org.netbeans.modules.javafx.editor.imports.ui.FixItemHandler;
import org.openide.util.NbBundle;

import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.Name;
import javax.lang.model.element.TypeElement;
import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.text.JTextComponent;
import java.lang.ref.Reference;
import java.text.Collator;
import java.util.*;
import java.util.logging.Logger;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @todo documentation
 */
class ModelEntryResolver implements ImportResolver {
    public static final Logger logger = Logger.getLogger(JFXImportManager.class.getName());
    private final ClassIndex index;
    private final CompilationInfo ci;
    private final Reference<JTextComponent> tc;
    private ArrayList<Element> queue;
    final FixImportsLayout<FixItem> fil;


    private ModelEntryResolver(ClassIndex index, CompilationInfo ci, Reference<JTextComponent> tc) {
        this.index = index;
        this.ci = ci;
        this.tc = tc;
        this.fil = FixImportsLayout.create();
    }

    public void resolve(ImportsModel model, Element e) {
        Set<ClassIndex.SearchScope> set = new TreeSet<ClassIndex.SearchScope>();
        set.add(ClassIndex.SearchScope.DEPENDENCIES);
        set.add(ClassIndex.SearchScope.SOURCE);
        final Set<ElementHandle<TypeElement>> result = prefilterResult(index.getDeclaredTypes(e.getSimpleName().toString(), ClassIndex.NameKind.SIMPLE_NAME, set));
        if (result.size() == 1) {
            ElementHandle<TypeElement> handle = result.iterator().next();
            Name qname = handle.resolve(ci).getQualifiedName();
            model.append(new ModelEntry(qname.toString()));
        } else if (!result.isEmpty()) {
            moveCaret(e);
            display(result, getHeaderText(e), model);
        }

    }


    public void resolve(ImportsModel model, Collection<Element> elements) {
        this.queue = new ArrayList<Element>(elements);

    }


    private class QueueChain implements Runnable, FixItemHandler {
        private final ImportsModel model;
        private final Element e;

        private QueueChain(ImportsModel model, Element e) {
            this.model = model;
            this.e = e;
        }

        /**
         * When an object implementing interface <code>Runnable</code> is used
         * to create a thread, starting the thread causes the object's
         * <code>run</code> method to be called in that separately executing
         * thread.
         * <p/>
         * The general contract of the method <code>run</code> is that it may
         * take any action whatsoever.
         *
         * @see Thread#run()
         */
        public void run() {
//            ModelEntryResolver.this.resolve(model, e);
            Set<ClassIndex.SearchScope> set = new TreeSet<ClassIndex.SearchScope>();
            set.add(ClassIndex.SearchScope.DEPENDENCIES);
            set.add(ClassIndex.SearchScope.SOURCE);
            final Set<ElementHandle<TypeElement>> result = prefilterResult(index.getDeclaredTypes(e.getSimpleName().toString(), ClassIndex.NameKind.SIMPLE_NAME, set));
            if (result.size() == 1) {
                ElementHandle<TypeElement> handle = result.iterator().next();
                Name qname = handle.resolve(ci).getQualifiedName();
                model.append(new ModelEntry(qname.toString()));
            } else if (!result.isEmpty()) {
                moveCaret(e);
                fil.setEditorComponent(tc.get());
                List<FixItem> items = createItems(result, fil, model);
                fil.show(items, getHeaderText(e), 0, new MyListSelectionListener(), null, null, 0);
            }
        }

        public void defaultAction(JTextComponent component, FixItem item) {
            fil.hide();
            model.append(new ModelEntry(item.getElement()));
            if (!queue.isEmpty()) {
                Element element = queue.remove(0);
                SwingUtilities.invokeLater(new QueueChain(model, element));
            }
        }
    }


    private Set<ElementHandle<TypeElement>> prefilterResult(Set<ElementHandle<TypeElement>> result) {
        Iterator<ElementHandle<TypeElement>> handleIterator = result.iterator();
        while (handleIterator.hasNext()) {
            ElementHandle<TypeElement> handle = handleIterator.next();
            if (handle.getKind() != ElementKind.CLASS) {
                handleIterator.remove();
            }
        }
        return result;
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

    private void display(final Set<ElementHandle<TypeElement>> options, String text, ImportsModel model) {
        // this code is intended to run in EDT.

    }

    static ModelEntryResolver create(ClassIndex index, CompilationInfo ci, Reference<JTextComponent> tc) {
        return new ModelEntryResolver(index, ci, tc);
    }

    private static class MyListSelectionListener implements ListSelectionListener {
        public void valueChanged(ListSelectionEvent e) {
        }
    }

    private List<FixItem> createItems(Set<ElementHandle<TypeElement>> options, FixImportsLayout<FixItem> fil, ImportsModel model) {
        List<FixItem> result = new ArrayList<FixItem>(options.size());
        for (ElementHandle<TypeElement> option : options) {
            String qn = option.getQualifiedName();
            result.add(new FixItem(qn, model, fil));
        }
        Collections.sort(result, FixItemComparator.get());
        return result;
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

}
