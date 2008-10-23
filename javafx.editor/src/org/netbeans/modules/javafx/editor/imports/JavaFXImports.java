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
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.editor.imports;

import org.netbeans.api.javafx.source.*;
import org.netbeans.editor.BaseAction;
import org.netbeans.modules.editor.MainMenuAction;
import org.netbeans.modules.javafx.editor.JFXImportManager;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.util.NbBundle;

import javax.lang.model.element.Element;
import javax.swing.*;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.lang.ref.SoftReference;
import java.text.Collator;
import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Logger;

/**
 * Algorithm:
 * * Collect all important symbols
 * * Collect all imports.
 * * iterate over all symbols and verify if it is already imported or have to be.
 * * collect resulting set of import
 * * modify document
 *
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.org">RKo</a>)
 */
public final class JavaFXImports extends BaseAction implements JFXImportManager {


    private static JavaFXImports instance;
    public static final Logger logger = Logger.getLogger(JFXImportManager.class.getName());

    public synchronized static JavaFXImports getInstance() {
        if (instance == null) {
            instance = new JavaFXImports();
        }
        return instance;
    }

    private JavaFXImports() {
        super(fixImportsAction);
    }

    /**
     * Get the default value for {@link javax.swing.Action#SHORT_DESCRIPTION} property.
     * <br>
     * If this method returns non-empty value it will only be called once
     * (its result will be remembered).
     *
     * @return value that will be use as result for
     *         <code>Action.getValue(Action.SHORT_DESCRIPTION)</code>.
     */
    @Override
    protected Object getDefaultShortDescription() {
        return NbBundle.getBundle(JavaFXImports.class).getString(fixImportsAction);
    }

    private static FileObject getFileObject(Document doc) {
        DataObject od = (DataObject) doc.getProperty(Document.StreamDescriptionProperty);
        return od != null ? od.getPrimaryFile() : null;
    }


    /**
     * Fix imports within source code.
     *
     * @param document containing source code.
     * @param target   component to display over.
     */
    public void fixImports(final Document document, JTextComponent target) {
        Runnable runnable = prepareTask(document, target);
        SwingUtilities.invokeLater(runnable);
    }

    private Runnable prepareTask(final Document document, final JTextComponent target) {
        return new Runnable() {
            public void run() {
                final JavaFXSource s = JavaFXSource.forDocument(document);
                int caret = target.getCaret().getDot();
                try {
                    s.runUserActionTask(new Task<CompilationController>() {
                        public void run(CompilationController cc) throws Exception {
                            final JavaFXSource.Phase phase = cc.toPhase(JavaFXSource.Phase.ANALYZED);
                            if (phase.lessThan(JavaFXSource.Phase.ANALYZED)) {
                                logger.warning("We did not reach required phase. Leaving without fix");
                                return;
                            }
                            final FileObject source = getFileObject(document);
                            if (source == null) {
                                throw new IllegalArgumentException("There is no associated fileobject for document.");
                            }
                            ClassIndex index = ClasspathInfo.create(source).getClassIndex();
                            Set<Element> elements = new TreeSet<Element>(InternalSetComparator.create());
                            cc.getCompilationUnit().accept(new IdentifierVisitor(cc), elements);
                            ImportsModel model = new ImportsModel(cc);
                            ImportResolver ir = ImportResolverImpl.create(index, target);
                            ir.resolve(model, elements);
                        }

                    }, false);
                } catch (IOException e) {
                    throw new IllegalArgumentException(NbBundle.getBundle(JavaFXImports.class).getString("FI-cannot-continue"), e);
                } finally {
                    target.getCaret().setDot(caret);
                }
            }
        };
    }

    public static final String fixImportsAction = "fix-imports";

    /**
     * The target method that performs the real action functionality.
     *
     * @param evt    action event describing the action that occured
     * @param target target component where the action occured. It's retrieved
     *               by the TextAction.getTextComponent(evt).
     */
    public void actionPerformed(ActionEvent evt, JTextComponent target) {
        fixImports(target.getDocument(), target);
    }

    public static final class GlobalAction extends MainMenuAction {
        private final JMenuItem menuPresenter;

        public GlobalAction() {
            super();
            this.menuPresenter = new JMenuItem(getMenuItemText());
            setMenu();
        }

        protected String getMenuItemText() {
            return NbBundle.getBundle(GlobalAction.class).getString("fix-imports-main-menu-source-item"); //NOI18N
        }

        protected String getActionName() {
            return fixImportsAction;
        }

        public JMenuItem getMenuPresenter() {
            return menuPresenter;
        }
    }

    private static class InternalSetComparator implements Comparator<Element> {
        public static final Collator collator = Collator.getInstance();
        static SoftReference<InternalSetComparator> instance;

        private InternalSetComparator() {
        }

        public int compare(Element o1, Element o2) {
            return collator.compare(o1.getSimpleName().toString(), o2.getSimpleName().toString());
        }

        private static InternalSetComparator create() {
            if (instance == null || instance.get() == null) {
                instance = new SoftReference<InternalSetComparator>(new InternalSetComparator());
            }
            return instance.get();
        }
    }
}
