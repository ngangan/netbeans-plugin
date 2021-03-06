/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
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
 * Contributor(s):
 *
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.modules.visage.editor.imports;

import org.netbeans.api.visage.source.*;
import org.netbeans.editor.BaseAction;
import org.netbeans.modules.editor.MainMenuAction;
import org.netbeans.modules.visage.editor.VSGImportManager;
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
import java.util.logging.Logger;
import org.netbeans.api.progress.ProgressHandle;
import org.netbeans.api.progress.ProgressHandleFactory;
import org.openide.util.RequestProcessor;

/**
 * Algorithm:
 * * Collect all declared imports
 * * Collect all unresolved type elements
 * * Generate option sets for unresolved type elements
 * * Let the user resolve the unresolved type elements (select one type for an unresolved element)
 * * Modify document
 *
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.org">RKo</a>)
 * @author Jaroslav Bachorik
 */
public final class VisageImports extends BaseAction implements VSGImportManager {


    private static VisageImports instance;
    public static final Logger logger = Logger.getLogger(VSGImportManager.class.getName());

    final private RequestProcessor taskQueue = new RequestProcessor(VisageImports.class.getName(), 1);

    public synchronized static VisageImports getInstance() {
        if (instance == null) {
            instance = new VisageImports();
        }
        return instance;
    }

    private VisageImports() {
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
        return NbBundle.getBundle(VisageImports.class).getString(fixImportsAction);
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
        taskQueue.post(runnable);
    }

    private Runnable prepareTask(final Document document, final JTextComponent target) {
        return new Runnable() {
            private ProgressHandle wHandle = ProgressHandleFactory.createHandle(NbBundle.getMessage(VisageImports.class, "MSG_Wait")); // NOI18N
            public void run() {
                wHandle.start();
                final VisageSource s = VisageSource.forDocument(document);
                try {
                    s.runWhenScanFinished(new Task<CompilationController>() {
                        public void run(CompilationController cc) throws Exception {
                            final FileObject source = getFileObject(document);
                            if (source == null) {
                                throw new IllegalArgumentException("There is no associated fileobject for document."); // NOI18N
                            }
                            ClassIndex index = cc.getClasspathInfo().getClassIndex();
                            ImportsWalker iw = new ImportsWalker(cc, index);
                            ImportsModel imn = new ImportsModel();
                            iw.scan(cc.getCompilationUnit(), imn);
                            ImportResolverImpl ir = ImportResolverImpl.create(cc, target);
                            if (wHandle != null) {
                                wHandle.finish();
                                wHandle = null;
                            }
                            ir.resolve(imn);
                        }

                    }, true);
                } catch (IOException e) {
                    throw new IllegalArgumentException(NbBundle.getBundle(VisageImports.class).getString("FI-cannot-continue"), e); // NOI18N
                } finally {
                    if (wHandle != null) {
                        wHandle.finish();
                    }
                }

            }
        };
    }

    public static final String fixImportsAction = "fix-imports"; // NOI18N

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

    static class InternalSetComparator implements Comparator<Element> {
        public static final Collator collator = Collator.getInstance();
        static SoftReference<InternalSetComparator> instance;

        private InternalSetComparator() {
        }

        public int compare(Element o1, Element o2) {
            return collator.compare(o1.getSimpleName().toString(), o2.getSimpleName().toString());
        }

        static InternalSetComparator create() {
            if (instance == null || instance.get() == null) {
                instance = new SoftReference<InternalSetComparator>(new InternalSetComparator());
            }
            return instance.get();
        }
    }
}
