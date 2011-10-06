/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Oracle and/or its affiliates. All rights reserved.
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
package org.netbeans.modules.visage.navigation;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.io.IOException;
import javax.lang.model.element.Element;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.tree.TreePath;
import org.netbeans.api.visage.editor.ElementVisagedoc;
import org.netbeans.api.visage.source.CompilationController;
import org.netbeans.api.visage.source.CompilationInfo;
import org.netbeans.api.visage.source.ElementHandle;
import org.netbeans.api.visage.source.VisageSource;
import org.netbeans.api.visage.source.Task;
import org.netbeans.modules.visage.navigation.ElementNode.Description;
import org.netbeans.modules.visage.navigation.actions.FilterSubmenuAction;
import org.netbeans.modules.visage.navigation.actions.SortActionSupport.SortByNameAction;
import org.netbeans.modules.visage.navigation.actions.SortActionSupport.SortBySourceAction;
import org.netbeans.modules.visage.navigation.base.FiltersManager;
import org.netbeans.modules.visage.navigation.base.TapPanel;
import org.openide.explorer.ExplorerManager;
import org.openide.explorer.view.BeanTreeView;
import org.openide.explorer.view.Visualizer;
import org.openide.filesystems.FileObject;
import org.openide.nodes.Node;
import org.openide.util.Exceptions;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;

/**
 *
 * @author phrebejk
 */
public class ClassMemberPanelUI extends JPanel implements ExplorerManager.Provider, FiltersManager.FilterChangeListener, PropertyChangeListener {

    private ExplorerManager manager = new ExplorerManager();
    private MyBeanTreeView elementView;
    private TapPanel filtersPanel;
    private InstanceContent selectedNodes = new InstanceContent();
    private Lookup lookup = new AbstractLookup(selectedNodes);
    private ClassMemberFilters filters;
    private Action[] actions; // General actions for the panel
    private static final Rectangle ZERO = new Rectangle(0, 0, 1, 1);

    public ClassMemberPanelUI() {
        manager.addPropertyChangeListener(this);

        // Tree view of the elements
        elementView = createBeanTreeView();
        setLayout(new BorderLayout());
        add(elementView, BorderLayout.CENTER);

        // filters
        filtersPanel = new TapPanel();
        filtersPanel.setOrientation(TapPanel.DOWN);
        // tooltip
        KeyStroke toggleKey = KeyStroke.getKeyStroke(KeyEvent.VK_T,
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
        String keyText = Utilities.keyToString(toggleKey);
        filtersPanel.setToolTipText(NbBundle.getMessage(ClassMemberPanelUI.class, "TIP_TapPanel", keyText)); //NOI18N

        filters = new ClassMemberFilters(this);
        filters.getInstance().hookChangeListener(this);
        JComponent buttons = filters.getComponent();
        buttons.setBorder(BorderFactory.createEmptyBorder(0, 5, 5, 0));
        filtersPanel.add(buttons);

        actions = new Action[]{
                    new SortByNameAction(filters),
                    new SortBySourceAction(filters),
                    null,
                    new FilterSubmenuAction(filters.getInstance())
                };

        add(filtersPanel, BorderLayout.SOUTH);

        manager.setRootContext(ElementNode.getWaitNode());

    }

    @Override
    public boolean requestFocusInWindow() {
        boolean result = super.requestFocusInWindow();
        elementView.requestFocusInWindow();
        return result;
    }

    public org.openide.util.Lookup getLookup() {
        // XXX Check for chenge of FileObject
        return lookup;
    }

    public ElementScanningTask getTask() {

        return new ElementScanningTask(this);

    }

    public void showWaitNode() {
        SwingUtilities.invokeLater(new Runnable() {

            public void run() {
                elementView.setRootVisible(true);
                manager.setRootContext(ElementNode.getWaitNode());
            }
        });
    }

    public void selectElementNode( ElementHandle<Element> eh ) {
        ElementNode root = getRootNode();
        if ( root == null ) {
            return;
        }
        ElementNode node = root.getNodeForElement(eh);
        try {
            manager.setSelectedNodes(new Node[]{ node == null ? getRootNode() : node });
        } catch (PropertyVetoException propertyVetoException) {
            Exceptions.printStackTrace(propertyVetoException);
        }
    }

    public void refresh(final Description description, final CompilationInfo compilationInfo) {
        final ElementNode rootNode = getRootNode();

        if (rootNode != null && rootNode.getDescritption().fileObject.equals(description.fileObject)) {
            // update
            //System.out.println("UPDATE ======" + description.fileObject.getName() );
            SwingUtilities.invokeLater(new Runnable() {

                public void run() {
                    rootNode.updateRecursively(description);
                }
            });
        } else {
            //System.out.println("REFRES =====" + description.fileObject.getName() );
            // New fileobject => refresh completely
            SwingUtilities.invokeLater(new Runnable() {

                public void run() {
                    elementView.setRootVisible(false);
                    manager.setRootContext(new ElementNode(description, compilationInfo));
                    boolean scrollOnExpand = getScrollOnExpand();
                    setScrollOnExpand(false);
                    elementView.expandAll();
                    setScrollOnExpand(scrollOnExpand);
                }
            });
        }
    }

    public void sort() {
        ElementNode root = getRootNode();
        if (null != root) {
            root.refreshRecursively();
        }
    }

    public ClassMemberFilters getFilters() {
        return filters;
    }

    public void expandNode(Node n) {
        elementView.expandNode(n);
    }

    public Action[] getActions() {
        return actions;
    }

    public FileObject getFileObject() {
        return getRootNode().getDescritption().fileObject;
    }
    
    // FilterChangeListener ----------------------------------------------------
    public void filterStateChanged(ChangeEvent e) {
        ElementNode root = getRootNode();

        if (root != null) {
            root.refreshRecursively();
        }
    }

    boolean getScrollOnExpand() {
        return null == elementView ? true : elementView.getScrollOnExpand();
    }

    void setScrollOnExpand(boolean scroll) {
        if (null != elementView) {
            elementView.setScrollOnExpand(scroll);
        }
    }

    // Private methods ---------------------------------------------------------
    private ElementNode getRootNode() {

        Node n = manager.getRootContext();
        if (n instanceof ElementNode) {
            return (ElementNode) n;
        } else {
            return null;
        }
    }

    private MyBeanTreeView createBeanTreeView() {
        return new MyBeanTreeView();
    }
    
    // ExplorerManager.Provider imlementation ----------------------------------
    public ExplorerManager getExplorerManager() {
        return manager;
    }

    protected ElementVisagedoc getJavaDocFor(ElementNode node) {
        ElementNode root = getRootNode();
        if (root == null) {
            return null;
        }

        ElementHandle<? extends Element> eh = node.getDescritption().elementHandle;

        final VisageSource js = VisageSource.forFileObject(root.getDescritption().fileObject);
        if (js == null) {
            return null;
        }
        JavaDocCalculator calculator = new JavaDocCalculator(eh);

        try {
            js.runUserActionTask(calculator, true);
        } catch (IOException ioE) {
            Exceptions.printStackTrace(ioE);
            return null;
        }

        return calculator.doc;

    }

    private static class JavaDocCalculator implements Task<CompilationController> {

        private ElementHandle<? extends Element> handle;
        private ElementVisagedoc doc;

        public JavaDocCalculator(ElementHandle<? extends Element> handle) {
            this.handle = handle;
        }

        public void run(CompilationController cc) throws Exception {
            cc.toPhase(VisageSource.Phase.UP_TO_DATE);

            Element e = null;
            try {
                e = handle.resolve(cc);
            } catch (Exception ex) {
                // can't convert to element (incomplete element)
            }
            if (e == null) {
                return;
            }
            doc = ElementVisagedoc.create(cc, e);
        }
    };

    private class MyBeanTreeView extends BeanTreeView implements ToolTipManagerEx.ToolTipProvider {
//    private class MyBeanTreeView extends BeanTreeView {
        public MyBeanTreeView() {
            new ToolTipManagerEx(this);
        }

        public boolean getScrollOnExpand() {
            return tree.getScrollsOnExpand();
        }

        public void setScrollOnExpand(boolean scroll) {
            this.tree.setScrollsOnExpand(scroll);
        }

        public JComponent getComponent() {
            return tree;
        }

        public String getToolTipText(Point loc) {
            ElementVisagedoc doc = getDocumentation(loc);
            return null == doc ? null : doc.getText();
        }

        private ElementVisagedoc getDocumentation(Point loc) {
            TreePath path = tree.getPathForLocation(loc.x, loc.y);
            if (null == path) {
                return null;
            }
            Node node = Visualizer.findNode(path.getLastPathComponent());
            if (node instanceof ElementNode) {
                return getJavaDocFor((ElementNode) node);
            }
            return null;
        }

        public Rectangle getToolTipSourceBounds(Point loc) {
            ElementNode root = getRootNode();
            if (root == null) {
                return null;
            }
            TreePath path = tree.getPathForLocation(loc.x, loc.y);
            return null == path ? null : tree.getPathBounds(path);
        }

        public Point getToolTipLocation(Point mouseLocation, Dimension tipSize) {
            Point screenLocation = getLocationOnScreen();
            Rectangle sBounds = getGraphicsConfiguration().getBounds();
            Dimension compSize = getSize();
            Point res = new Point();
            Rectangle tooltipSrcRect = getToolTipSourceBounds(mouseLocation);

            Point viewPosition = getViewport().getViewPosition();
            screenLocation.x -= viewPosition.x;
            screenLocation.y -= viewPosition.y;

            //first try bottom right
            res.x = screenLocation.x + compSize.width;
            res.y = screenLocation.y + tooltipSrcRect.y + tooltipSrcRect.height;

            if (res.x + tipSize.width <= sBounds.x + sBounds.width && res.y + tipSize.height <= sBounds.y + sBounds.height) {
                return res;
            }

            //upper right
            res.x = screenLocation.x + compSize.width;
            res.y = screenLocation.y + tooltipSrcRect.y - tipSize.height;

            if (res.x + tipSize.width <= sBounds.x + sBounds.width && res.y >= sBounds.y) {
                return res;
            }

            //lower left
            res.x = screenLocation.x - tipSize.width;
            res.y = screenLocation.y + tooltipSrcRect.y;

            if (res.x >= sBounds.x && res.y + tipSize.height <= sBounds.y + sBounds.height) {
                return res;
            }

            //upper left
            res.x = screenLocation.x - tipSize.width;
            res.y = screenLocation.y + tooltipSrcRect.y + tooltipSrcRect.height - tipSize.height;

            if (res.x >= sBounds.x && res.y >= sBounds.y) {
                return res;
            }

            //give up (who's got such a small display anyway?)
            res.x = screenLocation.x + tooltipSrcRect.x;
            if (sBounds.y + sBounds.height - (screenLocation.y + tooltipSrcRect.y + tooltipSrcRect.height) > screenLocation.y + tooltipSrcRect.y - sBounds.y) {
                res.y = screenLocation.y + tooltipSrcRect.y + tooltipSrcRect.height;
            } else {
                res.y = screenLocation.y + tooltipSrcRect.y - tipSize.height;
            }

            return res;
        }

        public void invokeUserAction(final MouseEvent me) {
            SwingUtilities.invokeLater(new Runnable() {

                public void run() {
                    if (null != me) {
                        ElementVisagedoc doc = getDocumentation(me.getPoint());
                        VisagedocTopComponent tc = VisagedocTopComponent.findInstance();
                        if (null != tc) {
                            tc.open();
                            tc.setJavadoc(doc);
                            tc.requestActive();
                        }
                    }
                }
            });
        }
        
        //#123940 start
        private boolean inHierarchy;
        private boolean doExpandAll;

        @Override
        public void addNotify() {
            super.addNotify();

            inHierarchy = true;

            if (doExpandAll) {
                super.expandAll();
                doExpandAll = false;
            }
        }

        @Override
        public void removeNotify() {
            super.removeNotify();

            inHierarchy = false;
        }

        @Override
        public void expandAll() {
            super.expandAll();

            if (!inHierarchy) {
                doExpandAll = true;
            }
        }
        //#123940 end
    }

    public void propertyChange(PropertyChangeEvent evt) {
        if (ExplorerManager.PROP_SELECTED_NODES.equals(evt.getPropertyName())) {
            for (Node n : (Node[]) evt.getOldValue()) {
                selectedNodes.remove(n);
            }
            for (Node n : (Node[]) evt.getNewValue()) {
                selectedNodes.add(n);
            }
        }
    }
    
}
