/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
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
package org.netbeans.modules.javafx.fxd.composer.navigator;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.lang.ref.WeakReference;
import java.util.WeakHashMap;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.editor.structure.api.DocumentModel;
import org.netbeans.modules.editor.structure.api.DocumentModelListener;
import org.netbeans.modules.javafx.fxd.composer.model.FXDElement;
import org.netbeans.modules.javafx.fxd.composer.model.FXDFileModel;
import org.netbeans.modules.javafx.fxd.composer.model.actions.SelectActionFactory;
import org.netbeans.modules.javafx.fxd.composer.source.FXDSourceEditor;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.openide.nodes.Node.Cookie;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.RequestProcessor;
import org.openide.windows.TopComponent;

/**
 * @author Marek Fukala
 * @author Pavel Benes
 */
class FXDNavigatorContent extends JPanel implements SelectActionFactory.SelectionListener {
    public static final String ATTRIBUTES_FILTER = "attrs"; //NOI18N
    public static final String ID_FILTER         = "id";    //NOI18N
    
    //private static final boolean DEBUG = false;
    private static FXDNavigatorContent s_navigatorContentInstance = null;
    
    public static synchronized FXDNavigatorContent getDefault() {
        if(s_navigatorContentInstance == null) {
            s_navigatorContentInstance = new FXDNavigatorContent();
        }
        return s_navigatorContentInstance;
    }
    
    private final JPanel                 emptyPanel;    
    private final JLabel                 msgLabel;    
    private       FXZDataObject          peerDO = null;
    private       NavigatorContentPanel  navigatorPanel = null; 
    private       boolean                blockNotification = false;
        
    private FXDNavigatorContent() {
        setLayout(new BorderLayout());
        //init empty panel
        setBackground(Color.WHITE);
        emptyPanel = new JPanel();
        emptyPanel.setBackground(Color.WHITE);
        emptyPanel.setLayout(new BorderLayout());
        msgLabel = new JLabel();
        emptyPanel.add(msgLabel, BorderLayout.CENTER);
    }

    public void selectionChanged( FXDElement [] newSelection, FXDElement [] oldSelection) {
        blockNotification = true;
        try {
            select( newSelection != null && newSelection.length > 0 ? newSelection[0] : null);
        } finally {
            blockNotification = false;
        }
    }

    private final WeakHashMap uiCache = new WeakHashMap();
    
    public synchronized void navigate(final FXZDataObject dObj) throws Exception { 
        if (dObj != peerDO) {
            if (peerDO != null) {
                //peerDO.getSceneManager().removeSelectionListener(this);
            }
            peerDO = dObj;
                        
            //try to find the UI in the UIcache
            final NavigatorContentPanel cachedPanel;
            WeakReference panelWR = (WeakReference)uiCache.get(dObj);
            if(panelWR != null) {
                NavigatorContentPanel cp = (NavigatorContentPanel)panelWR.get();
                if(cp != null) {
                    //System.out.println("panel is cached");  //NOI18N
                    //test if the document associated with the panel is the same we got now
                    if (cp.m_docModel == FXDFileModel.getDocumentModel(dObj)) {
                        cachedPanel = cp;
                    } else {
                        //System.out.println("but the document is different - creating a new UI...");
                        //remove the old mapping from the cache
                        uiCache.remove(dObj);
                        cachedPanel = null;
                    }
                } else {
                    cachedPanel = null;
                }
            } else {
                cachedPanel = null;
            }
        
            if (dObj != null) {
                //TODO check if the listener needs to be removed
                dObj.getController().getActionController().getSelectionModel().addSelectionListener( this);
                
                RequestProcessor.getDefault().post(new Runnable() {
                    public void run() {
                        showWaitPanel();
                        try {
                            while( FXDFileModel.getDocumentModel(dObj) == null) {
                                try {
                                    //System.err.println("Waiting for document to come up ...");  //NOI18N
                                    Thread.sleep(15);
                                } catch (InterruptedException ex) {
                                    ex.printStackTrace();
                                } 
                            }
                        } catch( Exception e) {
                            e.printStackTrace();
                            showCannotNavigate();
                            return;
                        }
                        
                        final NavigatorContentPanel panel;
                        if ( cachedPanel == null) {
                            try {
                                //cache the newly created panel
                                panel = new NavigatorContentPanel(dObj);
                                uiCache.put(dObj, new WeakReference(panel));
                            } catch (Exception ex) {
                                System.err.println(ex.getClass().getName() + " " + ex.getLocalizedMessage());  //NOI18N
                                ex.printStackTrace();
                                //SceneManager.log(Level.SEVERE, "Navigator panel creation failed", ex); //NOI18N
                                showCannotNavigate();
                                return;
                            }
                        } else {
                            panel = cachedPanel;
                        }
                        SwingUtilities.invokeLater(new Runnable() {
                            public void run() {
                                setContent(dObj, panel);
                            }
                        });
                    }
                });        
            } else {
                setContent(null, null);
            }
        }
    }

    synchronized void setContent( final FXZDataObject obj, final NavigatorContentPanel panel) {
        navigatorPanel = panel;
                
        removeAll();
        if (panel != null) {
            add(panel, BorderLayout.CENTER);  
        }
        validate();                                
        repaint();                                
    }
    
    synchronized void select(FXDElement elem) {
//        System.err.println( String.format("Selecting id %s in the navigator", elemId));
        if (navigatorPanel != null) {
            navigatorPanel.tree.selectNode(elem);
        }
    }

    public void release() {
        removeAll();
        peerDO = null;
        repaint();
    }
    
    public void showCannotNavigate() {
        removeAll();
        msgLabel.setIcon(null);
        msgLabel.setForeground(Color.GRAY);
        msgLabel.setText(NbBundle.getMessage(FXDNavigatorContent.class, "LBL_CannotNavigate")); //NOI18N
        msgLabel.setHorizontalAlignment(SwingConstants.CENTER);
        add(emptyPanel, BorderLayout.CENTER);
        repaint();
    }
    
    private void showWaitPanel() {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                removeAll();
                msgLabel.setIcon(null);
                msgLabel.setForeground(Color.GRAY);
                msgLabel.setHorizontalAlignment(SwingConstants.LEFT);
                msgLabel.setText(NbBundle.getMessage(FXDNavigatorContent.class, "LBL_Wait")); //NOI18N
                add(emptyPanel, BorderLayout.NORTH);
                repaint();
            }
        });
    }

     private class NavigatorContentPanel extends JPanel implements FiltersManager.FilterChangeListener {
        private final FXZDataObject    m_doj;
        private final DocumentModel    m_docModel;
        private final FXDNavigatorTree tree;
        private final FiltersManager   m_filters;

        private final DocumentModelListener m_modelListener = new DocumentModelListener() {
            public void documentElementAdded(DocumentElement de) {
                modelChanged();
            }
            public void documentElementRemoved(DocumentElement de) {
                modelChanged();
            }
            public void documentElementChanged(DocumentElement de) {
                modelChanged();
            }
            public void documentElementAttributesChanged(DocumentElement de) {
                modelChanged();
            }
            protected void modelChanged() {
                tree.repaint();
            }
        };       
        
        private String getElementId(DocumentElement de) {
            if ( de.getStartOffset() < de.getEndOffset()) {
                return FXDFileModel.getIdAttribute(de);
            } else {
                //TODO
                System.err.println("Deleted element found: " + de);  //NOI18N
                //SceneManager.log(Level.SEVERE, "Deleted element found: " + de); //NOI18N
                return null;
            }
        }
        
        public NavigatorContentPanel(FXZDataObject doj) throws Exception {
            m_doj = doj;
            setLayout(new BorderLayout());
            
            m_docModel = FXDFileModel.getDocumentModel(doj);
            //create the JTree pane
            tree = new FXDNavigatorTree(doj, m_docModel);
            ToolTipManager.sharedInstance().registerComponent(tree);
            
            MouseListener ml = new MouseAdapter() {
                @Override
                public void mousePressed(MouseEvent e) {
                    int selRow = tree.getRowForLocation(e.getX(), e.getY());
                    if(selRow != -1 && e.getClickCount() == 2) {
                        TreePath         selPath = tree.getPathForLocation(e.getX(), e.getY());
                        FXDNavigatorNode tna     = (FXDNavigatorNode)selPath.getLastPathComponent();                       
                        DocumentElement  de      = tna.getDocumentElement();
                        FXDSourceEditor.selectElement(m_doj, de.getStartOffset(), true);
                    }
                }
            };
            tree.addMouseListener(ml);
            
            final TreeSelectionModel selectionModel = new DefaultTreeSelectionModel();
            selectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
            tree.setSelectionModel(selectionModel);
            
            selectionModel.addTreeSelectionListener( new TreeSelectionListener() { 
                public void valueChanged(TreeSelectionEvent e) {
                    if ( !blockNotification) {
                        TreePath selPath = e.getPath();
                        String   id      = null;

                        if ( selPath != null && !selectionModel.isSelectionEmpty()) {
                            FXDNavigatorNode tna     = (FXDNavigatorNode)selPath.getLastPathComponent();                       
                            DocumentElement  de      = tna.getDocumentElement();
                            id     = getElementId(de);
                        }
                        TopComponent tc = m_doj.getMTVC();

//                        System.err.println("TC: " + tc);

                        if ( tc != null) {
                            Lookup           lkp    = tc.getLookup();                                
                            SelectionCookie  cookie = lkp.lookup(SelectionCookie.class);
                            if ( cookie != null && id != null) {
  //                              System.err.println("Selection cookie found.");
                                cookie.updateSelection(m_doj, new FXDElement(m_doj, id), false);
                            } else {
    //                            System.err.println("Selection cookie not found.");
                            }
                        }
                    }
                }
            });
                        
            JScrollPane treeView = new JScrollPane(tree);
            treeView.setBorder(BorderFactory.createEmptyBorder());
            treeView.setViewportBorder(BorderFactory.createEmptyBorder());
            add(treeView, BorderLayout.CENTER);
            
            //add popup menu mouse listener
            MouseListener pmml = new MouseAdapter() {
                @Override
                public void mousePressed(final MouseEvent e) {
                    /*
                    if(e.getClickCount() == 1 && e.getModifiers() == MouseEvent.BUTTON3_MASK) {
                        //show popup
                        JPopupMenu pm = new JPopupMenu();
                        
                        final AnimationCookie animCookie = (AnimationCookie) getCookie(AnimationCookie.class);                        
                        final DocumentElement de         = getElementAt(e.getX(), e.getY());
                        boolean         isReadOnly       = m_doj.getSceneManager().isReadOnly();
                        
                        if (animCookie != null && de != null && 
                            SVGFileModel.isAnimation(de)) {

                            JMenuItem animStart = new JMenuItem(NbBundle.getMessage(SVGNavigatorContent.class, "LBL_AnimStart")); //NOI18N
                            animStart.addActionListener(new ActionListener() {
                                public void actionPerformed(ActionEvent evt) {
                                    String id = getElementId(de);
                                    if ( id != null) {
                                        animCookie.startAnimation(m_doj, id);
                                    }
                                }
                            });
                            animStart.setEnabled(isReadOnly);
                            pm.add(animStart);

                            JMenuItem animStop = new JMenuItem(NbBundle.getMessage(SVGNavigatorContent.class, "LBL_AnimStop")); //NOI18N
                            animStop.addActionListener(new java.awt.event.ActionListener() {
                                public void actionPerformed(ActionEvent evt) {
                                    String id = getElementId(de);
                                    if ( id != null) {
                                        animCookie.stopAnimation(m_doj, id);
                                    }
                                }
                            });

                            animStop.setEnabled(isReadOnly);
                            pm.add(animStop);
                        }
                        
                        JMenuItem[] items = new FilterActions(m_filters).createMenuItems();
                        //add filter actions
                        for(int i = 0; i < items.length; i++) {
                            pm.add(items[i]);
                        }
                        pm.pack();
                        pm.show(tree, e.getX(), e.getY());
                    }
                */
                }
            };
            tree.addMouseListener(pmml);
            
            //expand all root elements which are tags
            TreeNode rootNode = (TreeNode)tree.getTreeModel().getRoot();
            for(int i = 0; i < rootNode.getChildCount(); i++) {
                TreeNode node = rootNode.getChildAt(i);
                if(node.getChildCount() > 0)
                    tree.expandPath(new TreePath(new TreeNode[]{rootNode, node}));
            }
                                
            //create the TapPanel
            TapPanel filtersPanel = new TapPanel();
            JLabel filtersLbl = new JLabel(NbBundle.getMessage(FXDNavigatorContent.class, "LBL_Filter")); //NOI18N
            filtersLbl.setBorder(new EmptyBorder(0, 5, 5, 0));
            filtersPanel.add(filtersLbl);
            filtersPanel.setOrientation(TapPanel.DOWN);
            // tooltip
            KeyStroke toggleKey = KeyStroke.getKeyStroke(KeyEvent.VK_T,
                    Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
            String keyText = org.openide.util.Utilities.keyToString(toggleKey);
            filtersPanel.setToolTipText(NbBundle.getMessage(FXDNavigatorContent.class, "TIP_TapPanel", keyText)); //NOI18N
            
            //create FiltersManager
            m_filters = createFilters();
            //listen to filters changes
            m_filters.hookChangeListener(this);
            
            filtersPanel.add(m_filters.getComponent());
            
            add(filtersPanel, BorderLayout.SOUTH);
            m_docModel.addDocumentModelListener(m_modelListener);
        }        

        @SuppressWarnings("unchecked")
        protected Cookie getCookie(Class clazz) {
            Cookie       cookie = null;
            TopComponent tc     = m_doj.getMTVC();

            if ( tc != null) {
                cookie = (Cookie) tc.getLookup().lookup(clazz);
            }
            return cookie;
        }

        protected DocumentElement getElementAt( int x, int y) {
            DocumentElement de     = null;
            int             selRow = tree.getRowForLocation(x, y);
            
            if(selRow != -1) {
                TreePath         selPath = tree.getPathForLocation(x, y);
                FXDNavigatorNode tna     = (FXDNavigatorNode)selPath.getLastPathComponent();                       
                de = tna.getDocumentElement();
            }
            return de;
        }
        
        /** Creates filter descriptions and filters itself */
        private FiltersManager createFilters() {
            FiltersDescription desc = new FiltersDescription();
            
            desc.addFilter(ATTRIBUTES_FILTER,
                    NbBundle.getMessage(FXDNavigatorContent.class, "LBL_ShowAttributes"),     //NOI18N
                    NbBundle.getMessage(FXDNavigatorContent.class, "LBL_ShowAttributesTip"),     //NOI18N
                    FXDNavigatorTree.showAttributes,
                    new ImageIcon(org.openide.util.ImageUtilities.loadImage("org/netbeans/modules/javafx/fxd/composer/resources/showAttribs.png")), //NOI18N
                    null
            );
            desc.addFilter(ID_FILTER,
                    NbBundle.getMessage(FXDNavigatorContent.class, "LBL_ShowId"),     //NOI18N
                    NbBundle.getMessage(FXDNavigatorContent.class, "LBL_ShowIdTip"),     //NOI18N
                    FXDNavigatorTree.showIdOnly,
                    new ImageIcon(org.openide.util.ImageUtilities.loadImage("org/netbeans/modules/javafx/fxd/composer/resources/filterSignificant.png")), //NOI18N
                    null
            );
            
            return FiltersDescription.createManager(desc);
        }

        public void filterStateChanged(ChangeEvent e) {
            boolean filterChanged         = false;   
            boolean attrVisibilityChanged = false;
            boolean selected;
            
            selected = m_filters.isSelected(ATTRIBUTES_FILTER);
            if ( selected != FXDNavigatorTree.showAttributes) {
                FXDNavigatorTree.showAttributes = selected;
                attrVisibilityChanged = true;
            }

            if ( (selected=m_filters.isSelected(ID_FILTER)) != FXDNavigatorTree.showIdOnly) {
                filterChanged = true;
                FXDNavigatorTree.showIdOnly = selected;
            }
                        
            if (filterChanged) {
                tree.filterChanged();
            } else {
                if (attrVisibilityChanged) {
                    tree.validate();
                    tree.repaint();
                }
            }
        }
     }    
}

