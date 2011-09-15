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
package org.netbeans.modules.javafx.fxd.composer.model.actions;

import java.awt.AWTEvent;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;
import javax.swing.Action;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.netbeans.modules.javafx.fxd.composer.model.FXDElement;
import org.netbeans.modules.javafx.fxd.composer.model.FXDElementOutline;
import org.netbeans.modules.javafx.fxd.composer.source.SourceTopComponent;

/**
 *
 * @author Pavel Benes
 */
public final class SelectActionFactory extends AbstractComposerActionFactory {
   // private static final Color SELECTION_BODY_COLOR = new Color( 64, 64, 255, 64);
   // private static final Color SELECTION_OUTLINE_COLOR = new Color( 64, 64, 255, 128);
    
    public static interface SelectionListener {
        public void selectionChanged( FXDElement [] newSelection, FXDElement [] oldSelection);
    }

    public class PreviousSelectionAction extends AbstractFXDAction {
        public PreviousSelectionAction() {  
            super("prev_sel", false);  //NOI18N
        }        
        public void actionPerformed(ActionEvent e) {
            if ( m_selectionHistoryIndex > 0){
                FXDElement selection = m_selectionHistory.get(--m_selectionHistoryIndex);
                setSelection(selection, false);
                updateSelectionHistoryButtons();
            }
        }
    };      

    public class NextSelectionAction extends AbstractFXDAction {
        public NextSelectionAction() {  
            super("next_sel", false, 1); //NOI18N
        }        
    
        public void actionPerformed(ActionEvent e) {
            if ( m_selectionHistoryIndex < m_selectionHistory.size() - 1){
                FXDElement selection = m_selectionHistory.get(++m_selectionHistoryIndex);
                setSelection(selection, false);
                updateSelectionHistoryButtons();
            }
        }
    };            

    public class ParentSelectionAction extends AbstractFXDAction {
        public ParentSelectionAction() { 
            super("parent_sel", false, 2);   //NOI18N
        }
    
        public void actionPerformed(ActionEvent e) {
            FXDElement [] selected = getSelection();
            if (selected != null && selected.length > 0) {
                assert selected[0] != null;
                FXDElement parent = selected[0].getVisibleParent();
                if (parent != null) {
                    setSelection(parent, true);
                }                           
            }
        }
    };            
    
    
    final class SelectAction extends AbstractComposerAction {
        private FXDElement [] m_selected;
        
        public SelectAction(final FXDElement [] selected) {
            m_selected = selected;
            FXDElement.repaint(m_selected, FXDElementOutline.SELECTOR_OVERLAP);
        }

        @Override
        public boolean consumeEvent(AWTEvent evt, boolean isOutsideEvent) {
            assert getController().getActionController().containsAction(SelectAction.class);

            if ( SelectActionFactory.getSelectionEvent(evt) != null) {
                actionCompleted();
            }
            return false;
        }

        public void paint(Graphics g) {
            if ( !isCompleted()) {
                for ( FXDElement elem : m_selected) {
                    if ( elem.isVisible() && !elem.isDeleted()) {
                        /*
                        System.err.println("Painting selected: " + elem);
                        FXDElementOutline outline = elem.getOutline();
                        outline.highlight(g, x, y, SELECTION_BODY_COLOR);
                        outline.draw(g, x, y, SELECTION_OUTLINE_COLOR, false);
                         */
                        elem.getOutline().draw(g, 0, 0, FXDElementOutline.SELECTOR_BODY, true);                        
                    }
                }
            }        
        }

        @Override
        public void actionCompleted() {
            FXDElement.repaint(m_selected, FXDElementOutline.SELECTOR_OVERLAP);
            super.actionCompleted();
        }
    }
    
    private final PreviousSelectionAction m_navigateBackAction    = new PreviousSelectionAction();
    private final AbstractFXDAction       m_navigateForwardAction = new NextSelectionAction();
    private final AbstractFXDAction       m_navigateUpAction      = new ParentSelectionAction();    
    private final List<FXDElement>          m_selectionHistory      = new ArrayList<FXDElement>();
    private final List<SelectionListener> m_selectionListeners;
    private int                           m_selectionHistoryIndex = -1;
    private       SelectAction            m_activeAction;
            
    SelectActionFactory(FXZDataObject dObj) {
        super(dObj);
        m_selectionListeners = new ArrayList<SelectionListener>();
    }

    public static MouseEvent getSelectionEvent(AWTEvent evt) {
        if ( evt.getID() == MouseEvent.MOUSE_CLICKED) {
            MouseEvent me = (MouseEvent)evt;
            if ( me.getButton() == MouseEvent.BUTTON1 && me.getClickCount() > 0) {
                return me;
            }
        }
        return null;
    }
        
    @Override
    public synchronized ComposerAction startAction(AWTEvent e, boolean isOutsideEvent) {        
        MouseEvent me;
        
        if (!isOutsideEvent && (me=getSelectionEvent(e)) != null) {
            FXDElement elem = m_dObj.getController().getElementAt(me.getX(), me.getY());

            if (elem != null)  {
                if (me.getClickCount() > 1) {
                    SourceTopComponent.selectElement(m_dObj, elem.getStartOffset(), true);
                } 
                
                return select( new FXDElement[] { elem}, true);
            }    
        }
        return null;
    }
    
    public FXDElement [] getSelection() {
        SelectAction active = getActiveAction();
        return active != null ? active.m_selected : null;
    }
    
    public void setSelection( FXDElement elem, boolean updateHistory) {
        SelectAction action = select( new FXDElement[] {elem}, updateHistory);
        getController().getActionController().getActiveActions().push(action);        
    }
    
    public void addSelectionListener( SelectionListener listener) {
        m_selectionListeners.add(listener);
    }

    public void removeSelectionListener( SelectionListener listener) {
        m_selectionListeners.remove(listener);
    }
        
    protected void fireSelectionChanged(FXDElement [] newSelection, FXDElement [] oldSelection) {
        for (SelectionListener listener : m_selectionListeners) {
            listener.selectionChanged(newSelection, oldSelection);
        }
    }    
    
    SelectAction select( FXDElement [] newSelection, boolean updateHistory) { 
        if ( updateHistory && newSelection != null && newSelection.length > 0) {
            updateSelectionHistory(newSelection[0]);
        }
        FXDElement [] oldSelection = getSelection();
                
        SelectAction action = new SelectAction( newSelection);
        setActiveAction(action);
        
        if (oldSelection != null) {
            for (int i = 0; i < oldSelection.length; i++) {
                m_dObj.getLookupContent().remove(oldSelection[i]);
            }
        }

        if (newSelection != null && newSelection.length > 0) {
            for (int i = 0; i < newSelection.length; i++) {
                m_dObj.getLookupContent().add(newSelection[i]);
            }
        }  
        boolean navigationUpEnabled = false;
        
        if ( newSelection != null && newSelection.length > 0) {
            if ( newSelection[0].hasParent()) {
                navigationUpEnabled = true;
            }
        }
        m_navigateUpAction.setEnabled(navigationUpEnabled);
        updateSelectionHistoryButtons();
        
        fireSelectionChanged(newSelection, oldSelection);
        return action;
    }
    
    private SelectAction getActiveAction() {
        if (m_activeAction != null && !m_activeAction.isCompleted()) {
            return m_activeAction;
        } else {
            return null;
        }
    }
    
    @Override
    public Action [] getMenuActions() {
        return new Action [] { m_navigateBackAction, m_navigateForwardAction, m_navigateUpAction};
    }
    
    private void setActiveAction( SelectAction active) {
        if (m_activeAction != null) {
            m_activeAction.actionCompleted();
        }
        m_activeAction = active;
    }
    
    private void updateSelectionHistory(FXDElement elem) {
        for (int i = m_selectionHistory.size() - 1; i > m_selectionHistoryIndex; i--) {
            m_selectionHistory.remove(i);
        }
        m_selectionHistoryIndex = m_selectionHistory.size();
        m_selectionHistory.add(elem);
        updateSelectionHistoryButtons();
    }
    
    private void updateSelectionHistoryButtons() {
        m_navigateBackAction.setEnabled( m_selectionHistoryIndex > 0);
        m_navigateForwardAction.setEnabled( m_selectionHistoryIndex < m_selectionHistory.size() - 1);
    }    
}
