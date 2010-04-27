/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
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

package org.netbeans.modules.javafx.fxd.composer.model.actions;

import java.awt.AWTEvent;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import javax.swing.Action;
import org.netbeans.modules.javafx.fxd.composer.model.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import javax.swing.JComponent;
import org.netbeans.modules.javafx.fxd.composer.misc.ActionLookup;
import org.netbeans.modules.javafx.fxd.composer.misc.ActionLookupUtils;
import org.netbeans.modules.javafx.fxd.composer.uistub.UIStubGenerator;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;

/**
 *
 * @author Pavel Benes
 */
public class ActionController implements ActionLookup {
    private final FXZDataObject               m_dObj;
    private final List<ComposerActionFactory> m_actionFactories;    
    private final Stack<ComposerAction>       m_activeActions;
    private final SelectActionFactory         m_selectActionFactory;

    public ActionController(FXZDataObject dObj) {
        m_dObj = dObj;
        m_activeActions   = new Stack<ComposerAction>();
        // test action to check painting
//        m_activeActions.push(new AbstractComposerAction() {
//            public void paint(Graphics g) {
//                g.setColor(Color.blue);
//                g.drawPolygon(new int[]{10, 100, 110, 20}, new int[]{10, 20, 110, 100}, 4);
//            }
//        });
        m_actionFactories = new ArrayList<ComposerActionFactory>();
        
        m_actionFactories.add( m_selectActionFactory=new SelectActionFactory(dObj));
        m_actionFactories.add( new HighlightActionFactory(dObj));
    } 
    
    public Stack<ComposerAction> getActiveActions() {
        return m_activeActions;
    }

    public SelectActionFactory getSelectionModel() {
        return m_selectActionFactory;
    }
    
    public ComposerAction findAction( Class clazz) {
        synchronized( m_activeActions) {
            for (int i = m_activeActions.size() - 1; i >= 0; i--) {
                ComposerAction action = m_activeActions.get(i);
                
                if ( clazz.isInstance( action)) {
                    return action;
                }
            }
            return null;    
        }
    }
    
    public boolean containsAction( Class clazz) {
        return findAction(clazz) != null;
    }    
    
     public void processEvent(AWTEvent event) {
         FXDComposerController controller = m_dObj.getController();
         
//         if (isEnabled(Level.FINEST)) {
//             SceneManager.log(Level.FINEST, "Processing event: " + event); //NOI18N
//         }
         
         if ( !controller.isBusy()) {
            boolean isOutsideEvent = event.getSource() != controller.getScenePanel();

            //first let ongoing actions to process the event         
            boolean consumed = false;
            ActionMouseCursor cursor = null;

            synchronized( m_activeActions) {
                for (int i = m_activeActions.size() - 1; i >= 0; i--) {
                    ComposerAction action = m_activeActions.get(i);
                    ActionMouseCursor c = action.getMouseCursor(isOutsideEvent);
                    if (cursor == null && c != null) {
                        cursor = c;
                    }
                    if ( action.consumeEvent(event, isOutsideEvent)) {
                        consumed = true;
                        break;
                    }
                    if (action.isCompleted()) {
                        m_activeActions.remove(i);
                    }
                }
            }

            ComposerAction action = null;

            if ( !consumed) {
                //now check if the new action should be started
                for (int i = m_actionFactories.size() - 1; i >= 0; i--) {
                    if ( (action=m_actionFactories.get(i).startAction(event, isOutsideEvent)) != null) {
                        synchronized( m_activeActions) {
                            m_activeActions.push(action);
                        }
                        break;
                    }
                }
            } 

            if ( event instanceof MouseEvent && cursor == null) {
                MouseEvent me = (MouseEvent) event;
                for (int i = m_actionFactories.size() - 1; i >= 0; i--) {
                    ActionMouseCursor c;
                    if ( (c=m_actionFactories.get(i).getMouseCursor(me, isOutsideEvent)) != null) {
                        if (cursor == null || cursor.getPriority() < c.getPriority()) {
                            cursor = c;
                        }
                    }
                }  
            }
            JComponent c = controller.getScenePanel();
            if (c != null) {
                c.setCursor(cursor != null ? cursor.getCursor() : null);
            }
         }
    }

    public Action get(Class clazz) {
        if ( GenerateUIStubAction.class.equals( clazz)) {
            return m_generateStubAction;
        }
        
        for ( ComposerActionFactory factory : m_actionFactories) {
            Action a =ActionLookupUtils.get( factory.getMenuActions(), clazz);
            if ( a != null) {
                return a;
            }
        }
        return null;
    }
    
    private final GenerateUIStubAction m_generateStubAction = new GenerateUIStubAction();
    
    public final class GenerateUIStubAction extends AbstractFXDAction {
        GenerateUIStubAction() {
            super("generate_stub", true); // NOI18N
        }
        public void actionPerformed(ActionEvent e) {
            UIStubGenerator generator = new UIStubGenerator(m_dObj);
            generator.generate();
        }        
    }
}
