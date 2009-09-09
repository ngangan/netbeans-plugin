/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.watchesfiltering;

import java.awt.Dialog;
import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.KeyStroke;
import org.netbeans.api.debugger.DebuggerManager;
import org.netbeans.api.debugger.Watch;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.Models;
import org.netbeans.spi.viewmodel.NodeActionsProvider;
import org.netbeans.spi.viewmodel.UnknownTypeException;
import org.openide.DialogDisplayer;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

/**
 *
 * @author Michal Skvor
 */
public class JavaFXWatchesActionsProvider implements NodeActionsProvider {

    private static final Action NEW_WATCH_ACTION = new AbstractAction(
        NbBundle.getBundle( JavaFXWatchesActionsProvider.class).getString( "CTL_WatchAction_AddNew" )) {
            public void actionPerformed( ActionEvent e ) {
                newWatch();
            }
    };
    
    private static final Action DELETE_ALL_ACTION = new AbstractAction( 
        NbBundle.getBundle( JavaFXWatchesActionsProvider.class ).getString( "CTL_WatchAction_DeleteAll" )) {
            public void actionPerformed( ActionEvent e ) {
                DebuggerManager.getDebuggerManager().removeAllWatches();
            }
    };

    private static final Action DELETE_ACTION = Models.createAction(
        NbBundle.getBundle( JavaFXWatchesActionsProvider.class ).getString( "CTL_WatchAction_Delete" ),
        new Models.ActionPerformer() {
            public boolean isEnabled( Object node ) {
                return true;
            }

            public void perform( Object[] nodes ) {
                int i, k = nodes.length;
                for (i = 0; i < k; i++) {
                    ((JavaFXWatch) nodes [i] ).getWatch().remove();
                }
            }
        },
        Models.MULTISELECTION_TYPE_ANY
    );

    static {
        DELETE_ACTION.putValue (
            Action.ACCELERATOR_KEY,
            KeyStroke.getKeyStroke( "DELETE" )
        );
    };
    
    private static final Action CUSTOMIZE_ACTION = Models.createAction (
        NbBundle.getBundle( JavaFXWatchesActionsProvider.class ).getString( "CTL_WatchAction_Customize" ),
        new Models.ActionPerformer() {
            public boolean isEnabled( Object node ) {
                return true;
            }
            public void perform( Object[] nodes ) {
                customize(((JavaFXWatch) nodes[0] ).getWatch());
            }
        },
        Models.MULTISELECTION_TYPE_EXACTLY_ONE
    );

    public Action[] getActions (Object node) throws UnknownTypeException {
        if (node instanceof JavaFXWatch )
            return new Action [] {
                NEW_WATCH_ACTION,
                null,
                DELETE_ACTION,
                DELETE_ALL_ACTION,
                null,
                CUSTOMIZE_ACTION
            };
        throw new UnknownTypeException( node );
    }

    public void performDefaultAction( Object node ) throws UnknownTypeException {
        if( node instanceof JavaFXWatch ) {
            customize(((JavaFXWatch) node).getWatch());
            return;
        }
        throw new UnknownTypeException( node );
    }

    public void addModelListener( ModelListener l ) {
    }

    public void removeModelListener( ModelListener l ) {
    }

    private static void customize( Watch w ) {
        WatchPanel wp = new WatchPanel( w.getExpression());
        JComponent panel = wp.getPanel();

        org.openide.DialogDescriptor dd = new org.openide.DialogDescriptor(
            panel,
            NbBundle.getMessage( JavaFXWatchesActionsProvider.class, "CTL_Edit_Watch_Dialog_Title", // NOI18N
                                           w.getExpression())
        );
        dd.setHelpCtx( new HelpCtx( "debug.add.watch" ));
        Dialog dialog = DialogDisplayer.getDefault().createDialog( dd );
        dialog.setVisible( true );
        dialog.dispose();

        if( dd.getValue() != org.openide.DialogDescriptor.OK_OPTION ) return;
        w.setExpression( wp.getExpression());
    }
    
    private static void newWatch() {
        WatchPanel wp = new WatchPanel( "" );
        JComponent panel = wp.getPanel();

        org.openide.DialogDescriptor dd = new org.openide.DialogDescriptor (
            panel,
            NbBundle.getMessage( JavaFXWatchesActionsProvider.class, "CTL_New_Watch_Dialog_Title" ) // NOI18N
        );
        dd.setHelpCtx( new HelpCtx( "debug.new.watch" ));
        Dialog dialog = DialogDisplayer.getDefault().createDialog( dd );
        dialog.setVisible( true );
        dialog.dispose();

        if( dd.getValue() != org.openide.DialogDescriptor.OK_OPTION ) return;
        DebuggerManager.getDebuggerManager().createWatch( wp.getExpression());
    }
}
