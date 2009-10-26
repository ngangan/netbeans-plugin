/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.watchesfiltering;

import org.netbeans.api.debugger.jpda.Field;
import org.netbeans.spi.debugger.DebuggerServiceRegistration;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.TableModel;
import org.netbeans.spi.viewmodel.TableModelFilter;
import org.netbeans.spi.viewmodel.UnknownTypeException;
import org.netbeans.spi.debugger.ui.Constants;

/**
 *
 * @author Michal Skvor
 */
@DebuggerServiceRegistration( path="netbeans-JPDASession/FX/WatchesView", types={ org.netbeans.spi.viewmodel.TableModelFilter.class } )
public class JavaFXWatchesTableModelFilter implements TableModelFilter {

    public JavaFXWatchesTableModelFilter() { }

    /**
     * Returns filterred value to be displayed in column <code>columnID</code>
     * and row <code>node</code>. Column ID is defined in by
     * {@link ColumnModel#getID}, and rows are defined by values returned from
     * {@TreeModel#getChildren}. You should not throw UnknownTypeException
     * directly from this method!
     *
     * @param   original the original table model
     * @param   node a object returned from {@TreeModel#getChildren} for this row
     * @param   columnID a id of column defined by {@link ColumnModel#getID}
     * @throws  ComputingException if the value is not known yet and will
     *          be computed later
     * @throws  UnknownTypeException this exception can be thrown from
     *          <code>original.getValueAt (...)</code> method call only!
     *
     * @return value of variable representing given position in tree table.
     */
    public Object getValueAt( TableModel original, Object node, String columnID ) throws UnknownTypeException {
        Object colValue = "";
        
        colValue = original.getValueAt( node, columnID );
        // Type
        if( Constants.WATCH_TYPE_COLUMN_ID.equals( columnID )) {
            if( node instanceof Field ) {
                String value = colValue.toString();
                if( "int".equals( colValue )) {
                    return "Integer";
                } else if( "float".equals( colValue )) {
                    return "Number";
                }

                return value.replace( '$', '.' );
            }
        }

        return colValue;
    }

    /**
     * Filters original isReadOnly value from given table model. You should
     * not throw UnknownTypeException
     * directly from this method!
     *
     * @param  original the original table model
     * @param  node a object returned from {@TreeModel#getChildren} for this row
     * @param  columnID a id of column defined by {@link ColumnModel#getID}
     * @throws  UnknownTypeException this exception can be thrown from
     *          <code>original.isReadOnly (...)</code> method call only!
     *
     * @return true if variable on given position is read only
     */
    public boolean isReadOnly( TableModel original, Object node, String columnID ) throws UnknownTypeException {
        boolean ro = original.isReadOnly(node, columnID);
        return ro;
    }

    /**
     * Changes a value displayed in column <code>columnID</code>
     * and row <code>node</code>. Column ID is defined in by
     * {@link ColumnModel#getID}, and rows are defined by values returned from
     * {@TreeModel#getChildren}. You should not throw UnknownTypeException
     * directly from this method!
     *
     * @param  original the original table model
     * @param  node a object returned from {@TreeModel#getChildren} for this row
     * @param  columnID a id of column defined by {@link ColumnModel#getID}
     * @param  value a new value of variable on given position
     * @throws  UnknownTypeException this exception can be thrown from
     *          <code>original.setValueAt (...)</code> method call only!
     */
    public void setValueAt( TableModel original, Object node, String columnID, Object value ) throws UnknownTypeException {
        original.setValueAt( node, columnID, value );
    }

    public void addModelListener(ModelListener l) {
    }

    public void removeModelListener(ModelListener l) {
    }

}
