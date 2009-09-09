/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.watchesfiltering;

import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.TableModel;
import org.netbeans.spi.viewmodel.UnknownTypeException;
import org.netbeans.spi.debugger.ui.Constants;

/**
 *
 * @author Michal Skvor
 */
public class JavaFXWatchesTableModel implements TableModel {

    public JavaFXWatchesTableModel() {}

    public Object getValueAt( Object row, String columnID ) throws UnknownTypeException {
        if( !( row instanceof JavaFXWatch )) throw new UnknownTypeException(row);
        JavaFXWatch watch = (JavaFXWatch) row;
        if( columnID.equals( Constants.WATCH_TO_STRING_COLUMN_ID )) {
            return watch.getValue();
        } else if( columnID.equals( Constants.WATCH_TYPE_COLUMN_ID )) {
            String type = watch.getType();
            if( "int".equals( type )) {
                return "Integer";
            } else if( "float".equals( type )) {
                return "Number";
            } else {
                return watch.getType();
            }
        } else if( columnID.equals( Constants.WATCH_VALUE_COLUMN_ID )) {
            String e = watch.getExceptionDescription ();
            if( e != null ) return "> " + e + " <";
            return watch.getValue();
        }
        throw new UnknownTypeException( row );
    }

    public boolean isReadOnly( Object row, String columnID ) throws UnknownTypeException {
        if( !( row instanceof JavaFXWatch )) throw new UnknownTypeException( row );
        return true;
    }

    public void setValueAt( Object row, String columnID, Object value ) throws UnknownTypeException {
        throw new UnknownTypeException( row );
    }

    public void addModelListener( ModelListener l ) {
    }

    public void removeModelListener( ModelListener l ) {
    }

}
