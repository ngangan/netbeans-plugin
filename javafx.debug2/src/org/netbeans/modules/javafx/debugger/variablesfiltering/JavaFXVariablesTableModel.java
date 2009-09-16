/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.variablesfiltering;

import org.netbeans.api.debugger.jpda.LocalVariable;
import org.netbeans.modules.javafx.debugger.watchesfiltering.*;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.TableModel;
import org.netbeans.spi.viewmodel.UnknownTypeException;
import org.netbeans.spi.debugger.ui.Constants;

/**
 *
 * @author Michal Skvor
 */
public class JavaFXVariablesTableModel implements TableModel {

    public JavaFXVariablesTableModel() {}

    public Object getValueAt( Object row, String columnID ) throws UnknownTypeException {
        if( !( row instanceof LocalVariable )) throw new UnknownTypeException( row );
        LocalVariable variable = (LocalVariable)row;
        if( columnID.equals( Constants.LOCALS_TO_STRING_COLUMN_ID )) {
            return variable.getValue();
        } else if( columnID.equals( Constants.LOCALS_TYPE_COLUMN_ID )) {
            String type = variable.getType();
            if( "int".equals( type )) {
                return "Integer";
            } else if( "float".equals( type )) {
                return "Number";
            } else {
                return variable.getType().replace( '$' , '.' );
            }
        } else if( columnID.equals( Constants.LOCALS_VALUE_COLUMN_ID )) {
            //String e = variable.getExceptionDescription ();
            //if( e != null ) return "> " + e + " <";
            return variable.getValue();
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
