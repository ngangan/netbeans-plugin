/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.tablerendering;

import com.sun.javafx.jdi.FXValue;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import org.netbeans.api.debugger.jpda.Field;
import org.netbeans.modules.debugger.jpda.expr.JDIVariable;
import org.netbeans.spi.debugger.DebuggerServiceRegistration;
import org.netbeans.spi.debugger.ui.Constants;
import org.netbeans.spi.viewmodel.ModelListener;
import org.netbeans.spi.viewmodel.TableRendererModel;
import org.netbeans.spi.viewmodel.UnknownTypeException;

/**
 *
 * @author Michal Skvor <michal.skvor at sun.com>
 */
@DebuggerServiceRegistration( path="netbeans-JPDASession/FX/LocalsView",types={ org.netbeans.spi.viewmodel.TableRendererModel.class } )
public class JavaFXTableRendererModel implements TableRendererModel {

    public JavaFXTableRendererModel() {}

    public boolean canRenderCell( Object o, String columnName ) throws UnknownTypeException {
        if( Constants.LOCALS_TYPE_COLUMN_ID.equals( columnName )) {
            if( o instanceof Field ) {
                Field f = (Field)o;
                if( f instanceof JDIVariable ) {
                    JDIVariable v = (JDIVariable)f;
                    if( v.getJDIValue() instanceof FXValue ) {
                        return true;
                    }
                }
            }
        } else if( Constants.LOCALS_VALUE_COLUMN_ID.equals( columnName )) {
            if( o instanceof Field ) {
                Field f = (Field)o;
                if( f instanceof JDIVariable ) {
                    JDIVariable v = (JDIVariable)f;
                    if( v.getJDIValue() instanceof FXValue ) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    public TableCellRenderer getCellRenderer( Object o, String columnName ) throws UnknownTypeException {
        if( Constants.LOCALS_TYPE_COLUMN_ID.equals( columnName ))
            return new VariableTypeRenderer( o );
        else if( Constants.LOCALS_VALUE_COLUMN_ID.equals( columnName ))
            return new VariableCellRenderer( o, columnName );

        return null;
    }

    public boolean canEditCell( Object o, String string ) throws UnknownTypeException {
        return true;
    }

    public TableCellEditor getCellEditor( Object o, String string ) throws UnknownTypeException {
        return new VariableCellEditor( o );
    };

    public void addModelListener( ModelListener ml ) {
    }

    public void removeModelListener( ModelListener ml ) {
    }
}
