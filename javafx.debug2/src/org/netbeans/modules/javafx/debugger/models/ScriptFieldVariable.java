/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.models;

import com.sun.javafx.jdi.FXClassType;
import com.sun.jdi.Field;
import com.sun.jdi.Value;
import org.netbeans.api.debugger.jpda.InvalidExpressionException;
import org.netbeans.api.debugger.jpda.JPDAClassType;
import org.netbeans.modules.debugger.jpda.JPDADebuggerImpl;
import org.netbeans.modules.debugger.jpda.jdi.FieldWrapper;
import org.netbeans.modules.debugger.jpda.jdi.InternalExceptionWrapper;
import org.netbeans.modules.debugger.jpda.jdi.ObjectCollectedExceptionWrapper;
import org.netbeans.modules.debugger.jpda.jdi.ReferenceTypeWrapper;
import org.netbeans.modules.debugger.jpda.jdi.TypeComponentWrapper;
import org.netbeans.modules.debugger.jpda.jdi.VMDisconnectedExceptionWrapper;
import org.netbeans.modules.debugger.jpda.models.AbstractVariable;
import org.netbeans.modules.debugger.jpda.models.JPDAClassTypeImpl;

/**
 *
 * @author Michal Skvor
 */
public class ScriptFieldVariable extends AbstractVariable implements org.netbeans.api.debugger.jpda.Field {

    private FXClassType parentClass;
    private Field field;
    
    public ScriptFieldVariable( JPDADebuggerImpl debugger, Field field, FXClassType parentClass, String parentID ) {
        super( debugger, null, parentID );
        
        this.field = field;
        this.parentClass = parentClass;
    }

    public String getName() {
        try {
            return TypeComponentWrapper.name( field );
        } catch( InternalExceptionWrapper ex) {
            return ex.getCause().getLocalizedMessage();
        } catch( VMDisconnectedExceptionWrapper ex ) {
            return "";
        }
    }

    public String getClassName() {
        try {
            return ReferenceTypeWrapper.name(TypeComponentWrapper.declaringType(field)); //className;
        } catch (InternalExceptionWrapper ex) {
            return ex.getCause().getLocalizedMessage();
        } catch (ObjectCollectedExceptionWrapper ex) {
            return "";
        } catch (VMDisconnectedExceptionWrapper ex) {
            return "";
        }
    }

    public JPDAClassType getDeclaringClass() {
        return new JPDAClassTypeImpl( getDebugger(), parentClass );
    }

    public String getDeclaredType() {
        try {
            return FieldWrapper.typeName( field );
        } catch (InternalExceptionWrapper ex) {
            return ex.getLocalizedMessage();
        } catch (VMDisconnectedExceptionWrapper ex) {
            return "";
        }
    }

    public boolean isStatic() {
        return false;
    }

    @Override
    public String getValue() {
        return parentClass.getValue( field ).toString();
    }

    @Override
    public Value getJDIValue() {
        return parentClass.getValue( field );
    }

    @Override
    protected void setValue( Value value ) throws InvalidExpressionException {
//        try {
//            parentClass.setValue( field, value );
//        } catch( InvalidTypeException ex ) {
//            Exceptions.printStackTrace( ex );
//        } catch( ClassNotLoadedException ex ) {
//            Exceptions.printStackTrace( ex );
//        }
    }
}
