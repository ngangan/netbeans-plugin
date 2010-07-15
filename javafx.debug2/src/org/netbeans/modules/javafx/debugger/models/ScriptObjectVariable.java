/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.models;

import com.sun.javafx.jdi.FXClassType;
import com.sun.javafx.jdi.FXField;
import com.sun.javafx.jdi.FXObjectReference;
import com.sun.jdi.Field;
import com.sun.jdi.InternalException;
import com.sun.jdi.ObjectCollectedException;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.VMDisconnectedException;
import com.sun.jdi.Value;
import java.util.logging.Logger;
import org.netbeans.api.debugger.jpda.InvalidExpressionException;
import org.netbeans.api.debugger.jpda.JPDAClassType;
import org.netbeans.modules.debugger.jpda.JPDADebuggerImpl;
import org.netbeans.modules.debugger.jpda.expr.JDIVariable;
import org.netbeans.modules.debugger.jpda.jdi.FieldWrapper;
import org.netbeans.modules.debugger.jpda.jdi.InternalExceptionWrapper;
import org.netbeans.modules.debugger.jpda.jdi.ObjectCollectedExceptionWrapper;
import org.netbeans.modules.debugger.jpda.jdi.ReferenceTypeWrapper;
import org.netbeans.modules.debugger.jpda.jdi.TypeComponentWrapper;
import org.netbeans.modules.debugger.jpda.jdi.VMDisconnectedExceptionWrapper;
import org.netbeans.modules.debugger.jpda.models.AbstractObjectVariable;
import org.netbeans.modules.debugger.jpda.models.JPDAClassTypeImpl;

/**
 *
 * @author Michal Skvor
 */
public class ScriptObjectVariable extends AbstractObjectVariable implements org.netbeans.api.debugger.jpda.Field {

    private static final Logger logger = Logger.getLogger( "org.netbeans.modules.debugger.jpda.getValue" ); // NOI18N
    
    private FXClassType parentClass;
    private Field field;
    
    private boolean valueSet = true;
    private final Object valueLock = new Object();
    private boolean valueRetrieved = false;
    private FXObjectReference value;
    
    public ScriptObjectVariable( JPDADebuggerImpl debugger, Field field, FXClassType parentClass, String parentID, FXObjectReference value ) {
        super( debugger, null, parentID );
        
        this.field = field;
        this.parentClass = parentClass;
        this.value = value;
    }
    
    public String getName() {
        try {
            return TypeComponentWrapper.name( field );
        } catch (InternalExceptionWrapper ex) {
            return ex.getCause().getLocalizedMessage();
        } catch (VMDisconnectedExceptionWrapper ex) {
            return "";
        }
    }

    public String getClassName() {
        try {
            return ReferenceTypeWrapper.name( TypeComponentWrapper.declaringType( field )); //className;
        } catch (InternalExceptionWrapper ex) {
            return ex.getCause().getLocalizedMessage();
        } catch(ObjectCollectedExceptionWrapper ex) {
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
        } catch( InternalExceptionWrapper ex ) {
            return ex.getLocalizedMessage();
        } catch( VMDisconnectedExceptionWrapper ex ) {
            return "";
        }
    }

    public boolean isStatic() {
        return false;
    }
    
    @Override
    protected Value getInnerValue() {
        synchronized (valueLock) {
            if (!valueRetrieved) {
                try {
                    Value v = parentClass.getValue( field );
                    this.value = (FXObjectReference) v;
                    this.valueRetrieved = true;
                } catch( RuntimeException e ) {
                    //
                }
            }
            return value;
        }
    }

    @Override
    public Value getJDIValue() {
        try {
            if( field.isStatic()) {
                return parentClass.getValue( field );
            } else {
                return value.getValue( field );
            }
        } catch( VMDisconnectedException ex ) {
            // Do nothing
        } catch( InternalException ex ) {
            
        } catch( ObjectCollectedException ex ) {
            
        }
        return null;
    }
    
    @Override
    public void setValue(String string) throws InvalidExpressionException {
        // Do nothing
    }    
}
