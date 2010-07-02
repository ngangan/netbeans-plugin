package org.netbeans.modules.javafx.debugger.models;

import com.sun.javafx.jdi.FXPrimitiveValue;
import com.sun.javafx.jdi.FXSequenceReference;
import com.sun.javafx.jdi.FXValue;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.Value;
import org.netbeans.api.debugger.jpda.Field;
import org.netbeans.api.debugger.jpda.InvalidExpressionException;
import org.netbeans.api.debugger.jpda.JPDAClassType;
import org.netbeans.api.debugger.jpda.ObjectVariable;
import org.netbeans.modules.debugger.jpda.JPDADebuggerImpl;
import org.netbeans.modules.debugger.jpda.expr.JDIVariable;
import org.netbeans.modules.debugger.jpda.jdi.InternalExceptionWrapper;
import org.netbeans.modules.debugger.jpda.jdi.ObjectCollectedExceptionWrapper;
import org.netbeans.modules.debugger.jpda.jdi.VMDisconnectedExceptionWrapper;
import org.netbeans.modules.debugger.jpda.jdi.ValueWrapper;
import org.netbeans.modules.debugger.jpda.models.AbstractVariable;
import org.netbeans.modules.debugger.jpda.models.JPDAClassTypeImpl;

/**
 * Helper representation of Sequence
 */
public class SequenceField extends AbstractVariable implements Field {

    private FXSequenceReference sequence;
    private ObjectVariable parent;
    private String name;
    private FXValue value;
    private int index;

    public SequenceField(JPDADebuggerImpl debugger, FXPrimitiveValue value, ObjectVariable parent, int index, String parentID) {
        super(debugger, value, parentID + "." + index);
        this.index = index;
        this.parent = parent;
        this.value = value;
        this.sequence = (FXSequenceReference) ((JDIVariable) parent).getJDIValue();
    }

    public String getName() {
        return "[" + index + "]";
    }

    public String getClassName() {
        return getType();
    }

    public JPDAClassType getDeclaringClass() {
        try {
            return new JPDAClassTypeImpl(getDebugger(), (ReferenceType) ValueWrapper.type(sequence));
        } catch (InternalExceptionWrapper ex) {
            // re-throw, we should not return null and can not throw anything checked.
            throw ex.getCause();
        } catch (ObjectCollectedExceptionWrapper ex) {
            // re-throw, we should not return null and can not throw anything checked.
            throw ex.getCause();
        } catch (VMDisconnectedExceptionWrapper ex) {
            // re-throw, we should not return null and can not throw anything checked.
            throw ex.getCause();
        }
    }

    public String getDeclaredType() {
        return value.type().name();
    }

    public boolean isStatic() {
        return false;
    }

    @Override
    protected void setValue(Value value) throws InvalidExpressionException {
        //                ArrayReferenceWrapper.setValue( sequence, index, value );
        sequence.setValue(index, value);
    }

    @Override
    public String toString() {
        return "SequenceField " + getName();
    }
}
