package org.netbeans.modules.javafx.debugger.models;

import com.sun.javafx.jdi.FXSequenceReference;
import com.sun.jdi.ObjectReference;
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
import org.netbeans.modules.debugger.jpda.models.AbstractObjectVariable;
import org.netbeans.modules.debugger.jpda.models.JPDAClassTypeImpl;

public final class SequenceObject extends AbstractObjectVariable implements Field {

    private final ObjectVariable parent;
    private int index;
    private int maxIndexLog;
    private String declaredType;

    public SequenceObject(JPDADebuggerImpl debugger, ObjectReference value, String declaredType, ObjectVariable array, int index, int maxIndex, String parentID) {
        super(debugger, value, parentID + '.' + index + "^");
        this.index = index;
        //            this.maxIndexLog = ArrayFieldVariable.log10( maxIndex );
        this.declaredType = declaredType;
        this.parent = array;
    }

    public String getName() {
        return "[" + index + "]";
        //            return ArrayFieldVariable.getName( maxIndexLog, index );
    }

    public String getClassName() {
        return getType();
    }

    public JPDAClassType getDeclaringClass() {
        try {
            FXSequenceReference a = (FXSequenceReference) ((JDIVariable) parent).getJDIValue();
            return new JPDAClassTypeImpl(getDebugger(), (ReferenceType) ValueWrapper.type(a));
        } catch (InternalExceptionWrapper ex) {
            throw ex.getCause();
        } catch (VMDisconnectedExceptionWrapper ex) {
            throw ex.getCause();
        } catch (ObjectCollectedExceptionWrapper ex) {
            throw ex.getCause();
        }
    }

    public ObjectVariable getParentVariable() {
        return parent;
    }

    public boolean isStatic() {
        return false;
    }

    public String getDeclaredType() {
        return declaredType.replace('$', '.');
    }

    protected void setValue(Value value) throws InvalidExpressionException {
        //            try {
        //                ArrayReferenceWrapper.setValue(array, index, value);
        //            } catch (InvalidTypeException ex) {
        //                throw new InvalidExpressionException (ex);
        //            } catch (ClassNotLoadedException ex) {
        //                throw new InvalidExpressionException (ex);
        //            } catch (InternalExceptionWrapper ex) {
        //                throw new InvalidExpressionException (ex.getCause());
        //            } catch (VMDisconnectedExceptionWrapper ex) {
        //                // Ignore
        //            } catch (ObjectCollectedExceptionWrapper ex) {
        //                throw new InvalidExpressionException (ex.getCause());
        //            }
    }

    public SequenceObject clone() {
        SequenceObject clon = new SequenceObject(getDebugger(), (ObjectReference) getJDIValue(), getDeclaredType(), parent, index, 0, getID());
        clon.maxIndexLog = this.maxIndexLog;
        return clon;
    }

    @Override
    public String toString() {
        return "SequenceObject " + getName();
    }
}
