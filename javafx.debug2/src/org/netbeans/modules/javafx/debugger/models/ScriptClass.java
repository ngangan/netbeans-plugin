/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.models;

import com.sun.javafx.jdi.FXClassType;
import com.sun.javafx.jdi.FXObjectReference;
import com.sun.javafx.jdi.FXPrimitiveType;
import com.sun.jdi.ClassNotLoadedException;
import com.sun.jdi.Value;
import java.util.ArrayList;
import java.util.List;
import org.netbeans.api.debugger.jpda.Field;
import org.netbeans.api.debugger.jpda.InvalidExpressionException;
import org.netbeans.api.debugger.jpda.JPDAClassType;
import org.netbeans.api.debugger.jpda.ObjectVariable;
import org.netbeans.modules.debugger.jpda.JPDADebuggerImpl;
import org.netbeans.modules.debugger.jpda.models.AbstractObjectVariable;
import org.netbeans.modules.debugger.jpda.models.JPDAClassTypeImpl;

/**
 *
 * @author Michal Skvor <michal.skvor@sun.com>
 */
public class ScriptClass extends AbstractObjectVariable implements ObjectVariable, Field {

    FXClassType scriptClass;
    FXObjectReference objectReference;
    
    public ScriptClass( JPDADebuggerImpl debugger, Value value, FXClassType scriptClass, FXObjectReference objectReference ) {        
        super( debugger, value, "script" );
        this.scriptClass = scriptClass;
        this.objectReference = objectReference;
    }

    @Override
    public String getType() {
        return scriptClass.name();
    }
    
    @Override
    public int getFieldsCount() {
        return scriptClass.allFields().size();
    }
    @Override
    public Field[] getFields( int start, int end ) {
        List<Field> result = new ArrayList<Field>();
        
        List<com.sun.jdi.Field> fields = scriptClass.allFields();
        for( int i = 0; i < fields.size(); i++ ) {
            com.sun.jdi.Field f = fields.get(  i );
            try {
                if( f.type() instanceof FXPrimitiveType ) {
                    result.add( new ScriptFieldVariable( getDebugger(), f, scriptClass, "FX" + f.name(), objectReference )); // NOI18N
                } else {
                    result.add( new ScriptObjectVariable( getDebugger(), f, scriptClass, "FX" + f.name(), objectReference )); // NOI18N
                }
            } catch( ClassNotLoadedException ex ) {
                ex.printStackTrace();
            }
        }

        return result.subList( start, end ).toArray( new Field[0] );
    }

    @Override
    public Field[] getAllStaticFields(int i, int i1) {
        return new Field[0];
    }

    @Override
    public Field[] getInheritedFields(int i, int i1) {
        return new Field[0];
    }

    public String getName() {
        return "Script"; // NOI18N
    }

    public String getClassName() {
        return scriptClass.name();
    }

    public JPDAClassType getDeclaringClass() {
        return new JPDAClassTypeImpl( getDebugger(), scriptClass );
    }

    public String getDeclaredType() {
        return scriptClass.name();
    }

    public boolean isStatic() {
        return false;
    }

    @Override
    protected void setValue(Value value) throws InvalidExpressionException {
        // Do nothing at all
    }
}
