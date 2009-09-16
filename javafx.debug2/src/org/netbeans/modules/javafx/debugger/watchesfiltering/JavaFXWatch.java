/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.watchesfiltering;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import org.netbeans.api.debugger.Watch;
import org.netbeans.api.debugger.jpda.InvalidExpressionException;
import org.netbeans.api.debugger.jpda.JPDADebugger;
import org.netbeans.api.debugger.jpda.Variable;
import org.openide.util.WeakListeners;

/**
 *
 * @author Michal Skvor
 */
public class JavaFXWatch implements PropertyChangeListener {

    private final Watch     watch;

    private boolean         evaluated = false;
    private JPDADebugger    debugger;
    private Variable        variable;
    private Exception       exception;

    public JavaFXWatch( Watch w, JPDADebugger debugger ) {
        watch = w;
        this.debugger = debugger;
        w.addPropertyChangeListener((PropertyChangeListener) WeakListeners.create( PropertyChangeListener.class, this, w ));
    }

    public String getExpression () {
        return watch.getExpression();
    }

    public String getType() {
        if( !evaluated ) {
            evaluate();
        }
        return variable == null ? "" : variable.getType();
    }

    public String getValue() {
        if( !evaluated ) {
            evaluate();
        }
        return variable == null ? "" : variable.getValue();
    }

    public String getExceptionDescription() {
        if( !evaluated ) {
            evaluate();
        }
        return exception == null ? null : exception.getMessage();
    }

    public String getToStringValue() throws InvalidExpressionException {
        return getValue().toString();
    }

    public Watch getWatch() {
        return watch;
    }

    public Variable getVariable() {
        if( !evaluated )
            evaluate();
        return variable;
    }

    private synchronized void evaluate() {
        String text = watch.getExpression();
        text = text.replace("\"", "\\\"");
//        text = "pageContext.getExpressionEvaluator().evaluate(\"" + text +
//                            "\", java.lang.String.class, (javax.servlet.jsp.PageContext)pageContext, null)";
        try {
            variable = debugger.evaluate( text );
            exception = null;
        } catch( Exception e ) {
            exception = e;
        } finally {
            evaluated = true;
        }
    }

    public void propertyChange( PropertyChangeEvent evt ) {
        setUnevaluated();
    }

    public void setUnevaluated() {
        evaluated = false;
    }
}
