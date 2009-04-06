package org.netbeans.installer.javafx;

import java.util.Calendar;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

/**
 *
 * @author Adam
 */
public class CurrentMillisTask extends Task {
    String property;

    public void setProperty(String property) {
        this.property = property;
    }

    @Override
    public void execute() throws BuildException {
        getProject().setNewProperty(property, String.valueOf(Calendar.getInstance().getTimeInMillis()));
    }

}
