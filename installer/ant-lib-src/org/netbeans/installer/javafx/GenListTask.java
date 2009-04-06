package org.netbeans.installer.javafx;

import java.io.File;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.taskdefs.CallTarget;
import org.apache.tools.ant.taskdefs.Property;
import org.apache.tools.ant.types.FileSet;

/**
 *
 * @author Adam
 */
public class GenListTask extends Task {
    File basedir;

    public void setDir(File dir) {
        this.basedir = dir;
    }

    @Override
    public void execute() throws BuildException {
       FileSet fileset = new FileSet();
       fileset.setDir(basedir);
       DirectoryScanner ds = fileset.getDirectoryScanner(getProject());
       for (String fName : ds.getIncludedDirectories()) {
           File dir = new File(basedir, fName);
           CallTarget call = new CallTarget();
           call.setProject(getProject());
           call.setTarget("-list-dir");
           Property p = call.createParam();
           p.setName("name");
           p.setValue(fName.replace('\\', '/'));
           p = call.createParam();
           p.setName("lastMod");
           p.setValue(String.valueOf(dir.lastModified()));
           p = call.createParam();
           p.setName("empty");
           p.setValue(String.valueOf(dir.list().length == 0));
           call.perform();
       }
       for (String fName : ds.getIncludedFiles()) {
           File file = new File(basedir, fName);
           CallTarget call = new CallTarget();
           call.setProject(getProject());
           call.setTarget("-list-file");
           Property p = call.createParam();
           p.setName("file");
           p.setLocation(file);
           p = call.createParam();
           p.setName("name");
           p.setValue(fName.replace('\\', '/'));
           p = call.createParam();
           p.setName("lastMod");
           p.setValue(String.valueOf(file.lastModified()));
           call.perform();
       }
    }
}
