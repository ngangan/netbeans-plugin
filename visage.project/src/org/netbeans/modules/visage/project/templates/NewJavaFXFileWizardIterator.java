/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 */

package org.netbeans.modules.visage.project.templates;

import java.awt.Component;
import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.NoSuchElementException;
import java.util.Set;
import javax.swing.JComponent;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.spi.project.ui.templates.support.Templates;
import org.openide.WizardDescriptor;
import org.openide.WizardDescriptor.Panel;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;

/**
 *
 * @author answer
 */
public class NewVisageFileWizardIterator implements WizardDescriptor.InstantiatingIterator{

    private static final long serialVersionUID = 1L;
    
    public static final int TYPE_FILE = 0;
    public static final int TYPE_PACKAGE = 1;
    public static final int TYPE_PKG_INFO = 2;
    
    private int type = TYPE_FILE;
    
    public NewVisageFileWizardIterator() {
    }

    private NewVisageFileWizardIterator( int type ) {
        this.type = type;
    }    
    
    public static NewVisageFileWizardIterator packageWizard() {
        return new NewVisageFileWizardIterator( TYPE_PACKAGE );
    }
    
    public static NewVisageFileWizardIterator packageInfoWizard () {
        return new NewVisageFileWizardIterator( TYPE_PKG_INFO );
    }
            
    private WizardDescriptor.Panel[] createPanels (WizardDescriptor wizardDescriptor) {
        Project project = Templates.getProject( wizardDescriptor );
        Sources sources = ProjectUtils.getSources(project);
        SourceGroup[] groups = sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_JAVA);
        assert groups != null : "Cannot return null from Sources.getSourceGroups: " + sources; // NOI18N
        if (groups.length == 0) {
            groups = sources.getSourceGroups( Sources.TYPE_GENERIC ); 
            return new WizardDescriptor.Panel[] {            
                Templates.createSimpleTargetChooser( project, groups ),
            };
        }
        else {
            if ( this.type == TYPE_FILE ) {
                return new WizardDescriptor.Panel[] {
                    VisageTemplates.createPackageChooser( project, groups ),
                };
            }
            else {                                
                return new WizardDescriptor.Panel[] {
                    new VisageTargetChooserPanel( project, groups, null, this.type, this.type == TYPE_PKG_INFO),
                };
            }
        }
    }
    
    private String[] createSteps(String[] before, WizardDescriptor.Panel[] panels) {
        assert panels != null;
        // hack to use the steps set before this panel processed
        int diff = 0;
        if (before == null) {
            before = new String[0];
        } else if (before.length > 0) {
            diff = ("...".equals (before[before.length - 1])) ? 1 : 0; // NOI18N
        }
        String[] res = new String[ (before.length - diff) + panels.length];
        for (int i = 0; i < res.length; i++) {
            if (i < (before.length - diff)) {
                res[i] = before[i];
            } else {
                res[i] = panels[i - before.length + diff].getComponent ().getName ();
            }
        }
        return res;
    }
            
    private transient int index;
    private transient WizardDescriptor.Panel[] panels;
    private transient WizardDescriptor wiz;
    
    public void initialize(WizardDescriptor wiz) {
        this.wiz = wiz;
        index = 0;
        panels = createPanels( wiz );
        // Make sure list of steps is accurate.
        String[] beforeSteps = null;
        Object prop = wiz.getProperty ("WizardPanel_contentData"); // NOI18N
        if (prop != null && prop instanceof String[]) {
            beforeSteps = (String[])prop;
        }
        String[] steps = createSteps (beforeSteps, panels);
        for (int i = 0; i < panels.length; i++) {
            Component c = panels[i].getComponent();
            if (steps[i] == null) {
                // Default step name to component name of panel.
                // Mainly useful for getting the name of the target
                // chooser to appear in the list of steps.
                steps[i] = c.getName();
            }
            if (c instanceof JComponent) { // assume Swing components
                JComponent jc = (JComponent)c;
                // Step #.
                jc.putClientProperty("WizardPanel_contentSelectedIndex", new Integer(i)); // NOI18N
                // Step name (actually the whole list for reference).
                jc.putClientProperty("WizardPanel_contentData", steps); // NOI18N
            }
        }
    }

    public Set instantiate() throws IOException {
        FileObject dir = Templates.getTargetFolder( wiz );
        String targetName = Templates.getTargetName( wiz );
        
        DataFolder df = DataFolder.findFolder( dir );
        FileObject template = Templates.getTemplate( wiz );
        
        FileObject createdFile = null;
        if ( this.type == TYPE_PACKAGE ) {
            targetName = targetName.replace( '.', '/' ); // NOI18N
            createdFile = FileUtil.createFolder( dir, targetName );
        }
        else {
            DataObject dTemplate = DataObject.find( template );                
            DataObject dobj = dTemplate.createFromTemplate( df, targetName );
            createdFile = dobj.getPrimaryFile();
        }
        
        return Collections.singleton( createdFile );
    }

    public void uninitialize(WizardDescriptor wiz) {
        this.wiz = null;
        panels = null;
    }

    private transient Set<ChangeListener> listeners = new HashSet<ChangeListener>(1);
    
    public void addChangeListener(ChangeListener cl) {
        synchronized(listeners){
            listeners.add(cl);
        }
    }

    public void removeChangeListener(ChangeListener cl) {
        synchronized(listeners){
            listeners.remove(cl);
        }
    }
    
    public Panel current() {
        return panels[index];
    }

    public boolean hasNext() {
        return index < panels.length - 1;
    }

    public boolean hasPrevious() {
        return index > 0;
    }

    public String name() {
        return "";
    }

    public void nextPanel() {
        if (!hasNext()){
            throw new NoSuchElementException();
        }
        index++;
    }

    public void previousPanel() {
        if (!hasPrevious()){
            throw new NoSuchElementException();
        }
        index--;
    }

    protected final void fireChangeEvent() {
        ChangeListener[] ls;
        synchronized (listeners) {
            ls = listeners.toArray(new ChangeListener[listeners.size()]);
        }
        ChangeEvent ev = new ChangeEvent(this);
        for (ChangeListener l : ls) {
            l.stateChanged(ev);
        }
    }
    
}
