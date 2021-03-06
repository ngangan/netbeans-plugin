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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
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

package org.netbeans.modules.visage.project.ui.wizards;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.URISyntaxException;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeMap;
import javax.swing.JComponent;
import javax.swing.event.ChangeListener;
import org.netbeans.api.visage.platform.VisagePlatform;
import org.netbeans.api.progress.ProgressHandle;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.modules.visage.project.VisageProject;
import org.netbeans.modules.visage.project.VisageProjectGenerator;
import org.netbeans.modules.visage.project.ui.FoldersListSettings;
import org.netbeans.modules.visage.project.ui.customizer.VisageProjectProperties;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.netbeans.spi.project.ui.support.ProjectChooser;
import org.openide.ErrorManager;
import org.openide.WizardDescriptor;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;

/**
 * Wizard to create a new Visage project.
 */
public class NewVisageProjectWizardIterator implements WizardDescriptor.ProgressInstantiatingIterator {

    enum WizardType {APP, EXT}
    
    static final String PROP_NAME_INDEX = "nameIndex";      //NOI18N

    private static final String MANIFEST_FILE = "manifest.mf"; // NOI18N

    private static final long serialVersionUID = 1L;
    
    private WizardType type;
    
    /** Create a new wizard iterator. */
    public NewVisageProjectWizardIterator() {
        this(WizardType.APP);
    }
    
    public NewVisageProjectWizardIterator(WizardType type) {
        this.type = type;
    }
    
    private WizardDescriptor.Panel[] createPanels() {
        return new WizardDescriptor.Panel[] {
            new PanelConfigureProject(type)
        };
    }
    
    private String[] createSteps() {
        return new String[] {
            NbBundle.getMessage(NewVisageProjectWizardIterator.class,"LAB_ConfigureProject"), // NOI18N
        };
    }
    
    
    public Set<?> instantiate() throws IOException {
        assert false : "Cannot call this method if implements WizardDescriptor.ProgressInstantiatingIterator."; // NOI18N
        return null;
    }
        
    public Set<FileObject> instantiate (ProgressHandle handle) throws IOException {
        handle.start (4);
        Set<FileObject> resultSet = new HashSet<FileObject>();
        File dirF = (File)wiz.getProperty("projdir");        //NOI18N
        if (dirF != null) {
            dirF = FileUtil.normalizeFile(dirF);
        }
        String name = (String)wiz.getProperty("name");        //NOI18N
        String mainClass = (String)wiz.getProperty("mainClass");        //NOI18N
        handle.progress (NbBundle.getMessage (NewVisageProjectWizardIterator.class, "LBL_NewVisageProjectWizardIterator_WizardProgress_CreatingProject"), 1); // NOI18N
        
        type = (WizardType)wiz.getProperty("projectType"); // NOI18N
        
        switch (type) {
        case EXT:
            File[] sourceFolders = (File[])wiz.getProperty("sourceRoot");        //NOI18N
            AntProjectHelper h = VisageProjectGenerator.createProject(dirF, name, sourceFolders, MANIFEST_FILE );
            EditableProperties ep = h.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
            String includes = (String) wiz.getProperty(VisageProjectProperties.INCLUDES);
            if (includes == null) {
                includes = "**"; // NOI18N
            }
            ep.setProperty(VisageProjectProperties.INCLUDES, includes);
            String excludes = (String) wiz.getProperty(VisageProjectProperties.EXCLUDES);
            if (excludes == null) {
                excludes = ""; // NOI18N
            }
            ep.setProperty(VisageProjectProperties.EXCLUDES, excludes);
            h.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, ep);
            handle.progress (2);
            for (File f : sourceFolders) {
                FileObject srcFo = FileUtil.toFileObject(f);
                if (srcFo != null) {
                    resultSet.add (srcFo);
                }
            }

            createRuntimeProfiles( h );        
            break;
        default:
            h = VisageProjectGenerator.createProject(dirF, name, mainClass, type == WizardType.APP ? MANIFEST_FILE : null);
            handle.progress (2);
            if (mainClass != null && mainClass.length () > 0) {
                try {
                    //String sourceRoot = "src"; //(String)visageProperties.get (VisageProjectProperties.SRC_DIR);
                    FileObject sourcesRoot = h.getProjectDirectory ().getFileObject ("src");        //NOI18N
                    FileObject mainClassFo = getMainClassFO (sourcesRoot, mainClass);
                    assert mainClassFo != null : "sourcesRoot: " + sourcesRoot + ", mainClass: " + mainClass;        //NOI18N
                    // Returning FileObject of main class, will be called its preferred action
                    resultSet.add (mainClassFo);

                    createRuntimeProfiles( h );
                } catch (Exception x) {
                    ErrorManager.getDefault().notify(x);
                }
            }
        }
        FileObject dir = FileUtil.toFileObject(dirF);
        switch (type) {
            case APP:
            case EXT:
                createManifest(dir);
        }
        handle.progress (3);

        // Returning FileObject of project diretory. 
        // Project will be open and set as main
        int index = (Integer) wiz.getProperty(PROP_NAME_INDEX);
        switch (type) {
            case APP:
                FoldersListSettings.getDefault().setNewApplicationCount(index);
                break;
            case EXT:
                FoldersListSettings.getDefault().setNewProjectCount(index);
                break;
        }        
        resultSet.add (dir);
        handle.progress (NbBundle.getMessage (NewVisageProjectWizardIterator.class, "LBL_NewVisageProjectWizardIterator_WizardProgress_PreparingToOpen"), 4); // NOI18N
        dirF = (dirF != null) ? dirF.getParentFile() : null;
        if (dirF != null && dirF.exists()) {
            ProjectChooser.setProjectsFolder (dirF);    
        }
                        
        return resultSet;
    }

    private void createRuntimeProfiles( AntProjectHelper helper ) throws IOException {
        // Create runtime profiles
        VisageProject project = (VisageProject)ProjectManager.getDefault().
                findProject( helper.getProjectDirectory());
        VisageProjectProperties projectProperties = new VisageProjectProperties(
                project, project.getUpdateHelper(), project.evaluator(), project.getReferenceHelper(), null );

        Map<String, Map<String, String>> profiles = projectProperties.readRunConfigs();

        Map<String, String> profile;

        // Browser
        profile = new TreeMap<String, String>();
        profile.put( "$label", NbBundle.getMessage(NewVisageProjectWizardIterator.class, "LBL_Profile_Browser" )); //NOI18N
        profile.put( "visage.profile", "desktop" ); //NOI18N
        profile.put( "execution.target", "applet" ); //NOI18N
        profiles.put( "browser", profile );
        // Web start
        profile = new TreeMap<String, String>();
        profile.put( "$label", NbBundle.getMessage(NewVisageProjectWizardIterator.class, "LBL_Profile_WebStart" )); //NOI18N
        profile.put( "visage.profile", "desktop" ); //NOI18N
        profile.put( "execution.target", "jnlp" ); //NOI18N
        profiles.put( "webstart", profile );

        // Mobile
        try {
            if( new File( new File( VisagePlatform.getDefaultFXPlatform().getVisageFolder().toURI()), "emulator/mobile/bin/preverify" + (Utilities.isWindows() ? ".exe" : "")).isFile()) { // NOI18N
                profile = new TreeMap<String, String>();
                profile.put( "$label", NbBundle.getMessage(NewVisageProjectWizardIterator.class, "LBL_Profile_Mobile" )); //NOI18N
                profile.put( "visage.profile", "mobile" ); //NOI18N
                profile.put( "execution.target", "midp" ); //NOI18N
                profiles.put( "mobile", profile );
            }
        } catch( URISyntaxException e ) { /* Should not happen */ }

        // TV
        try {
            if( new File( new File( VisagePlatform.getDefaultFXPlatform().getVisageFolder().toURI()), "emulator/mobile/bin/preverify" + (Utilities.isWindows() ? ".exe" : "")).isFile()) { // NOI18N
                profile = new TreeMap<String, String>();
                profile.put( "$label", NbBundle.getMessage(NewVisageProjectWizardIterator.class, "LBL_Profile_TV" )); //NOI18N
                profile.put( "visage.profile", "tv" ); //NOI18N
                profile.put( "execution.target", "cvm" ); //NOI18N
                profiles.put( "tv", profile );
             }
         } catch( URISyntaxException e ) { /* Should not happen */ }

        EditableProperties prj = project.getUpdateHelper().getProperties( AntProjectHelper.PROJECT_PROPERTIES_PATH );
        EditableProperties prv = project.getUpdateHelper().getProperties( AntProjectHelper.PRIVATE_PROPERTIES_PATH );
        projectProperties.storeRunConfigs( profiles, prj, prv);
    }
        
    private transient int index;
    private transient WizardDescriptor.Panel[] panels;
    private transient WizardDescriptor wiz;
    
    public void initialize(WizardDescriptor wiz) {
        this.wiz = wiz;
        index = 0;
        panels = createPanels();
        // Make sure list of steps is accurate.
        String[] steps = createSteps();
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
                jc.putClientProperty("WizardPanel_contentSelectedIndex", i); // NOI18N
                // Step name (actually the whole list for reference).
                jc.putClientProperty("WizardPanel_contentData", steps); // NOI18N
            }
        }
        //set the default values of the sourceRoot and the testRoot properties
        this.wiz.putProperty("sourceRoot", new File[0]);    //NOI18N
        this.wiz.putProperty("testRoot", new File[0]);      //NOI18N
        
        this.wiz.putProperty("projectType", type);      //NOI18N
    }

    public void uninitialize(WizardDescriptor wiz) {
        if (this.wiz != null) {
            this.wiz.putProperty("projdir",null);           //NOI18N
            this.wiz.putProperty("name",null);          //NOI18N
            this.wiz.putProperty("mainClass",null);         //NOI18N
            switch (type) {
            case EXT:
                this.wiz.putProperty("sourceRoot",null);    //NOI18N
                this.wiz.putProperty("testRoot",null);      //NOI18N
            }
            this.wiz = null;
            panels = null;
        }
    }
    
    public String name() {
        return NbBundle.getMessage(NewVisageProjectWizardIterator.class, "LAB_IteratorName", index + 1, panels.length); // NOI18N
    }
    
    public boolean hasNext() {
        return index < panels.length - 1;
    }
    public boolean hasPrevious() {
        return index > 0;
    }
    public void nextPanel() {
        if (!hasNext()) throw new NoSuchElementException();
        index++;
    }
    public void previousPanel() {
        if (!hasPrevious()) throw new NoSuchElementException();
        index--;
    }
    public WizardDescriptor.Panel current () {
        return panels[index];
    }
    
    // If nothing unusual changes in the middle of the wizard, simply:
    public final void addChangeListener(ChangeListener l) {}
    public final void removeChangeListener(ChangeListener l) {}
    
    // helper methods, finds mainclass's FileObject
    private FileObject getMainClassFO (FileObject sourcesRoot, String mainClass) {
        // replace '.' with '/'
        mainClass = mainClass.replace ('.', '/'); // NOI18N
        return sourcesRoot.getFileObject (mainClass+ ".fx"); // NOI18N
    }

    static String getPackageName (String displayName) {
        StringBuffer builder = new StringBuffer ();
        boolean firstLetter = true;
        for (int i=0; i< displayName.length(); i++) {
            char c = displayName.charAt(i);            
            if ((!firstLetter && Character.isJavaIdentifierPart (c)) || (firstLetter && Character.isJavaIdentifierStart(c))) {
                firstLetter = false;
                if (Character.isUpperCase(c)) {
                    c = Character.toLowerCase(c);
                }                    
                builder.append(c);
            }            
        }
        return builder.length() == 0 ? NbBundle.getMessage(NewVisageProjectWizardIterator.class,"TXT_DefaultPackageName") : builder.toString(); // NOI18N
    }
    
    /**
     * Create a new application manifest file with minimal initial contents.
     * @param dir the directory to create it in
     * @throws IOException in case of problems
     */
    private static void createManifest(final FileObject dir) throws IOException {
        FileObject manifest = dir.createData(MANIFEST_FILE);
        FileLock lock = manifest.lock();
        try {
            OutputStream os = manifest.getOutputStream(lock);
            try {
                PrintWriter pw = new PrintWriter(os);
                pw.println("Manifest-Version: 1.0"); // NOI18N
                pw.println("X-COMMENT: Main-Class will be added automatically by build"); // NOI18N
                pw.println(); // safest to end in \n\n due to JRE parsing bug
                pw.flush();
            } finally {
                os.close();
            }
        } finally {
            lock.releaseLock();
        }
    }

}
