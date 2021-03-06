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

package org.netbeans.modules.visage.project.classpath;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.api.project.ant.AntArtifact;
import org.netbeans.api.project.libraries.Library;
import org.netbeans.modules.java.api.common.ant.UpdateHelper;
import org.netbeans.modules.visage.project.VisageProject;
import org.netbeans.modules.visage.project.ui.customizer.VisageProjectProperties;
import org.netbeans.spi.java.project.classpath.ProjectClassPathModifierImplementation;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.netbeans.spi.project.support.ant.ReferenceHelper;
import org.openide.ErrorManager;
import org.openide.filesystems.FileUtil;
import org.openide.util.Mutex;
import org.openide.util.MutexException;

/**
 *@author Tomas Zezula
 *
 */
public class VisageProjectClassPathModifier extends ProjectClassPathModifierImplementation {
    
    static final int ADD = 1;
    static final int REMOVE = 2;
    
    private final VisageProject project;
    private final UpdateHelper helper;
    private final ReferenceHelper refHelper;
    private final PropertyEvaluator eval;    
    private final ClassPathSupport cs;    
    
    /** Creates a new instance of VisageProjectClassPathModifier */
    public VisageProjectClassPathModifier(final VisageProject project, final UpdateHelper helper, final PropertyEvaluator eval, final ReferenceHelper refHelper) {
        assert project != null;
        assert helper != null;
        assert eval != null;
        assert refHelper != null;
        this.project = project;
        this.helper = helper;
        this.eval = eval;
        this.refHelper = refHelper;        
        this.cs = new ClassPathSupport( eval, refHelper, helper.getAntProjectHelper(), 
                                        VisageProjectProperties.WELL_KNOWN_PATHS, 
                                        VisageProjectProperties.LIBRARY_PREFIX, 
                                        VisageProjectProperties.LIBRARY_SUFFIX, 
                                        VisageProjectProperties.ANT_ARTIFACT_PREFIX );
    }
    
    protected SourceGroup[] getExtensibleSourceGroups() {
        Sources s = (Sources) this.project.getLookup().lookup(Sources.class);
        assert s != null;
        return s.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_JAVA);
    }
    
    protected String[] getExtensibleClassPathTypes (SourceGroup sg) {
        return new String[] {
            ClassPath.COMPILE,
            ClassPath.EXECUTE
        };
    }

    protected boolean removeRoots(final URL[] classPathRoots, final SourceGroup sourceGroup, final String type) throws IOException, UnsupportedOperationException {
        return handleRoots (classPathRoots, getClassPathProperty(sourceGroup, type), REMOVE);
    }

    protected boolean addRoots (final URL[] classPathRoots, final SourceGroup sourceGroup, final String type) throws IOException, UnsupportedOperationException {        
        return handleRoots (classPathRoots, getClassPathProperty(sourceGroup, type), ADD);
    }
    
    boolean handleRoots (final URL[] classPathRoots, final String classPathProperty, final int operation) throws IOException, UnsupportedOperationException {
        assert classPathRoots != null : "The classPathRoots cannot be null";      //NOI18N        
        assert classPathProperty != null;
        try {
            return ProjectManager.mutex().writeAccess(
                    new Mutex.ExceptionAction<Boolean>() {
                        public Boolean run() throws Exception {
                            EditableProperties props = helper.getProperties (AntProjectHelper.PROJECT_PROPERTIES_PATH);
                            String raw = props.getProperty(classPathProperty);                            
                            List<ClassPathSupport.Item> resources = cs.itemsList(raw);
                            boolean changed = false;
                            for (int i=0; i< classPathRoots.length; i++) {
                                assert classPathRoots[i] != null;
                                assert classPathRoots[i].toExternalForm().endsWith("/");    //NOI18N
                                URL toAdd = FileUtil.getArchiveFile(classPathRoots[i]);
                                if (toAdd == null) {
                                    toAdd = classPathRoots[i];
                                }
                                File f = FileUtil.normalizeFile( new File (URI.create(toAdd.toExternalForm())));
                                if (f == null ) {
                                    throw new IllegalArgumentException ("The file must exist on disk");     //NOI18N
                                }
                                ClassPathSupport.Item item = ClassPathSupport.Item.create( f, null );
                                if (operation == ADD && !resources.contains(item)) {
                                    resources.add (item);
                                    changed = true;
                                }                            
                                else if (operation == REMOVE && resources.contains(item)) {
                                    resources.remove(item);
                                    changed = true;
                                }
                            }                                                                                                                
                            if (changed) {
                                String itemRefs[] = cs.encodeToStrings( resources.iterator() );
                                props = helper.getProperties (AntProjectHelper.PROJECT_PROPERTIES_PATH);  //PathParser may change the EditableProperties
                                props.setProperty(classPathProperty, itemRefs);
                                helper.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, props);
                                ProjectManager.getDefault().saveProject(project);
                                return true;
                            }
                            return false;
                        }
                    }
            );
        } catch (Exception e) {
            if (e instanceof IOException) {
                throw (IOException) e;
            }
            else {
                Exception t = new IOException ();
                throw (IOException) ErrorManager.getDefault().annotate(t,e);
            }
        }
    }

    protected boolean removeAntArtifacts(final AntArtifact[] artifacts, final URI[] artifactElements, final SourceGroup sourceGroup, final String type) throws IOException, UnsupportedOperationException {
        return handleAntArtifacts (artifacts, artifactElements, getClassPathProperty(sourceGroup, type), REMOVE);
    }

    protected boolean addAntArtifacts(final AntArtifact[] artifacts, final URI[] artifactElements, final SourceGroup sourceGroup, final String type) throws IOException, UnsupportedOperationException {
        return handleAntArtifacts (artifacts, artifactElements, getClassPathProperty(sourceGroup, type), ADD);
    }
    
    boolean handleAntArtifacts (final AntArtifact[] artifacts, final URI[] artifactElements, final String classPathProperty, final int operation) throws IOException, UnsupportedOperationException {
        assert artifacts != null : "Artifacts cannot be null";    //NOI18N
        assert artifactElements != null : "ArtifactElements cannot be null";  //NOI18N
        assert artifacts.length == artifactElements.length : "Each artifact has to have corresponding artifactElement"; //NOI18N
        assert classPathProperty != null;
        try {
            return ProjectManager.mutex().writeAccess(
                    new Mutex.ExceptionAction<Boolean>() {
                        public Boolean run() throws Exception {
                            EditableProperties props = helper.getProperties (AntProjectHelper.PROJECT_PROPERTIES_PATH);
                            String raw = props.getProperty (classPathProperty);
                            List<ClassPathSupport.Item> resources = cs.itemsList(raw);
                            boolean changed = false;
                            for (int i=0; i<artifacts.length; i++) {
                                assert artifacts[i] != null;
                                assert artifactElements[i] != null;
                                ClassPathSupport.Item item = ClassPathSupport.Item.create( artifacts[i], artifactElements[i], null );
                                if (operation == ADD && !resources.contains(item)) {
                                    resources.add (item);
                                    changed = true;
                                }
                                else if (operation == REMOVE && resources.contains(item)) {
                                    resources.remove(item);
                                    changed = true;
                                }
                            }                            
                            if (changed) {
                                String itemRefs[] = cs.encodeToStrings( resources.iterator() );                                
                                props = helper.getProperties (AntProjectHelper.PROJECT_PROPERTIES_PATH);    //Reread the properties, PathParser changes them
                                props.setProperty (classPathProperty, itemRefs);
                                helper.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, props);
                                ProjectManager.getDefault().saveProject(project);
                                return true;
                            }
                            return false;
                        }
                    }
            );
        } catch (Exception e) {
            if (e instanceof IOException) {
                throw (IOException) e;
            }
            else {
                Exception t = new IOException ();
                throw (IOException) ErrorManager.getDefault().annotate(t,e);
            }
        }
    }
    
    
    protected boolean removeLibraries(final Library[] libraries, final SourceGroup sourceGroup, final String type) throws IOException, UnsupportedOperationException {
        return handleLibraries (libraries, getClassPathProperty(sourceGroup, type), REMOVE);
    }

    protected boolean addLibraries(final Library[] libraries, final SourceGroup sourceGroup, final String type) throws IOException, UnsupportedOperationException {
        return handleLibraries (libraries, getClassPathProperty(sourceGroup, type), ADD);
    }
    
    boolean handleLibraries (final Library[] libraries, final String classPathProperty, final int operation) throws IOException, UnsupportedOperationException {
        assert libraries != null : "Libraries cannot be null";  //NOI18N
        assert classPathProperty != null;
        try {
            return ProjectManager.mutex().writeAccess(
                    new Mutex.ExceptionAction<Boolean>() {
                        public Boolean run() throws IOException {
                            EditableProperties props = helper.getProperties (AntProjectHelper.PROJECT_PROPERTIES_PATH);
                            String raw = props.getProperty(classPathProperty);
                            List<ClassPathSupport.Item> resources = cs.itemsList(raw);
                            List<ClassPathSupport.Item> changed = new ArrayList<ClassPathSupport.Item>(libraries.length);
                            for (int i=0; i< libraries.length; i++) {
                                assert libraries[i] != null;
                                ClassPathSupport.Item item = ClassPathSupport.Item.create( libraries[i], null );
                                if (operation == ADD && !resources.contains(item)) {
                                    resources.add (item);                                
                                    changed.add(item);
                                }
                                else if (operation == REMOVE && resources.contains(item)) {
                                    resources.remove(item);
                                    changed.add(item);
                                }
                            }
                            if (!changed.isEmpty()) {
                                String itemRefs[] = cs.encodeToStrings( resources.iterator() );                                
                                props = helper.getProperties (AntProjectHelper.PROJECT_PROPERTIES_PATH);    //PathParser may change the EditableProperties                                
                                props.setProperty(classPathProperty, itemRefs);                                
                                if (operation == ADD) {
                                    for (ClassPathSupport.Item item : changed) {
                                        String prop = cs.getLibraryReference(item);
                                        prop = prop.substring(2, prop.length()-1); // XXX make a PropertyUtils method for this!
                                        ClassPathSupport.relativizeLibraryClassPath(props, helper.getAntProjectHelper(), prop);
                                    }
                                }
                                helper.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, props);
                                ProjectManager.getDefault().saveProject(project);
                                return true;
                            }
                            return false;
                        }
                    }
            );
        } catch (MutexException e) {
            throw (IOException) e.getException();
        }
    }
    
    private String getClassPathProperty (final SourceGroup sg, final String type) throws UnsupportedOperationException {
        assert sg != null : "SourceGroup cannot be null";  //NOI18N
        assert type != null : "Type cannot be null";  //NOI18N
        final String classPathProperty = project.getClassPathProvider().getPropertyName (sg, type);
        if (classPathProperty == null) {
            throw new UnsupportedOperationException ("Modification of [" + sg.getRootFolder().getPath() +", " + type + "] is not supported"); // NOI18N
        }
        return classPathProperty;
    }
}
