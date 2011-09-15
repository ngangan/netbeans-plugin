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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2009 Sun
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

package org.netbeans.modules.visage.project;

import java.io.File;
import java.util.ArrayList;
import junit.framework.Test;
import junit.framework.TestSuite;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.junit.NbTestCase;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.modules.SpecificationVersion;

/**
 * @author answer
 */
public class CreateVisageProjectTest extends NbTestCase {

    private static final String[] createdFiles = {
        "build.xml",
        "nbproject/build-impl.xml",
        "nbproject/project.xml",
        "nbproject/project.properties",
        "src",
    };

    private static final String[] createdFilesExtSources = {
        "build.xml",
        "nbproject/build-impl.xml",
        "nbproject/project.xml",
        "nbproject/project.properties",
        "nbproject/private/private.properties",
    };

    private static final String[] createdProperties = {
        "build.classes.dir",
        "build.classes.excludes",
        "build.dir",
        "build.generated.dir",
        "build.sysclasspath",
        "build.test.classes.dir",
        "build.test.results.dir",
        "debug.classpath",
        "debug.test.classpath",
        "dist.dir",
        "dist.jar",
        "dist.javadoc.dir",
        "jar.compress",
        "javac.classpath",
        "javac.compilerargs",
        "javac.deprecation",
        "javac.source",
        "javac.target",
        "javac.test.classpath",
        "javadoc.author",
        "javadoc.encoding",
        "javadoc.noindex",
        "javadoc.nonavbar",
        "javadoc.notree",
        "javadoc.private",
        "javadoc.splitindex",
        "javadoc.use",
        "javadoc.version",
        "javadoc.windowtitle",
        "javadoc.additionalparam",
        "main.class",
        "manifest.file",
        "meta.inf.dir",
        "platform.active",
        "source.encoding",
        "run.classpath",
        "run.jvmargs",
        "run.test.classpath",
        "src.dir",
        "test.src.dir",
        "main.fx.class",
        "build.classpath",
        "FXBuild.class"
    };

    private static final String[] createdPropertiesExtSources = {
        "build.classes.dir",
        "build.classes.excludes",
        "build.dir",
        "build.generated.dir",
        "build.sysclasspath",
        "build.test.classes.dir",
        "build.test.results.dir",
        "debug.classpath",
        "debug.test.classpath",
        "dist.dir",
        "dist.jar",
        "dist.javadoc.dir",
        "jar.compress",
        "javac.classpath",
        "javac.compilerargs",
        "javac.deprecation",
        "javac.source",
        "javac.target",
        "javac.test.classpath",
        "javadoc.author",
        "javadoc.encoding",
        "javadoc.noindex",
        "javadoc.nonavbar",
        "javadoc.notree",
        "javadoc.private",
        "javadoc.splitindex",
        "javadoc.use",
        "javadoc.version",
        "javadoc.windowtitle",
        "javadoc.additionalparam",
        "main.class",
        "manifest.file",
        "meta.inf.dir",
        "platform.active",
        "source.encoding",
        "run.classpath",
        "run.jvmargs",
        "run.test.classpath",
        "src.dir",
        "test.src.dir",
        "main.fx.class",
        "build.classpath",
        "FXBuild.class"
    };

    public CreateVisageProjectTest(String testName) {
        super(testName);
    }

    public static Test suite(){
        TestSuite suite = new TestSuite(CreateVisageProjectTest.class);
        return suite;
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        org.openide.util.test.MockLookup.setLayersAndInstances();
        clearWorkDir();
    }

    public void testSanity() throws Exception {
        File prjDirF = new File(getDataDir(), "fxproject");
        assertTrue(prjDirF + " is directory", prjDirF.isDirectory());
        Project project = ProjectManager.getDefault().findProject(FileUtil.toFileObject(prjDirF));
        assertNotNull("cannot find project in: " + prjDirF, project);
    }

    public void testCreateEmptyProject() throws Exception{
        String name = "TestVisageApp";
        VisageProjectGenerator.setDefaultSourceLevel(new SpecificationVersion ("1.4"));   //NOI18N
        AntProjectHelper aph = null; // FIXME (not compilable) VisageProjectGenerator.createProject(new File(getWorkDir(), name), name, null, "manifest.mf");
        VisageProjectGenerator.setDefaultSourceLevel(null);
        assertNotNull(aph);

        FileObject fo = aph.getProjectDirectory();
        for (int i=0; i<createdFiles.length; i++) {
            assertNotNull(createdFiles[i]+" file/folder cannot be found", fo.getFileObject(createdFiles[i]));
        }
        EditableProperties props = aph.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
        ArrayList l = new ArrayList(props.keySet());
        for (int i=0; i<createdProperties.length; i++) {
            assertNotNull(createdProperties[i]+" property cannot be found in project.properties", props.getProperty(createdProperties[i]));
            l.remove(createdProperties[i]);
        }
        assertEquals("Found unexpected property: "+l,createdProperties.length, props.keySet().size());
    }

    public void testCreateEmptyProjectWithMain() throws Exception{
        String name = "TestVisageApp";
        String mainClass = name.toLowerCase() + ".Main";
        VisageProjectGenerator.setDefaultSourceLevel(new SpecificationVersion ("1.4"));   //NOI18N
        AntProjectHelper aph = VisageProjectGenerator.createProject(new File(getWorkDir(), name), name, mainClass, "manifest.mf");
        VisageProjectGenerator.setDefaultSourceLevel(null);
        assertNotNull(aph);

        FileObject fo = aph.getProjectDirectory();
        for (int i=0; i<createdFiles.length; i++) {
            assertNotNull(createdFiles[i]+" file/folder cannot be found", fo.getFileObject(createdFiles[i]));
        }
        assertNotNull(fo.getFileObject("src/" + mainClass.replace('.', '/') + ".fx"));//check main class

        EditableProperties props = aph.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
        ArrayList l = new ArrayList(props.keySet());
        for (int i=0; i<createdProperties.length; i++) {
            assertNotNull(createdProperties[i]+" property cannot be found in project.properties", props.getProperty(createdProperties[i]));
            l.remove(createdProperties[i]);
        }
        assertEquals("Found unexpected property: "+l,createdProperties.length, props.keySet().size());
    }

    public void testCreateProjectFromSources() throws Exception{
        File root = getWorkDir();
        String name = "TestVisageApp";
        File proj = new File (root, name);
        proj.mkdir();
        File src1 = new File (root, "src1");
        src1.mkdir ();
        File test1 = new File (root, "test1");
        test1.mkdir();
        VisageProjectGenerator.setDefaultSourceLevel(new SpecificationVersion ("1.4"));   //NOI18N
        AntProjectHelper aph = null; // FIXME (not compilable): VisageProjectGenerator.createProject(proj, name, new File[]{src1}, new File[]{test1}, "manifest.mf");
        VisageProjectGenerator.setDefaultSourceLevel(null);
        assertNotNull(aph);

        FileObject fo = FileUtil.toFileObject(proj);
        for (int i=0; i<createdFilesExtSources.length; i++) {
            assertNotNull(createdFilesExtSources[i]+" file/folder cannot be found", fo.getFileObject(createdFilesExtSources[i]));
        }
        EditableProperties props = aph.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
        ArrayList l = new ArrayList(props.keySet());
        int extFileRefCount = 0;
        for (int i=0; i<createdPropertiesExtSources.length; i++) {
            String propName = createdPropertiesExtSources[i];
            String propValue = props.getProperty(propName);
            assertNotNull(propName+" property cannot be found in project.properties", propValue);
            l.remove(propName);
            if ("manifest.file".equals (propName)) {
                assertEquals("Invalid value of manifest.file property.", "manifest.mf", propValue);
            }
            else if ("src.dir".equals (propName)) {
                PropertyEvaluator eval = aph.getStandardPropertyEvaluator();
                assertTrue("Value of the external source dir should be file reference",propValue.startsWith("${file.reference."));
                if (l.remove (propValue.subSequence(2,propValue.length()-1))) {
                    extFileRefCount++;
                }
                File file = aph.resolveFile(eval.evaluate(propValue));
                assertEquals("Invalid value of src.dir property.", src1, file);
            }
            else if ("test.src.dir".equals(propName)) {
                PropertyEvaluator eval = aph.getStandardPropertyEvaluator();
                assertTrue("Value of the external test dir should be file reference",propValue.startsWith("${file.reference."));
                if (l.remove (propValue.subSequence(2,propValue.length()-1))) {
                    extFileRefCount++;
                }
                File file = aph.resolveFile(eval.evaluate(propValue));
                assertEquals("Invalid value of test.src.dir property.", test1, file);
            }
        }
        assertEquals("Found unexpected property: "+l,createdPropertiesExtSources.length, props.keySet().size() - extFileRefCount);
    }

}
