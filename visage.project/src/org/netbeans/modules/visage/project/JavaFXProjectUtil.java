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

package org.netbeans.modules.visage.project;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Vector;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.platform.JavaPlatform;
import org.netbeans.api.java.platform.JavaPlatformManager;
import org.netbeans.api.java.platform.Specification;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.SourceUtils;
import org.netbeans.api.visage.platform.VisagePlatform;
import org.netbeans.api.project.Project;
import org.netbeans.modules.visage.project.ui.customizer.VisageProjectProperties;
import org.netbeans.modules.visage.project.ui.customizer.MainClassChooser;
import org.netbeans.spi.project.support.ant.GeneratedFilesHelper;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

/**
 * Miscellaneous utilities for the visage/project module.
 * @author  Jiri Rechtacek
 */
public class VisageProjectUtil {
    private VisageProjectUtil () {}
    
    /**
     * Returns the property value evaluated by VisageProject's PropertyEvaluator.
     *
     * @param p project
     * @param value of property
     * @return evaluated value of given property or null if the property not set or
     * if the project doesn't provide AntProjectHelper
     */    
    public static Object getEvaluatedProperty(Project p, String value) {
        if (value == null) {
            return null;
        }
        VisageProject visageprj = (VisageProject) p.getLookup().lookup(VisageProject.class);
        if (visageprj != null) {
            return visageprj.evaluator().evaluate(value);
        } else {
            return null;
        }
    }
    
    /** Check if the given file object represents a source with the main method.
     * 
     * @param fo source
     * @return true if the source contains the main method
     */
    public static boolean hasMainMethod(FileObject fo) {
        // support for unit testing
        if (MainClassChooser.unitTestingSupport_hasMainMethodResult != null) {
            return MainClassChooser.unitTestingSupport_hasMainMethodResult.booleanValue ();
        }
        if (fo == null) {
            // ??? maybe better should be thrown IAE
            return false;
        }
        return !SourceUtils.getMainClasses(fo).isEmpty();
    }
    
    public static Collection<ElementHandle<TypeElement>> getMainMethods (final FileObject fo) {
        // support for unit testing
        if (fo == null || MainClassChooser.unitTestingSupport_hasMainMethodResult != null) {
            return Collections.<ElementHandle<TypeElement>>emptySet();
        }
        return SourceUtils.getMainClasses(fo);
    }

        
    public static boolean isMainClass (final String className, ClassPath bootPath, ClassPath compilePath, ClassPath sourcePath) {
        ClasspathInfo cpInfo = ClasspathInfo.create(bootPath, compilePath, sourcePath);
        List<String> fxFiles = getFXFiles(sourcePath.getRoots());
        
        return fxFiles.contains(className) || SourceUtils.isMainClass(className, cpInfo);
    }
  
    public static List<String> getFXFiles(FileObject[] sourcesRoots) {
        Vector result = new Vector();
        for(FileObject fo : sourcesRoots) {
            findFXFiles(fo, fo, result);
        }
        return(result);
    }
    
    private static void findFXFiles(FileObject fo, FileObject root, List<String> storage) {
        if(fo.isFolder()) {
            for(FileObject foc : fo.getChildren()) {
                findFXFiles(foc, root, storage);
            }
        } else {
            if ("text/x-fx".equals(FileUtil.getMIMEType(fo)) && // NOI18N
                    "fx".equals(fo.getExt())) { // NOI18N
                String shortPath = fo.getPath().substring(root.getPath().length());
                shortPath = shortPath.replace(shortPath.charAt(0), '.'). // NOI18N
                        substring(1, shortPath.length() - "fx".length() - 1); // NOI18N
                storage.add(shortPath);
            }
        }
    }
  
    
    /**
     * Creates an URL of a classpath or sourcepath root
     * For the existing directory it returns the URL obtained from {@link File#toUri()}
     * For archive file it returns an URL of the root of the archive file
     * For non existing directory it fixes the ending '/'
     * @param root the file of a root
     * @param offset a path relative to the root file or null (eg. src/ for jar:file:///lib.jar!/src/)" 
     * @return an URL of the root
     * @throws MalformedURLException if the URL cannot be created
     */
    public static URL getRootURL (File root, String offset) throws MalformedURLException {
        URL url = root.toURI().toURL();
        if (FileUtil.isArchiveFile(url)) {
            url = FileUtil.getArchiveRoot(url);
        } else if (!root.exists()) {
            url = new URL(url.toExternalForm() + "/"); // NOI18N
        }
        if (offset != null) {
            assert offset.endsWith("/");    //NOI18N
            url = new URL(url.toExternalForm() + offset); // NOI18N
        }
        return url;
    }
    
    
    /**
     * Returns the active platform used by the project or null if the active
     * project platform is broken.
     * @param activePlatformId the name of platform used by Ant script or null
     * for default platform.
     * @return active {@link JavaPlatform} or null if the project's platform
     * is broken
     */
    public static VisagePlatform getActivePlatform (final String activePlatformId) {
        final JavaPlatformManager pm = JavaPlatformManager.getDefault();
        if (activePlatformId == null) {
            return VisagePlatform.getDefaultFXPlatform();
        }
        else {
            JavaPlatform[] installedPlatforms = pm.getPlatforms(null, new Specification ("Visage",null));   //NOI18N
            for (int i=0; i<installedPlatforms.length; i++) {
                String antName = (String) installedPlatforms[i].getProperties().get("platform.ant.name");        //NOI18N
                if (installedPlatforms[i] instanceof VisagePlatform && antName != null && antName.equals(activePlatformId)) {
                    return (VisagePlatform)installedPlatforms[i];
                }
            }
            return null;
        }
    }
    
    public static String getBuildXmlName (final VisageProject project) {
        assert project != null;
        String buildScriptPath = project.evaluator().getProperty(VisageProjectProperties.BUILD_SCRIPT);
        if (buildScriptPath == null) {
            buildScriptPath = GeneratedFilesHelper.BUILD_XML_PATH;
        }
        return buildScriptPath;
    }
    
    public static FileObject getBuildXml (final VisageProject project) {
        return project.getProjectDirectory().getFileObject (getBuildXmlName(project));
    }
}
