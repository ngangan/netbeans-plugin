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
package org.netbeans.modules.visage.profiler;

import java.awt.Dialog;
import java.awt.event.MouseEvent;
import org.netbeans.api.java.platform.JavaPlatform;
import org.netbeans.api.java.platform.JavaPlatformManager;
import org.netbeans.api.java.platform.Specification;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.lib.profiler.ProfilerLogger;
import org.netbeans.lib.profiler.common.SessionSettings;
import org.netbeans.modules.profiler.AbstractProjectTypeProfiler;
import org.netbeans.modules.profiler.ui.ProfilerDialogs;
import org.netbeans.modules.profiler.utils.AppletSupport;
import org.netbeans.modules.profiler.utils.ProjectUtilities;
import org.netbeans.modules.visage.profiler.utilities.VisageProjectUtilities;
import org.netbeans.modules.profiler.projectsupport.utilities.SourceUtils;
import org.netbeans.api.visage.source.VisageSourceUtils;
import org.netbeans.spi.project.AuxiliaryConfiguration;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.netbeans.spi.project.support.ant.PropertyProvider;
import org.netbeans.spi.project.support.ant.GeneratedFilesHelper;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.PropertyUtils;
import org.openide.ErrorManager;
import org.openide.NotifyDescriptor;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.modules.InstalledFileLocator;
import org.openide.util.NbBundle;
import org.openide.xml.XMLUtil;
import org.w3c.dom.Element;
import java.io.InputStream;
import java.io.IOException;
import java.io.FileInputStream;
import java.io.BufferedInputStream;
import java.io.OutputStreamWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.Map;
import java.util.Properties;
import javax.swing.JButton;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.modules.visage.project.VisageProject;
import org.netbeans.modules.visage.project.VisageProjectUtil;
import org.netbeans.modules.visage.project.api.VisagePropertyEvaluator;
import org.netbeans.modules.visage.project.classpath.ClassPathProviderImpl;
import org.netbeans.modules.visage.project.ui.customizer.VisageProjectProperties;
import org.netbeans.modules.java.api.common.ant.UpdateHelper;
import org.netbeans.modules.visage.project.ui.customizer.MainClassWarning;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import org.openide.awt.MouseUtils;

/**
 * @author Tomas Hurka
 * @author Ian Formanek
 */
public final class VisageProjectTypeProfiler extends AbstractProjectTypeProfiler {
    //~ Inner Classes ------------------------------------------------------------------------------------------------------------

    private static class MyPropertyProvider implements PropertyProvider {
        //~ Instance fields ------------------------------------------------------------------------------------------------------

        private Properties props;

        //~ Constructors ---------------------------------------------------------------------------------------------------------
        private MyPropertyProvider(Properties props) {
            this.props = props;
        }

        //~ Methods --------------------------------------------------------------------------------------------------------------
        public Map /*<String,String>*/ getProperties() {
            return props;
        }

        public void addChangeListener(ChangeListener l) {
        }

        public void removeChangeListener(ChangeListener l) {
        }
    }

    //~ Static fields/initializers -----------------------------------------------------------------------------------------------

    // -----
    // I18N String constants
    private static final String MODIFY_BUILDSCRIPT_CAPTION = NbBundle.getMessage(VisageProjectTypeProfiler.class,
            "VisageProjectTypeProfiler_ModifyBuildScriptCaption"); // NOI18N
    private static final String MODIFY_BUILDSCRIPT_MSG = NbBundle.getMessage(VisageProjectTypeProfiler.class,
            "VisageProjectTypeProfiler_ModifyBuildScriptMsg"); // NOI18N
    private static final String REGENERATE_BUILDSCRIPT_MSG = NbBundle.getMessage(VisageProjectTypeProfiler.class,
            "VisageProjectTypeProfiler_RegenerateBuildScriptMsg"); // NOI18N
    private static final String CANNOT_FIND_BUILDSCRIPT_MSG = NbBundle.getMessage(VisageProjectTypeProfiler.class,
            "VisageProjectTypeProfiler_CannotFindBuildScriptMsg"); // NOI18N
    private static final String CANNOT_BACKUP_BUILDSCRIPT_MSG = NbBundle.getMessage(VisageProjectTypeProfiler.class,
            "VisageProjectTypeProfiler_CannotBackupBuildScriptMsg"); // NOI18N
    private static final String MODIFY_BUILDSCRIPT_MANUALLY_MSG = NbBundle.getMessage(VisageProjectTypeProfiler.class,
            "VisageProjectTypeProfiler_ModifyBuildScriptManuallyMsg"); // NOI18N
    // -----
    public static final ErrorManager err = ErrorManager.getDefault().getInstance("org.netbeans.modules.visage.profiler"); // NOI18N
    private static final String Visage_PROJECT_NAMESPACE_40 = "http://www.netbeans.org/ns/visage-project/1"; // NOI18N
    private static final String Visage_PROJECT_NAMESPACE_41 = "http://www.netbeans.org/ns/visage-project/2"; // NOI18N
    private static final String Visage_PROJECT_NAMESPACE_50 = "http://www.netbeans.org/ns/visage-project/3"; // NOI18N
    private static final String STANDARD_IMPORT_STRING = "<import file=\"nbproject/build-impl.xml\"/>"; // NOI18N
    private static final String PROFILER_IMPORT_STRING = "<import file=\"nbproject/profiler-build-impl.xml\"/>"; // NOI18N
    private static final String PROFILE_VERSION_ATTRIBUTE = "version"; // NOI18N
    private static final String VERSION_NUMBER = "0.9.1"; // NOI18N
    private static final String JAVAFX_MIME_TYPE = "text/x-fx"; // NOI18N
    private static final String JAVA_MIME_TYPE = "text/x-java"; // NOI18N    

    //~ Instance fields ----------------------------------------------------------------------------------------------------------
    private String mainClassSetManually = null; // used for case when the main class is not set in project and user is prompted for it

    public String getProfilerTargetName(final Project project, final FileObject buildScript, final int type,
            final FileObject profiledClassFile) {
        switch (type) {
            case TARGET_PROFILE:
                return "profile"; // NOI18N
            case TARGET_PROFILE_SINGLE:

                if (SourceUtils.isApplet(profiledClassFile) ||
                        VisageSourceUtils.isVisageApplet(profiledClassFile)) {
                    return "profile-applet"; // NOI18N
                } else {
                    return "profile-single"; // NOI18N
                }
            case TARGET_PROFILE_TEST:
                return null; // not currently supported // "profile-test"; // NOI18N
            case TARGET_PROFILE_TEST_SINGLE:
                return "profile-test-single"; // NOI18N
            default:
                return null;
        }
    }

    // --- ProjectTypeProfiler implementation ------------------------------------------------------------------------------
    public boolean isProfilingSupported(final Project project) {
        VisagePropertyEvaluator evaluator = project.getLookup().lookup(VisagePropertyEvaluator.class);
        if (evaluator == null) return false; // not a visage project
        
        String profile = evaluator.evaluator().getProperty("visage.profile");
        profile = profile != null ? profile : "desktop"; // some projects don't have the property set for "Standard"

        if (evaluator == null || !"desktop".equals(profile)) return false;

        final AuxiliaryConfiguration aux = (AuxiliaryConfiguration) project.getLookup().lookup(AuxiliaryConfiguration.class);

        if (aux == null) {
            ProfilerLogger.severe("Auxiliary Configuration is null for Project: " + project); // NOI18N

            return false;
        }

        Element e = aux.getConfigurationFragment("data", Visage_PROJECT_NAMESPACE_40, true); // NOI18N

        if (e == null) {
            e = aux.getConfigurationFragment("data", Visage_PROJECT_NAMESPACE_41, true); // NOI18N
        }

        if (e == null) {
            e = aux.getConfigurationFragment("data", Visage_PROJECT_NAMESPACE_50, true); // NOI18N
        }

        return (e != null);
    }

    @Override
    public JavaPlatform getProjectJavaPlatform(Project project) {
        PropertyEvaluator props = getProjectProperties(project);
        String platformName = props.getProperty("platform.active"); // NOI18N

        if (platformName == null) {
            return null; // not provided for some reason
        }

        JavaPlatformManager jpm = JavaPlatformManager.getDefault();

        if (platformName.equals("default_platform")) {  // NOI18N
            return jpm.getDefaultPlatform();
        }

        JavaPlatform[] platforms = jpm.getPlatforms(null, new Specification("visage", null)); // NOI18N

        for (int i = 0; i < platforms.length; i++) {
            JavaPlatform platform = platforms[i];
            String antName = (String) platform.getProperties().get("platform.ant.name"); // NOI18N

            if (antName.equals(platformName)) {
                return platform;
            }
        }

        return null;
    }

    public boolean checkProjectCanBeProfiled(final Project project, final FileObject profiledClassFile) {
        if (profiledClassFile == null) {
            String config = ((VisageProject)project).evaluator().getProperty("config"); // VisageConfigurationProvider.PROP_CONFIG
            String path;
            if (config == null || config.length() == 0) {
                path = AntProjectHelper.PROJECT_PROPERTIES_PATH;
            } else {
                // Set main class for a particular config only.
                path = "nbproject/configs/" + config + ".properties"; // NOI18N
            }

            // check project's main class
            // Check whether main class is defined in this config. Note that we use the evaluator,
            // not ep.getProperty(MAIN_CLASS), since it is permissible for the default pseudoconfig
            // to define a main class - in this case an active config need not override it.
            UpdateHelper helper = ((VisageProject)project).getUpdateHelper();
            EditableProperties ep = helper.getProperties(path);

            String mainClass = ((VisageProject)project).evaluator().getProperty(VisageProjectProperties.MAIN_CLASS);
            MainClassStatus result = isSetMainClass ((VisageProject)project, mainClass);
  
            if (result != MainClassStatus.SET_AND_VALID) {
                do {
                    // show warning, if cancel then return
                    if (showMainClassWarning ((VisageProject)project, mainClass, ProjectUtils.getInformation(project).getDisplayName(), ep,result)) {
                        return false;
                    }
                    // No longer use the evaluator: have not called putProperties yet so it would not work.
                    mainClass = ep.get(VisageProjectProperties.MAIN_CLASS);
                    result=isSetMainClass ((VisageProject)project, mainClass);
                } while (result != MainClassStatus.SET_AND_VALID);
                try {
                    if (helper.requestUpdate()) {
                        helper.putProperties(path, ep);
                        ProjectManager.getDefault().saveProject(project);
                    }
                    else {
                        return false;
                    }
                } catch (IOException ioe) {
                    ErrorManager.getDefault().log(ErrorManager.INFORMATIONAL, "Error while saving project: " + ioe); // NOI18N
                }
            }
//            final PropertyEvaluator pp = getProjectProperties(project);
//            String profiledClass = pp.getProperty("main.class"); // NOI18N
//
//            if ((profiledClass == null) || "".equals(profiledClass.trim())) { // NOI18N
//                mainClassSetManually = ProjectUtilities.selectMainClass(project, null, ProjectUtilities.getProjectName(project),
//                        -1);
//                if (mainClassSetManually == null) {
//                    return false;
//                }
//            }

            return true;
        } else {
            return isFileObjectSupported(project, profiledClassFile);
        }
    }

    public boolean checkProjectIsModifiedForProfiler(final Project project) {
        if (ProjectUtilities.isProfilerIntegrated(project)) {
            return true; // already modified by this version, nothing more to do
        }

        String projectName = ProjectUtils.getInformation(project).getDisplayName();
        String caption = MessageFormat.format(MODIFY_BUILDSCRIPT_CAPTION, new Object[]{projectName});
        String message = MessageFormat.format(MODIFY_BUILDSCRIPT_MSG, new Object[]{projectName, "build-before-profiler.xml"}); // NOI18N

        if (ProfilerDialogs.notify(new NotifyDescriptor(message, caption, NotifyDescriptor.OK_CANCEL_OPTION,
                NotifyDescriptor.INFORMATION_MESSAGE,
                new Object[]{
                    NotifyDescriptor.OK_OPTION, NotifyDescriptor.CANCEL_OPTION
                }, NotifyDescriptor.OK_OPTION)) != NotifyDescriptor.OK_OPTION) {
            return false; // cancelled by the user
        }

        // we are going to regenerate the build script in one of 3 cases:
        // 1. it has not been generated yet
        // 2. the profiler version has been changed (see above)
        // 3. the stylesheet changed (usually should be caught by 2.)
        final GeneratedFilesHelper gfh = new GeneratedFilesHelper(project.getProjectDirectory());
        int flags = gfh.getBuildScriptState("nbproject/profiler-build-impl.xml", // NOI18N
                VisageProjectTypeProfiler.class.getResource("profiler-build-impl.xsl")); // NOI18N

        if (((flags & GeneratedFilesHelper.FLAG_MISSING) != 0) || ((flags & GeneratedFilesHelper.FLAG_OLD_STYLESHEET) != 0)) {
            try {
                if ((flags & GeneratedFilesHelper.FLAG_MODIFIED) != 0) {
                    if (ProfilerDialogs.notify(new NotifyDescriptor.Confirmation(MessageFormat.format(REGENERATE_BUILDSCRIPT_MSG,
                            new Object[]{
                                "profiler-build-impl.xml" // NOI18N
                            }),
                            NotifyDescriptor.OK_CANCEL_OPTION)) != NotifyDescriptor.OK_OPTION) {
                        return false;
                    }
                }

                gfh.generateBuildScriptFromStylesheet("nbproject/profiler-build-impl.xml", // NOI18N
                        VisageProjectTypeProfiler.class.getResource("profiler-build-impl.xsl")); // NOI18N
            } catch (IOException e1) {
                err.notify(ErrorManager.WARNING, e1);

                return false;
            }
        }

        // store info about profiler with project's auxiliary configuration
        final Element profilerFragment = XMLUtil.createDocument("ignore", null, null, null) // NOI18N
                .createElementNS(ProjectUtilities.PROFILER_NAME_SPACE, "data"); // NOI18N
        profilerFragment.setAttribute(PROFILE_VERSION_ATTRIBUTE, VERSION_NUMBER);
        ((AuxiliaryConfiguration) project.getLookup().lookup(AuxiliaryConfiguration.class)).putConfigurationFragment(profilerFragment,
                false);

        try {
            ProjectManager.getDefault().saveProject(project);
        } catch (IOException e1) {
            err.notify(e1);
            ProfilerLogger.log(e1);

            return false;
        }

        final String buildScript = ProjectUtilities.getProjectBuildScript(project);

        if (buildScript == null) {
            ProfilerDialogs.notify(new NotifyDescriptor.Message(MessageFormat.format(CANNOT_FIND_BUILDSCRIPT_MSG,
                    new Object[]{"build.xml"}), // NOI18N
                    NotifyDescriptor.ERROR_MESSAGE));

            return false;
        }

        if (!ProjectUtilities.backupBuildScript(project)) {
            if (ProfilerDialogs.notify(new NotifyDescriptor.Confirmation(CANNOT_BACKUP_BUILDSCRIPT_MSG,
                    NotifyDescriptor.OK_CANCEL_OPTION,
                    NotifyDescriptor.WARNING_MESSAGE)) != NotifyDescriptor.OK_OPTION) {
                return false; // cancelled by the user
            }
        }

        final StringBuffer newDataBuffer = new StringBuffer(buildScript.length() + 200);
        final int importIndex = buildScript.indexOf(STANDARD_IMPORT_STRING);

        if (importIndex == -1) {
            // notify the user that the build script cannot be modified, and he should perform the change himself
            ProfilerDialogs.notify(new NotifyDescriptor.Message(MessageFormat.format(MODIFY_BUILDSCRIPT_MANUALLY_MSG,
                    new Object[]{
                        "build.xml", // NOI18N
                        "<import file=\"nbproject/profiler-build-impl.xml\"/>" // NOI18N
                    }),
                    NotifyDescriptor.WARNING_MESSAGE));

            return false;
        }

        String indent = ""; // NOI18N
        int idx = importIndex - 1;

        while (idx >= 0) {
            if (buildScript.charAt(idx) == ' ') { // NOI18N
                indent = " " + indent; // NOI18N
            } else if (buildScript.charAt(idx) == '\t') { // NOI18N
                indent = "\t" + indent; // NOI18N
            } else {
                break;
            }

            idx--;
        }

        newDataBuffer.append(buildScript.substring(0, importIndex + STANDARD_IMPORT_STRING.length() + 1));
        newDataBuffer.append("\n"); // NOI18N
        newDataBuffer.append(indent);
        newDataBuffer.append(PROFILER_IMPORT_STRING);
        newDataBuffer.append(buildScript.substring(importIndex + STANDARD_IMPORT_STRING.length() + 1));

        final FileObject buildFile = project.getProjectDirectory().getFileObject("build.xml"); // NOI18N
        FileLock lock = null;
        OutputStreamWriter writer = null;

        try {
            lock = buildFile.lock();
            writer = new OutputStreamWriter(buildFile.getOutputStream(lock), "UTF-8"); // NOI18N // According to Issue 65557, build.xml uses UTF-8, not default encoding!
            writer.write(newDataBuffer.toString());
        } catch (FileNotFoundException e1) {
            ProfilerLogger.log(e1);
            err.notify(e1);
        } catch (IOException e1) {
            ProfilerLogger.log(e1);
            err.notify(e1);
        } finally {
            if (lock != null) {
                lock.releaseLock();
            }

            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException ex) {
                }
            }
        }

        return true;
    }

    @Override
    public void configurePropertiesForProfiling(final Properties props, final Project project, final FileObject profiledClassFile) {
        PropertyEvaluator projectProps = getProjectProperties(project);
        if (profiledClassFile == null) {
            if (mainClassSetManually != null) {
                props.put("main.class", mainClassSetManually); // NOI18N
                mainClassSetManually = null;
            }
        } else {
            // In case the class to profile is explicitely selected (profile-single)
            // 1. specify profiled class name
            if (SourceUtils.isApplet(profiledClassFile) ||
                    VisageSourceUtils.isVisageApplet(profiledClassFile)) {
                String jvmargs = props.getProperty("run.jvmargs"); // NOI18N

                URL url = null;

                // do this only when security policy is not set manually
                if ((jvmargs == null) || !(jvmargs.indexOf("java.security.policy") > 0)) { //NOI18N

                    String buildDirProp = projectProps.getProperty("build.dir"); //NOI18N
                    // TODO [M9] what if buildDirProp is null?

                    FileObject buildFolder = ProjectUtilities.getOrCreateBuildFolder(project, buildDirProp);

                    AppletSupport.generateSecurityPolicy(project.getProjectDirectory(), buildFolder);

                    if ((jvmargs == null) || (jvmargs.length() == 0)) {
                        props.setProperty("run.jvmargs", // NOI18N
                                "-Djava.security.policy=" + FileUtil.toFile(buildFolder).getPath() + File.separator // NOI18N
                                + "applet.policy"); //NOI18N
                    } else {
                        props.setProperty("run.jvmargs", // NOI18N
                                jvmargs + " -Djava.security.policy=" + FileUtil.toFile(buildFolder).getPath() // NOI18N
                                + File.separator + "applet.policy"); //NOI18N
                    }
                }

                if (profiledClassFile.existsExt("html") || profiledClassFile.existsExt("HTML")) { //NOI18N
                    url = ProjectUtilities.copyAppletHTML(project, getProjectProperties(project), profiledClassFile, "html"); //NOI18N
                } else {
                    url = ProjectUtilities.generateAppletHTML(project, getProjectProperties(project), profiledClassFile);
                }

                if (url == null) {
                    return; // TODO: fail?
                }

                props.setProperty("applet.url", url.toString()); // NOI18N
            } else {
                final String profiledClass = SourceUtils.getToplevelClassName(profiledClassFile);
                if (null != profiledClass) {
                    props.setProperty("profile.class", profiledClass); //NOI18N
                    final String clazz = FileUtil.getRelativePath(ProjectUtilities.getRootOf(ProjectUtilities.getSourceRoots(project),
                            profiledClassFile), profiledClassFile);
                    props.setProperty("javac.includes", clazz); //NOI18N
                } else {
                    if (project instanceof VisageProject) {
                        VisageProject projectVSG = (VisageProject) project;
                        String clazz = FileUtil.getRelativePath(VisageProjectUtilities.getRoot(projectVSG.getFOSourceRoots(), profiledClassFile), profiledClassFile);
                        props.setProperty("javac.includes", clazz); // NOI18N
                        clazz = clazz.substring(0, clazz.length() - 3);
                        clazz = clazz.replace('/', '.'); // NOI18N
                        props.setProperty("profile.class", clazz); //NOI18N
                    }
                }
            }
        }
    }

    @Override
    public void setupProjectSessionSettings(final Project project, final SessionSettings ss) {
        final PropertyEvaluator pp = getProjectProperties(project);

        if (mainClassSetManually == null) {
            String mainClass = pp.getProperty("main.class"); // NOI18N
            ss.setMainClass((mainClass != null) ? mainClass : ""); // NOI18N
        } else {
            ss.setMainClass(mainClassSetManually);
        }

        // is this all really necessary???
        String appArgs = pp.getProperty("application.args"); // NOI18N
        ss.setMainArgs((appArgs != null) ? appArgs : ""); // NOI18N

        String runCP = pp.getProperty("run.classpath"); // NOI18N
        ss.setMainClassPath((runCP != null) ? runCP : ""); // NOI18N

        String jvmArgs = pp.getProperty("run.jvmargs"); // NOI18N
        ss.setJVMArgs((jvmArgs != null) ? jvmArgs : ""); // NOI18N
    }

    @Override
    public boolean supportsSettingsOverride() {
        return true; // supported for Visage project
    }

    @Override
    public boolean supportsUnintegrate(Project project) {
        return true;
    }

    @Override
    public void unintegrateProfiler(Project project) {
        ProjectUtilities.unintegrateProfiler(project);
    }

    // --- Private methods -------------------------------------------------------------------------------------------------
    private PropertyEvaluator getProjectProperties(final Project project) {
        final Properties privateProps = new Properties();
        final Properties projectProps = new Properties();
        final Properties userPropsProps = new Properties();
        final Properties configProps = new Properties();

        final FileObject privatePropsFile = project.getProjectDirectory().getFileObject(AntProjectHelper.PRIVATE_PROPERTIES_PATH);
        final FileObject projectPropsFile = project.getProjectDirectory().getFileObject(AntProjectHelper.PROJECT_PROPERTIES_PATH);
        final File userPropsFile = InstalledFileLocator.getDefault().locate("build.properties", null, false); // NOI18N
        final FileObject configPropsFile = project.getProjectDirectory().getFileObject("nbproject/private/config.properties"); // NOI18N
        final FileObject configPropsDir = project.getProjectDirectory().getFileObject("nbproject/configs"); // NOI18N

        ProjectManager.mutex().readAccess(new Runnable() {

            public void run() {
                // the order is 1. private, 2. project, 3. user to reflect how Ant handles property definitions (immutable, once set property value cannot be changed)
                if (privatePropsFile != null) {
                    try {
                        final InputStream is = privatePropsFile.getInputStream();

                        try {
                            privateProps.load(is);
                        } finally {
                            is.close();
                        }
                    } catch (IOException e) {
                        err.notify(ErrorManager.INFORMATIONAL, e);
                    }
                }

                if (projectPropsFile != null) {
                    try {
                        final InputStream is = projectPropsFile.getInputStream();

                        try {
                            projectProps.load(is);
                        } finally {
                            is.close();
                        }
                    } catch (IOException e) {
                        err.notify(ErrorManager.INFORMATIONAL, e);
                    }
                }

                if (userPropsFile != null) {
                    try {
                        final InputStream is = new BufferedInputStream(new FileInputStream(userPropsFile));

                        try {
                            userPropsProps.load(is);
                        } finally {
                            is.close();
                        }
                    } catch (IOException e) {
                        err.notify(ErrorManager.INFORMATIONAL, e);
                    }
                }

                if ((configPropsDir != null) && (configPropsFile != null)) {
                    try {
                        InputStream is = configPropsFile.getInputStream();
                        Properties activeConfigProps = new Properties();

                        try {
                            activeConfigProps.load(is);

                            String activeConfig = activeConfigProps.getProperty("config"); // NOI18N

                            if ((activeConfig != null) && (activeConfig.length() > 0)) {
                                FileObject configSpecPropFile = configPropsDir.getFileObject(activeConfig + ".properties"); // NOI18N

                                if (configSpecPropFile != null) {
                                    is = configSpecPropFile.getInputStream();
                                    configProps.load(is);
                                }
                            }
                        } finally {
                            is.close();
                        }
                    } catch (IOException e) {
                        err.notify(ErrorManager.INFORMATIONAL, e);
                    }
                }
            }
        });

        PropertyEvaluator pe = PropertyUtils.sequentialPropertyEvaluator(null,
                new PropertyProvider[]{
                    new MyPropertyProvider(configProps),
                    new MyPropertyProvider(privateProps),
                    new MyPropertyProvider(userPropsProps),
                    new MyPropertyProvider(projectProps)
                });

        return pe;
    }

    // Copied over from VisageActionProvider
    private static enum MainClassStatus {
        SET_AND_VALID,
        SET_BUT_INVALID,
        UNSET
    }

    /**
     * Copied over from VisageActionProvider
     * Tests if the main class is set
     * @param sourcesRoots source roots
     * @param mainClass main class name
     * @return status code
     */
    private MainClassStatus isSetMainClass(VisageProject project, String mainClass) {
        FileObject[] sourcesRoots = project.getSourceRoots().getRoots();
        
        if (mainClass == null || mainClass.length () == 0) {
            return MainClassStatus.UNSET;
        }
        if (sourcesRoots.length > 0) {
            ClassPath bootPath = ClassPath.getClassPath (sourcesRoots[0], ClassPath.BOOT);        //Single compilation unit
            ClassPath compilePath = ClassPath.getClassPath (sourcesRoots[0], ClassPath.EXECUTE);
            ClassPath sourcePath = ClassPath.getClassPath(sourcesRoots[0], ClassPath.SOURCE);
            if (VisageProjectUtil.isMainClass (mainClass, bootPath, compilePath, sourcePath)) {
                return MainClassStatus.SET_AND_VALID;
            }
        }
        else {
            ClassPathProviderImpl cpProvider = project.getClassPathProvider();
            if (cpProvider != null) {
                ClassPath bootPath = cpProvider.getProjectSourcesClassPath(ClassPath.BOOT);
                ClassPath compilePath = cpProvider.getProjectSourcesClassPath(ClassPath.EXECUTE);
                ClassPath sourcePath = cpProvider.getProjectSourcesClassPath(ClassPath.SOURCE);   //Empty ClassPath
                if (VisageProjectUtil.isMainClass (mainClass, bootPath, compilePath, sourcePath)) {
                    return MainClassStatus.SET_AND_VALID;
                }
            }
        }
        return MainClassStatus.SET_BUT_INVALID;
    }

    /**
     * Copied over from VisageActionProvider
     * Asks user for name of main class
     * @param mainClass current main class
     * @param projectName the name of project
     * @param ep project.properties to possibly edit
     * @param messgeType type of dialog
     * @return true if user selected main class
     */
    private boolean showMainClassWarning(VisageProject project, String mainClass, String projectName, EditableProperties ep, MainClassStatus messageType) {
        boolean canceled;
        final JButton okButton = new JButton (NbBundle.getMessage (MainClassWarning.class, "LBL_MainClassWarning_ChooseMainClass_OK")); // NOI18N
        okButton.getAccessibleContext().setAccessibleDescription (NbBundle.getMessage (MainClassWarning.class, "AD_MainClassWarning_ChooseMainClass_OK")); // NOI18N

        // main class goes wrong => warning
        String message;
        switch (messageType) {
            case UNSET:
                message = MessageFormat.format (NbBundle.getMessage(MainClassWarning.class,"LBL_MainClassNotFound"), new Object[] { // NOI18N
                    projectName
                });
                break;
            case SET_BUT_INVALID:
                message = MessageFormat.format (NbBundle.getMessage(MainClassWarning.class,"LBL_MainClassWrong"), new Object[] { // NOI18N
                    mainClass,
                    projectName
                });
                break;
            default:
                throw new IllegalArgumentException ();
        }
        final MainClassWarning panel = new MainClassWarning (message,project.getSourceRoots().getRoots());
        Object[] options = new Object[] {
            okButton,
            DialogDescriptor.CANCEL_OPTION
        };

        panel.addChangeListener (new ChangeListener () {
           public void stateChanged (ChangeEvent e) {
               if (e.getSource () instanceof MouseEvent && MouseUtils.isDoubleClick (((MouseEvent)e.getSource ()))) {
                   // click button and the finish dialog with selected class
                   okButton.doClick ();
               } else {
                   okButton.setEnabled (panel.getSelectedMainClass () != null);
               }
           }
        });

        okButton.setEnabled (false);
        DialogDescriptor desc = new DialogDescriptor (panel,
            NbBundle.getMessage (MainClassWarning.class, "CTL_MainClassWarning_Title", ProjectUtils.getInformation(project).getDisplayName()), // NOI18N
            true, options, options[0], DialogDescriptor.BOTTOM_ALIGN, null, null);
        desc.setMessageType (DialogDescriptor.INFORMATION_MESSAGE);
        Dialog dlg = DialogDisplayer.getDefault ().createDialog (desc);
        dlg.setVisible (true);
        if (desc.getValue() != options[0]) {
            canceled = true;
        } else {
            mainClass = panel.getSelectedMainClass ();
            canceled = false;
            ep.put(VisageProjectProperties.MAIN_CLASS, mainClass == null ? "" : mainClass); // NOI18N
        }
        dlg.dispose();

        return canceled;
    }
}
