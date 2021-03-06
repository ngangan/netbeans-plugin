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

package org.netbeans.modules.visage.project.ui.customizer;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collection;
import java.util.Properties;
import javax.swing.ButtonModel;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.PlainDocument;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.modules.visage.project.VisageProject;
import org.netbeans.modules.visage.project.api.VisageProjectConfigurations;
import org.netbeans.spi.project.ProjectConfiguration;
import org.netbeans.spi.project.ProjectConfigurationProvider;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.netbeans.spi.project.support.ant.ui.StoreGroup;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Mutex;
import org.openide.util.MutexException;
import org.openide.util.NbBundle;

/**
 *
 * @author Milan Kubec
 */
public class WebStartProjectProperties {

    public static final String JNLP_ICON = "jnlp.icon"; // NOI18N
    public static final String JNLP_OFFLINE = "jnlp.offline-allowed"; // NOI18N
    public static final String JNLP_CBASE_TYPE = "jnlp.codebase.type"; // NOI18N
    public static final String JNLP_CBASE_USER = "jnlp.codebase.user"; // NOI18N
    public static final String JNLP_CBASE_URL = "jnlp.codebase.url"; // NOI18N
    public static final String JNLP_SPEC = "jnlp.spec"; // NOI18N
    public static final String JNLP_INIT_HEAP = "jnlp.initial-heap-size"; // NOI18N
    public static final String JNLP_MAX_HEAP = "jnlp.max-heap-size"; // NOI18N
    public static final String JNLP_SIGNED = "jnlp.signed"; // NOI18N
    public static final String CB_TYPE_LOCAL = "local"; // NOI18N
    public static final String CB_TYPE_USER = "user"; // NOI18N
    public static final String UP_TYPE_ALWAYS = "always"; // NOI18N
    public static final String UP_TYPE_TIMEOUT = "timeout"; // NOI18N
    public static final String UP_TYPE_BACKGROUND = "background"; // NOI18N
    public static final String JNLP_UPDATE_MODEL = "jnlp.update.model"; // NOI18N
    public static final String PACK200_COMPRESS_MODEL="pack200.jar.compress"; // NOI18N
    public static final String PACK200_ENABLE_MODEL = "jnlp.packEnabled"; // NOI18N
    // special value to persist Ant script handling
    public static final String CB_URL_WEB_PROP_VALUE = "$$$$codebase"; // NOI18N
    public static final String JNLP_VISAGE_MAIN_JAR = "jnlp.visage.main.jar.value"; // NOI18N
    private StoreGroup jnlpPropGroup = new StoreGroup();
    private PropertyEvaluator evaluator;
    private VisageProject visageProject;
    // Models
    ButtonModel allowOfflineModel;
    ButtonModel signedModel;
    ButtonModel pack200Model;
    ButtonModel enablePack200Model;
    ComboBoxModel codebaseModel;
    ComboBoxModel updateModel;
    // and Documents
    Document iconDocument;
    Document codebaseURLDocument;

    /** Creates a new instance of JWSProjectProperties */
    public WebStartProjectProperties(VisageProject project, PropertyEvaluator evaluator) {
        visageProject = project;
        this.evaluator = evaluator;
        /*
        visageProject = context.lookup(org.netbeans.api.project.Project.class);
        if (visageProject != null) {
        visagePropEval = visageProject.getLookup().lookup(org.netbeans.modules.visage.project.api.VisagePropertyEvaluator.class);
        } else {
        // XXX
        }
        evaluator = visagePropEval.evaluator();
         */
        allowOfflineModel = jnlpPropGroup.createToggleButtonModel(evaluator, JNLP_OFFLINE);
        signedModel = jnlpPropGroup.createToggleButtonModel(evaluator, JNLP_SIGNED);
        if (evaluator.getProperty(JNLP_SIGNED) == null) {
            signedModel.setSelected(true);
        }
        iconDocument = jnlpPropGroup.createStringDocument(evaluator, JNLP_ICON);
        pack200Model = jnlpPropGroup.createToggleButtonModel(evaluator, PACK200_COMPRESS_MODEL);
        enablePack200Model = jnlpPropGroup.createToggleButtonModel(evaluator, PACK200_ENABLE_MODEL);
        codebaseModel = new CodebaseComboBoxModel();
        updateModel = new UpdateComboBoxModel();
        codebaseURLDocument = createCBTextFieldDocument();
    }

    private void storeRest(EditableProperties editableProps) {
        String selItem = ((CodebaseComboBoxModel) codebaseModel).getSelectedCodebaseItem();
        String propName = null;
        String propValue = null;
        if (CB_TYPE_USER.equals(selItem)) {
            propName = JNLP_CBASE_USER;
            try {
                propValue = codebaseURLDocument.getText(0, codebaseURLDocument.getLength());
            } catch (BadLocationException ex) {
                // do not store anything
                // XXX log the exc
                return;
            }
        } else if (CB_TYPE_LOCAL.equals(selItem)) {
            propName = JNLP_CBASE_URL;
            propValue = getProjectDistDir();
        }
        if (propName == null || propValue == null) {
            return;
        } else {
            editableProps.setProperty(JNLP_CBASE_TYPE, selItem);
            editableProps.setProperty(propName, propValue);
        }
        editableProps.setProperty(JNLP_UPDATE_MODEL, ((UpdateComboBoxModel)updateModel).getSelectedUpdateItem());
    }

    public void store() throws IOException {

        final EditableProperties ep = new EditableProperties(true);
        final FileObject projPropsFO = visageProject.getProjectDirectory().getFileObject("nbproject/project.properties"); // NOI18N

        try {
            final InputStream is = projPropsFO.getInputStream();
            ProjectManager.mutex().writeAccess(new Mutex.ExceptionAction<Void>() {

                public Void run() throws Exception {
                    try {
                        ep.load(is);
                    } finally {
                        if (is != null) {
                            is.close();
                        }
                    }
                    jnlpPropGroup.store(ep);
                    storeRest(ep);
                    OutputStream os = null;
                    FileLock lock = null;
                    try {
                        lock = projPropsFO.lock();
                        os = projPropsFO.getOutputStream(lock);
                        ep.store(os);
                    } finally {
                        if (lock != null) {
                            lock.releaseLock();
                        }
                        if (os != null) {
                            os.close();
                        }
                    }
                    return null;
                }
            });
        } catch (MutexException mux) {
            throw (IOException) mux.getException();
        }
    }

    private Document createCBTextFieldDocument() {
        Document doc = new PlainDocument();
        String valueURL = evaluator.getProperty(JNLP_CBASE_USER);
        String valueType = evaluator.getProperty(JNLP_CBASE_TYPE);
        String docString = ""; // NOI18N
        if (CB_TYPE_LOCAL.equals(valueType)) {
            docString = getProjectDistDir();
        } else if (CB_TYPE_USER.equals(valueType)) {
            docString = getCodebaseLocation();
        }
        try {
            doc.insertString(0, docString, null);
        } catch (BadLocationException ex) {
            // do nothing, just return PlainDocument
            // XXX log the exc
        }
        return doc;
    }

    /*
    private StyledDocument createDescTextAreaDocument() {
    StyledDocument doc = new DefaultStyledDocument();
    String docString = "";
    docString = evaluator.getProperty(JNLP_DESC);
    try {
    doc.insertString(0, docString, null);
    } catch (BadLocationException ex) {
    // do nothing, just return DefaultStyledDocument
    }
    return doc;
    }
     */
    public String getCodebaseLocation() {
        return evaluator.getProperty(JNLP_CBASE_USER);
    }

    public String getProjectDistDir() {
        File distDir = new File(FileUtil.toFile(visageProject.getProjectDirectory()), evaluator.getProperty("dist.dir")); // NOI18N
        return distDir.toURI().toString();
    }

    // only should return JNLP properties
    public String getProperty(String propName) {
        return evaluator.getProperty(propName);
    }

//    public void createConfigurationFiles(ProjectConfigurationProvider configProvider, Boolean enabled) throws IOException {
//        if (enabled) {
//            // XXX logging
//            // test if the file already exists, if so do not z tigenerate, just set as active
//            VisageProjectConfigurations.createConfigurationFiles(visageProject, "JWS_generated", prepareSharedProps(), null); // NOI18N
//            setActiveConfig(configProvider, NbBundle.getBundle(VisageCompositePanelProvider.class).getString("LBL_Category_WebStart"));
////            copyTemplate(visageProject);
////            modifyBuildXml(visageProject);
//        } else {
//            setActiveConfig(configProvider, NbBundle.getBundle(VisageCompositePanelProvider.class).getString("LBL_Category_Default"));
//        }
//        CustomizerWebStart.runComponent.setCheckboxEnabled(true);
//        CustomizerWebStart.runComponent.setHintVisible(true);
//    }

    private void setActiveConfig(final ProjectConfigurationProvider provider, String displayName) throws IOException {
        Collection<ProjectConfiguration> configs = provider.getConfigurations();
        for (final ProjectConfiguration c : configs) {
            if (displayName.equals(c.getDisplayName())) {
                try {
                    ProjectManager.mutex().writeAccess(new Mutex.ExceptionAction<Void>() {

                        public Void run() throws Exception {
                            provider.setActiveConfiguration(c);
                            return null;
                        }
                    });
                } catch (MutexException mex) {
                    throw (IOException) mex.getException();
                }
            }
        }
    }

//    private void copyTemplate(Project proj) throws IOException {
//        FileObject projDir = proj.getProjectDirectory();
//        FileObject jnlpBuildFile = projDir.getFileObject("nbproject/jnlp-impl.xml"); // NOI18N
//        if (jnlpBuildFile == null) {
//            FileSystem sfs = Repository.getDefault().getDefaultFileSystem();
//            FileObject templateFO = sfs.findResource("Templates/Project/Visage/jnlp-impl.xml"); // NOI18N
//            if (templateFO != null) {
//                FileUtil.copyFile(templateFO, projDir.getFileObject("nbproject"), "jnlp-impl"); // NOI18N
//            }
//        }
//    }
//
//    private void modifyBuildXml(Project proj) throws IOException {
//        FileObject projDir = proj.getProjectDirectory();
//        final FileObject buildXmlFO = projDir.getFileObject("build.xml"); // NOI18N
//        File buildXmlFile = FileUtil.toFile(buildXmlFO);
//        org.w3c.dom.Document xmlDoc = null;
//        try {
//            xmlDoc = XMLUtil.parse(new InputSource(buildXmlFile.toURI().toString()), false, true, null, null);
//        } catch (SAXException ex) {
//            ErrorManager.getDefault().notify(ex);
//        }
//        FileObject jnlpBuildFile = projDir.getFileObject("nbproject/jnlp-impl.xml"); // NOI18N
//        AntBuildExtender extender = proj.getLookup().lookup(AntBuildExtender.class);
//        if (extender != null) {
//            assert jnlpBuildFile != null;
//            if (extender.getExtension("jws") == null) {
//                // NOI18N
//                AntBuildExtender.Extension ext = extender.addExtension("jws", jnlpBuildFile); // NOI18N
//                ext.addDependency("jar", "jnlp"); // NOI18N
//            }
//            ProjectManager.getDefault().saveProject(proj);
//        } else {
//            Logger.getLogger(VisageCompositePanelProvider.class.getName()).log(Level.INFO, "Trying to include JWS build snippet in project type that doesn't support AntBuildExtender API contract."); // NOI18N
//        }
//
//        //TODO this piece shall not proceed when the upgrade to j2se-project/4 was cancelled.
//        //how to figure..
//        Element docElem = xmlDoc.getDocumentElement();
//        NodeList nl = docElem.getElementsByTagName("target"); // NOI18N
//        Element target = null;
//        for (int i = 0; i < nl.getLength(); i++) {
//            Element e = (Element) nl.item(i);
//            if (e.getAttribute("name") != null && "-post-jar".equals(e.getAttribute("name"))) {
//                // NOI18N
//                target = e;
//                break;
//            }
//        }
//        boolean changed = false;
//        if (target != null) {
//            if (target.getAttribute("depends") != null && target.getAttribute("depends").contains("jnlp")) {
//                // NOI18N
//                String old = target.getAttribute("depends"); // NOI18N
//                old = old.replaceAll("jnlp", ""); // NOI18N
//                old = old.replaceAll(",[\\s]*$", ""); // NOI18N
//                old = old.replaceAll("^[\\s]*,", ""); // NOI18N
//                old = old.replaceAll(",[\\s]*,", ","); // NOI18N
//                old = old.trim();
//                if (old.length() == 0) {
//                    target.removeAttribute("depends"); // NOI18N
//                } else {
//                    target.setAttribute("depends", old); // NOI18N
//                }
//                changed = true;
//            }
//        }
//        nl = docElem.getElementsByTagName("import"); // NOI18N
//        for (int i = 0; i < nl.getLength(); i++) {
//            Element e = (Element) nl.item(i);
//            if (e.getAttribute("file") != null && "nbproject/jnlp-impl.xml".equals(e.getAttribute("file"))) {
//                // NOI18N
//                e.getParentNode().removeChild(e);
//                changed = true;
//                break;
//            }
//        }
//
//        if (changed) {
//            final org.w3c.dom.Document fdoc = xmlDoc;
//            try {
//                ProjectManager.mutex().writeAccess(new Mutex.ExceptionAction<Void>() {
//
//                    public Void run() throws Exception {
//                        FileLock lock = buildXmlFO.lock();
//                        try {
//                            OutputStream os = buildXmlFO.getOutputStream(lock);
//                            try {
//                                XMLUtil.write(fdoc, os, "UTF-8"); // NOI18N
//                            } finally {
//                                os.close();
//                            }
//                        } finally {
//                            lock.releaseLock();
//                        }
//                        return null;
//                    }
//                });
//            } catch (MutexException mex) {
//                throw (IOException) mex.getException();
//            }
//        }
//    }

//    private Properties prepareSharedProps() {
//        Properties props = new Properties();
//        props.setProperty("$label", NbBundle.getBundle(VisageCompositePanelProvider.class).getString("LBL_Category_WebStart"));
//        if (CustomizerWebStart.runComponent.isRunCheckBoxSelected()) {
//            props.setProperty("$target.run", "jws-run"); // NOI18N
//            props.setProperty("$target.debug", "jws-debug"); // NOI18N
//        }
//        return props;
//    }
    // ----------

    public class CodebaseComboBoxModel extends DefaultComboBoxModel {

        String localLabel = NbBundle.getBundle(WebStartProjectProperties.class).getString("LBL_CB_Combo_Local"); // NOI18N
        String userLabel = NbBundle.getBundle(WebStartProjectProperties.class).getString("LBL_CB_Combo_User"); // NOI18N
        Object[] visItems = new Object[]{localLabel, userLabel};
        String[] cbItems = new String[]{CB_TYPE_LOCAL, CB_TYPE_USER};

        public CodebaseComboBoxModel() {
            super();
            addElement(visItems[0]);
            addElement(visItems[1]);
            String propValue = evaluator.getProperty(JNLP_CBASE_TYPE);
            if (cbItems[1].equals(propValue)) {
                setSelectedItem(visItems[1]);
            } else if (cbItems[0].equals(propValue)) {
                setSelectedItem(visItems[0]);
            }
        }

        public String getSelectedCodebaseItem() {
            return cbItems[getIndexOf(getSelectedItem())];
        }
    }

    public class UpdateComboBoxModel extends DefaultComboBoxModel {

        String alwaysLabel = NbBundle.getBundle(WebStartProjectProperties.class).getString("LBL_UP_Combo_Always"); // NOI18N
        String timeoutLabel = NbBundle.getBundle(WebStartProjectProperties.class).getString("LBL_UP_Combo_Timeout"); // NOI18N
        String backgroundLabel = NbBundle.getBundle(WebStartProjectProperties.class).getString("LBL_UP_Combo_Background"); // NOI18N
        Object[] visItems = new Object[]{alwaysLabel, timeoutLabel, backgroundLabel};
        String[] upItems = new String[]{UP_TYPE_ALWAYS, UP_TYPE_TIMEOUT, UP_TYPE_BACKGROUND};

        public UpdateComboBoxModel() {
            super();
            addElement(visItems[0]);
            addElement(visItems[1]);
            addElement(visItems[2]);
            String propValue = evaluator.getProperty(JNLP_UPDATE_MODEL);
            if (upItems[2].equals(propValue)) {
                setSelectedItem(visItems[2]);
            } else if (upItems[1].equals(propValue)) {
                setSelectedItem(visItems[1]);
            } else {
                setSelectedItem(visItems[0]);
            }
        }

        public String getSelectedUpdateItem() {
            return upItems[getIndexOf(getSelectedItem())];
        }
    }
}