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

package org.netbeans.modules.visage.project.ui;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.Action;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeListener;
import org.netbeans.api.project.Project;
import org.netbeans.modules.java.api.common.SourceRoots;
import org.netbeans.modules.java.api.common.ant.UpdateHelper;
import org.netbeans.modules.visage.project.VisageProject;
import org.netbeans.modules.visage.project.ui.customizer.CustomizerLibraries;
import org.netbeans.modules.visage.project.ui.customizer.VisageProjectProperties;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.netbeans.spi.project.support.ant.ReferenceHelper;
import org.netbeans.spi.project.ui.support.NodeFactory;
import org.netbeans.spi.project.ui.support.NodeList;
import org.openide.nodes.Node;
import org.openide.util.ChangeSupport;
import org.openide.util.NbBundle;

/**
 *
 * @author mkleint
 */
public final class LibrariesNodeFactory implements NodeFactory {
    
    /** Creates a new instance of LibrariesNodeFactory */
    public LibrariesNodeFactory() {
    }

    public NodeList createNodes(Project p) {
        VisageProject project = (VisageProject)p.getLookup().lookup(VisageProject.class);
        assert project != null;
        return new LibrariesNodeList(project);
    }

    private static class LibrariesNodeList implements NodeList<String>, PropertyChangeListener {
        private static final String LIBRARIES = "Libs"; //NOI18N
        private static final String TEST_LIBRARIES = "TestLibs"; //NOI18N

        private SourceRoots testSources;
        private VisageProject project;
        private final ChangeSupport changeSupport = new ChangeSupport(this);

        private PropertyEvaluator evaluator;
        private UpdateHelper helper;
        private ReferenceHelper resolver;
        
        LibrariesNodeList(VisageProject proj) {
            project = proj;
            testSources = project.getTestSourceRoots();
            VisageLogicalViewProvider logView = (VisageLogicalViewProvider)project.getLookup().lookup(VisageLogicalViewProvider.class);
            assert logView != null;
            evaluator = logView.getEvaluator();
            helper = logView.getUpdateHelper();
            resolver = logView.getRefHelper();
        }
        
        public List<String> keys() {
            List<String> result = new ArrayList<String>();
            result.add(LIBRARIES);
//            URL[] testRoots = testSources.getRootURLs();
//            boolean addTestSources = false;
//            for (int i = 0; i < testRoots.length; i++) {
//                File f = new File(URI.create(testRoots[i].toExternalForm()));
//                if (f.exists()) {
//                    addTestSources = true;
//                    break;
//                }
//            }
//            if (addTestSources) {
//                result.add(TEST_LIBRARIES);
//            }
            return result;
        }

        public void addChangeListener(ChangeListener l) {
            changeSupport.addChangeListener(l);
        }

        public void removeChangeListener(ChangeListener l) {
            changeSupport.removeChangeListener(l);
        }

        public Node node(String key) {
            if (key == LIBRARIES) {
                //Libraries Node
                return  
                    new LibrariesNode(NbBundle.getMessage(VisageLogicalViewProvider.class,"CTL_LibrariesNode"), // NOI18N
                        project, evaluator, helper, resolver, VisageProjectProperties.RUN_CLASSPATH,
                        new String[] {VisageProjectProperties.BUILD_CLASSES_DIR},
                        "platform.active", // NOI18N
                        new Action[] {
                            LibrariesNode.createAddProjectAction(project, VisageProjectProperties.JAVAC_CLASSPATH),
                            LibrariesNode.createAddLibraryAction(project, VisageProjectProperties.JAVAC_CLASSPATH),
                            LibrariesNode.createAddFolderAction(project, VisageProjectProperties.JAVAC_CLASSPATH),
                            null,
                            new SourceNodeFactory.PreselectPropertiesAction(project, "Libraries", CustomizerLibraries.COMPILE), // NOI18N
                        }
                    );
            }
            return null;
            
        }

        public void addNotify() {
            testSources.addPropertyChangeListener(this);
        }

        public void removeNotify() {
            testSources.removePropertyChangeListener(this);
        }

        public void propertyChange(PropertyChangeEvent evt) {
            // The caller holds ProjectManager.mutex() read lock
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    changeSupport.fireChange();
                }
            });
        }
        
    }
    
}
