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
package org.netbeans.modules.visage.platform.wizard;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import javax.swing.event.ChangeListener;
import org.netbeans.api.java.platform.JavaPlatform;
import org.netbeans.modules.visage.platform.platformdefinition.PlatformConvertor;
import org.openide.ErrorManager;
import org.openide.WizardDescriptor;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.Repository;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;
import org.openide.util.ChangeSupport;
import org.openide.util.NbBundle;

/**
 * Wizard Iterator for standard Visage platforms. It assumes that there is a
 * 'bin{/}java[.exe]' underneath the platform's directory, which can be run to
 * produce the target platform's VM environment.
 *
 * @author Svata Dedic, Tomas Zezula
 */
public class VisageWizardIterator implements WizardDescriptor.InstantiatingIterator<WizardDescriptor> {

    private static VisageWizardIterator instance;
    File installFolder, fxFolder;
    String platformName;
    DetectPanel.WizardPanel detectPanel;
    final ChangeSupport listeners = new ChangeSupport(this);
    WizardDescriptor wizard;

    public static final VisageWizardIterator getDefault() {
        if (instance == null) {
            instance = new VisageWizardIterator();
        }
        return instance;
    }

    public VisageWizardIterator() {
    }

    public void addChangeListener(ChangeListener l) {
        listeners.addChangeListener(l);
    }

    public WizardDescriptor.Panel<WizardDescriptor> current() {
        return this.detectPanel;
    }

    public boolean hasNext() {
        return false;
    }

    public boolean hasPrevious() {
        return false;
    }

    public void initialize(WizardDescriptor wiz) {
        this.wizard = wiz;
        this.detectPanel = new DetectPanel.WizardPanel(this);
    }

    /**
     * This finally produces the java platform's XML that represents the basic
     * platform's properties. The XML is returned in the resulting Set.
     * @return singleton Set with java platform's instance DO inside.
     */
    public java.util.Set instantiate() throws IOException {
        Set<JavaPlatform> result = new HashSet<JavaPlatform>();
        NewVisagePlatform platform = NewVisagePlatform.create(platformName, installFolder, fxFolder);
        platform.run();
        if (platform.isValid()) {
            final String systemName = platform.getAntName();
            FileObject platformsFolder = Repository.getDefault().getDefaultFileSystem().findResource(
                    "Services/Platforms/org-netbeans-api-java-Platform"); //NOI18N
            if (platformsFolder.getFileObject(systemName, "xml") != null) {   //NOI18N
                String msg = NbBundle.getMessage(VisageWizardIterator.class, "ERROR_InvalidName"); // NOI18N
                throw (IllegalStateException) ErrorManager.getDefault().annotate(
                        new IllegalStateException(msg), ErrorManager.USER, null, msg, null, null);
            }
            DataObject dobj = PlatformConvertor.create(platform, DataFolder.findFolder(platformsFolder), systemName);
            result.add(dobj.getNodeDelegate().getLookup().lookup(JavaPlatform.class));
        }
        return Collections.unmodifiableSet(result);
    }

    public String name() {
        return NbBundle.getMessage(VisageWizardIterator.class, "TITLE_PlatformName"); // NOI18N
    }

    public void nextPanel() {
    }

    public void previousPanel() {
    }

    public void removeChangeListener(ChangeListener l) {
        listeners.removeChangeListener(l);
    }

    public void uninitialize(WizardDescriptor wiz) {
        this.wizard = null;
        this.detectPanel = null;
    }
}
