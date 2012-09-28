/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Oracle and/or its affiliates. All rights reserved.
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
 *
 * Contributor(s):
 *
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.modules.visage.sdksamples;

import java.awt.Component;
import java.util.HashSet;
import java.util.Set;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.openide.WizardDescriptor;
import org.openide.WizardValidationException;
import org.openide.filesystems.FileObject;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;

/**
 *
 * @author Michal Skvor
 */
public class SDKSamplesWizardPanel implements WizardDescriptor.Panel, WizardDescriptor.ValidatingPanel, WizardDescriptor.FinishablePanel  {

    private WizardDescriptor wizardDescriptor;
    private SDKSamplesVisualPanel component;

    private FileObject file;

    public SDKSamplesWizardPanel( FileObject file ) {
        this.file = file;
    }

    public Component getComponent() {
        if( component == null ) {
            component = new SDKSamplesVisualPanel( this );
            component.setName( NbBundle.getMessage( SDKSamplesWizardPanel.class, "LBL_CreateProjectStep" )); // NOI18N
        }
        return component;
    }

    FileObject getFile() {
        return file;
    }

    public HelpCtx getHelp() {
        return new HelpCtx( SDKSamplesWizardPanel.class );
    }

    public boolean isValid() {
        getComponent();
        return component.isValid( wizardDescriptor );
    }

    private final Set<ChangeListener> listeners = new HashSet<ChangeListener>(1); // or can use ChangeSupport in NB 6.0

    public void addChangeListener(ChangeListener l) {
        synchronized( listeners ) {
            listeners.add( l );
        }
    }

    public void removeChangeListener(ChangeListener l) {
        synchronized (listeners) {
            listeners.remove(l);
        }
    }

    protected final void fireChangeEvent() {
        Set<ChangeListener> ls;
        synchronized (listeners) {
            ls = new HashSet<ChangeListener>( listeners );
        }
        ChangeEvent ev = new ChangeEvent( this );
        for( ChangeListener l : ls ) {
            l.stateChanged( ev );
        }
    }

    public void readSettings(Object settings) {
        if (wizardDescriptor == null){
            ((WizardDescriptor)settings).putProperty("name", null); // NOI18N
        }
        wizardDescriptor = (WizardDescriptor) settings;
        component.load( wizardDescriptor );
    }

    public void storeSettings(Object settings) {
        WizardDescriptor d = (WizardDescriptor) settings;
        component.store( d );
    }

    public void validate() throws WizardValidationException {
        getComponent();
        component.isValid( wizardDescriptor );
    }

    public boolean isFinishPanel() {
        return true;
    }

}