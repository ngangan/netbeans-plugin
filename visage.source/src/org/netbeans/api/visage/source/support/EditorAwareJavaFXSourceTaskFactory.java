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
package org.netbeans.api.visage.source.support;

import java.util.List;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.netbeans.api.visage.source.VisageSource.Phase;
import org.netbeans.api.visage.source.VisageSource.Priority;
import org.netbeans.api.visage.source.VisageSourceTaskFactory;
//import org.netbeans.api.visage.source.SourceUtils;
import org.openide.filesystems.FileObject;

/**A {@link VisageSourceTaskFactorySupport} that registers tasks to all files that are
 * opened in the editor and are visible.
 *
 * @author Jan Lahoda
 */
public abstract class EditorAwareVisageSourceTaskFactory extends VisageSourceTaskFactory {
    
    private String[] supportedMimeTypes;
    
    /**Construct the EditorAwareJavaSourceTaskFactory with given {@link Phase} and {@link Priority}.
     *
     * @param phase phase to use for tasks created by {@link #createTask}
     * @param priority priority to use for tasks created by {@link #createTask}
     */
    protected EditorAwareVisageSourceTaskFactory(Phase phase, Priority priority) {
        this(phase, priority, (String[]) null);
    }
    
    /**Construct the EditorAwareJavaSourceTaskFactory with given {@link Phase} and {@link Priority}.
     *
     * @param phase phase to use for tasks created by {@link #createTask}
     * @param priority priority to use for tasks created by {@link #createTask}
     * @param supportedMimeTypes a list of mime types on which the tasks created by this factory should be run
     * @since 0.21
     */
    protected EditorAwareVisageSourceTaskFactory(Phase phase, Priority priority, String... supportedMimeTypes) {
        super(phase, priority);
        //XXX: weak, or something like this:
        OpenedEditors.getDefault().addChangeListener(new ChangeListenerImpl());
        this.supportedMimeTypes = supportedMimeTypes != null ? supportedMimeTypes.clone() : null;
    }
    
    /**@inheritDoc*/
    public List<FileObject> getFileObjects() {
        List<FileObject> files = OpenedEditors.filterSupportedMIMETypes(OpenedEditors.getDefault().getVisibleEditorsFiles(), supportedMimeTypes);

        return files;
    }

    private class ChangeListenerImpl implements ChangeListener {
        public void stateChanged(ChangeEvent e) {
            fileObjectsChanged();
        }
    }

}