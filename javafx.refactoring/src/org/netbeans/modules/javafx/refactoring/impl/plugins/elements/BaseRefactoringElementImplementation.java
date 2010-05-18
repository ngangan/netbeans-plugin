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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
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
package org.netbeans.modules.javafx.refactoring.impl.plugins.elements;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.text.Position.Bias;
import org.netbeans.modules.javafx.refactoring.transformations.Transformation;
import org.netbeans.modules.javafx.refactoring.transformations.Transformer;
import org.netbeans.modules.refactoring.api.RefactoringSession;
import org.netbeans.modules.refactoring.spi.BackupFacility;
import org.netbeans.modules.refactoring.spi.SimpleRefactoringElementImplementation;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.text.DataEditorSupport;
import org.openide.text.PositionBounds;
import org.openide.util.Lookup;
import org.openide.util.lookup.Lookups;

/**
 *
 * @author Jaroslav Bachorik <jaroslav.bachorik@sun.com>
 */
abstract public class BaseRefactoringElementImplementation extends SimpleRefactoringElementImplementation {
    final private static Logger LOGGER = Logger.getLogger(BaseRefactoringElementImplementation.class.getName());

    private FileObject srcFO;
    private RefactoringSession session;
    private PositionBounds pb;
    final private boolean shouldBackup;

    final private Set<Transformation> transformations = new HashSet<Transformation>();

    public BaseRefactoringElementImplementation(FileObject srcFO, RefactoringSession session) {
        this(srcFO, session, true);
    }
    
    public BaseRefactoringElementImplementation(FileObject srcFO, RefactoringSession session, boolean shouldBackup) {
        this.srcFO = srcFO;
        this.session = session;
        this.shouldBackup = shouldBackup;
        if (srcFO != null) {
            try {
                DataObject dobj = DataObject.find(srcFO);
                DataEditorSupport des = (DataEditorSupport) dobj.getCookie(EditorCookie.class);
                pb = new PositionBounds(des.createPositionRef(0, Bias.Forward), des.createPositionRef(des.openDocument().getLength(), Bias.Forward));
            } catch (IOException ex) {
                LOGGER.log(Level.SEVERE, null, ex);
                pb = null;
            }
        }

    }

    public Lookup getLookup() {
        return Lookups.fixed(srcFO);
    }

    public FileObject getParentFile() {
        return srcFO;
    }

    public PositionBounds getPosition() {
        return pb;
    }

    public String getText() {
        return getDisplayText();
    }

    public String getDisplayText() {
        return getRefactoringText();
    }

    private BackupFacility.Handle backupHandle = null;
    final private AtomicBoolean performed = new AtomicBoolean(false);

    final public void performChange() {
        if (performed.compareAndSet(false, true)) {
            FileObject targetFO = getTargetFO();
            if (targetFO != null) {
                try {
                    backupHandle = shouldBackup ? BackupFacility.getDefault().backup(targetFO) : null;
                    final Transformer t = Transformer.forFileObject(targetFO, session);
                    if (t != null) {
                        t.transform(getTransformations());
                    }
                } catch (IOException e) {
                    LOGGER.log(Level.SEVERE, null, e);
                }
            }
        }
    }

    public boolean hasChanges() {
        return getTransformations().size() > 0;
    }

    @Override
    protected String getNewFileContent() {
        try {
            StringBuilder content = new StringBuilder(srcFO.asText().replace("\r\n", "\n")); // NOI18N

            final Transformer t = Transformer.forText(content);
            if (t != null) {
                t.transform(getTransformations());
                return content.toString();
            }
        } catch (IOException e) {
        }
        return ""; // NOI18N
    }

    protected FileObject getTargetFO() {
        return srcFO;
    }

    @Override
    final public void undoChange() {
        try {
            if (backupHandle != null) {
                backupHandle.restore();
            }
        } catch (IOException ex) {
            LOGGER.log(Level.SEVERE, null, ex);
        }
    }

    private List<Transformation> getTransformations() {
        synchronized(transformations) {
            if (transformations.isEmpty()) {
                transformations.addAll(prepareTransformations(srcFO));
            }
            return new ArrayList<Transformation>(transformations);
        }
    }

    abstract protected Set<Transformation> prepareTransformations(FileObject fo);
    abstract protected String getRefactoringText();
}
