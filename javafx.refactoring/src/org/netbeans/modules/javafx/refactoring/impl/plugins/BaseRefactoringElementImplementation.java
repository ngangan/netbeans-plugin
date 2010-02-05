/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Sun Microsystems, Inc. All rights reserved.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
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
package org.netbeans.modules.javafx.refactoring.impl.plugins;

import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.modules.javafx.refactoring.transformations.Transformation;
import org.netbeans.modules.javafx.refactoring.transformations.Transformer;
import org.netbeans.modules.refactoring.api.ProgressEvent;
import org.netbeans.modules.refactoring.api.ProgressListener;
import org.netbeans.modules.refactoring.api.RefactoringSession;
import org.netbeans.modules.refactoring.spi.SimpleRefactoringElementImplementation;
import org.openide.filesystems.FileChangeAdapter;
import org.openide.filesystems.FileChangeListener;
import org.openide.filesystems.FileEvent;
import org.openide.filesystems.FileObject;
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

    final private Set<Transformation> transformations = new HashSet<Transformation>();

    final private FileChangeListener fcl = new FileChangeAdapter() {

        @Override
        public void fileChanged(FileEvent fe) {
            JavaFXSource jfxs = JavaFXSource.forFileObject(fe.getFile());
            try {
                jfxs.runUserActionTask(new Task<CompilationController>() {

                    public void run(CompilationController cc) throws Exception {
                        synchronized(transformations) {
                            transformations.clear();
                            transformations.addAll(prepareTransformations(cc));
                        }
                    }
                }, true);
            } catch (IOException iOException) {
                LOGGER.log(Level.WARNING, null, iOException);
            }
        }
    };

    final private ProgressListener pl = new ProgressListener() {

        public void start(ProgressEvent pe) {
            srcFO.removeFileChangeListener(fcl);
        }

        public void step(ProgressEvent pe) {
            //
        }

        public void stop(ProgressEvent pe) {
            //
        }
    };

    public BaseRefactoringElementImplementation(FileObject srcFO, RefactoringSession session) {
        this.srcFO = srcFO;
        this.session = session;
        srcFO.addFileChangeListener(fcl);
        session.addProgressListener(pl);
    }

    public Lookup getLookup() {
        return Lookups.fixed(srcFO);
    }

    public FileObject getParentFile() {
        return srcFO;
    }

    public PositionBounds getPosition() {
        return null;
    }

    public String getText() {
        return getDisplayText();
    }

    final public void performChange() {
        FileObject targetFO = getTargetFO();
        if (targetFO != null) {
            final Transformer t = Transformer.forFileObject(targetFO, session);
            if (t != null) {
                t.addTransformations(getTransformations());
                t.transform();
            }
        }
    }

    final public boolean hasChanges() {
        return getTransformations().size() > 0;
    }

    @Override
    protected String getNewFileContent() {
        final Transformer t = Transformer.forFileObject(srcFO, session, false);
        if (t != null) {
            t.addTransformations(getTransformations());
            return t.preview();
        }
        return "";
    }

    final protected FileObject getSourceFO() {
        return srcFO;
    }

    protected FileObject getTargetFO() {
        return srcFO;
    }

    @Override
    final public void undoChange() {
        FileObject targetFO = getTargetFO();
        if (targetFO != null) {
            final Transformer t = Transformer.forFileObject(targetFO, session);
            if (t != null) {
//                t.addTransformations(getTransformations());
                t.revert();
            }
        }
    }

    private Set<Transformation> getTransformations() {
        synchronized(transformations) {
            if (transformations.isEmpty()) {
                JavaFXSource jfxs = JavaFXSource.forFileObject(srcFO);
                try {
                    jfxs.runUserActionTask(new Task<CompilationController>() {

                        public void run(CompilationController cc) throws Exception {
                            transformations.addAll(prepareTransformations(cc));
                        }
                    }, true);
                } catch (IOException iOException) {
                    LOGGER.log(Level.WARNING, null, iOException);
                }
            }
            return Collections.unmodifiableSet(transformations);
        }
    }

    abstract protected Set<Transformation> prepareTransformations(CompilationController cc);
}
