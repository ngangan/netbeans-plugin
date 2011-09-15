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
 * Portions Copyrighted 2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.editor;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.util.Exceptions;
import org.openide.util.NbBundle;
import org.openide.util.RequestProcessor;
import org.openide.windows.WindowManager;

/**
 *
 * @author Jan Lahoda
 */
public class RunOffAWT {

    public static void runOffAWT(final Runnable what, String featureName, final AtomicBoolean cancelOut) {
        if (!SwingUtilities.isEventDispatchThread()) {
            what.run();
            return; // running in a non-EDT
        }

        final CountDownLatch l = new CountDownLatch(1);
        final AtomicReference<Dialog> d = new AtomicReference<Dialog>();
        
        WORKER.post(new Runnable() {
            public void run() {
                if (cancelOut.get()) return ;
                
                what.run();

                l.countDown();

                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        Dialog dd = d.get();

                        if (dd != null) {
                            dd.setVisible(false);
                        }
                    }
                });
            }
        });

        Component glassPane = ((JFrame) WindowManager.getDefault().getMainWindow()).getGlassPane();
        
        if (waitMomentarily(glassPane, null, 50, l))
            return ;

        Cursor wait = org.openide.util.Utilities.createProgressCursor(glassPane);

        if (waitMomentarily(glassPane, wait, 1450, l))
            return ;

        String warning = NbBundle.getMessage(GoToSupport.class, "LBL_Long", featureName);
        String cancelButton = NbBundle.getMessage(GoToSupport.class, "BTN_Long_Cancel");

        DialogDescriptor nd = new DialogDescriptor(warning, featureName, true, new Object[] {cancelButton}, cancelButton, DialogDescriptor.DEFAULT_ALIGN, null, new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                cancelOut.set(true);
                d.get().setVisible(false);
            }
        });

        nd.setMessageType(NotifyDescriptor.INFORMATION_MESSAGE);

        d.set(DialogDisplayer.getDefault().createDialog(nd));
        d.get().setVisible(true);
    }

    private static boolean waitMomentarily(Component glassPane, Cursor wait, int timeout, final CountDownLatch l) {
        Cursor original = glassPane.getCursor();

        try {
            if (wait != null)
                glassPane.setCursor(wait);

            glassPane.setVisible(true);
            try {
                return l.await(timeout, TimeUnit.MILLISECONDS);
            } catch (InterruptedException ex) {
                Logger.getLogger(RunOffAWT.class.getName()).log(Level.FINE, null, ex);
                return true;
            }
        } finally {
            glassPane.setVisible(false);
            glassPane.setCursor(original);
        }
    }

    public static <T> T computeOffAWT(Worker<T> w, String featureName, final JavaFXSource source, JavaFXSource.Phase phase) {
        AtomicBoolean cancel = new AtomicBoolean();
        Compute<T> c = new Compute(cancel, source, phase, w);

        runOffAWT(c, featureName, cancel);

        return c.result;
    }

    private static final class Compute<T> implements Runnable, Task<CompilationController> {

        private final AtomicBoolean cancel;
        private final JavaFXSource source;
        private final JavaFXSource.Phase phase;
        private final Worker<T> worker;
        private       T result;

        public Compute(AtomicBoolean cancel, JavaFXSource source, JavaFXSource.Phase phase, Worker<T> worker) {
            this.cancel = cancel;
            this.source = source;
            this.phase = phase;
            this.worker = worker;
        }

        public void run() {
            try {
                source.runUserActionTask(this, true);
            } catch (IOException ex) {
                Exceptions.printStackTrace(ex);
                result = null;
            }
        }

        public void run(CompilationController parameter) throws Exception {
            if (cancel.get()) return ;

            parameter.toPhase(phase);

            if (cancel.get()) return ;

            T t = worker.process(parameter);

            if (cancel.get()) return ;

            result = t;
        }
        
    }
    private static final RequestProcessor WORKER = new RequestProcessor(RunOffAWT.class.getName());

    public static interface Worker<T> {
        T process(CompilationInfo info);
    }
}
