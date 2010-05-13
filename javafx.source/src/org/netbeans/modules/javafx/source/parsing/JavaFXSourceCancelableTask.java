/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 2008 Sun Microsystems, Inc. All rights reserved.
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

package org.netbeans.modules.javafx.source.parsing;

import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.CompilationPhase;
import org.netbeans.api.javafx.source.JavaFXParserResult;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.modules.parsing.spi.ParserResultTask;
import org.netbeans.modules.parsing.spi.Scheduler;
import org.netbeans.modules.parsing.spi.SchedulerEvent;
import org.openide.util.Exceptions;

/**
 * JavaFX specific version of the {@link ParserResultTask}. In addition to the
 * {@link ParserResultTask} it adds a support for javafxc phases.
 */
public final class JavaFXSourceCancelableTask extends ParserResultTask<JavaFXParserResult> {

    private final JavaFXSource javaFXSource;
    private final CancellableTask<CompilationInfo> task;
    private final CompilationPhase phase;
    private final int priority;

    protected JavaFXSourceCancelableTask(JavaFXSource javaFXSource,
            CancellableTask<CompilationInfo> task,
            CompilationPhase phase, int priority)
    {
        this.javaFXSource = javaFXSource;
        this.task = task;
        this.phase = phase;
        this.priority = priority;
    }

    /**
     * Returns the phase needed by task.
     * @return the pahse
     */
    public final CompilationPhase getPhase () {
        return this.phase;
    }

    @Override
    public int getPriority() {
        return this.priority;
    }

    @Override
    public Class<? extends Scheduler> getSchedulerClass() {
        return null;
    }

    @Override
    public void cancel() {
        this.task.cancel();
    }

    @Override
    public void run(JavaFXParserResult result, SchedulerEvent event) {
        final CompilationController compilationController = CompilationController.create(result);
        assert compilationController != null;
        try {
            if (!result.toPhase(phase).lessThan(phase)) { // #186081: Sanity check
                this.task.run(compilationController);
            }
        } catch (Exception ex) {
            Exceptions.printStackTrace(ex);
        }
    }

    public ClasspathInfo classpathInfo () {
        return javaFXSource.getClasspathInfo();
    }

    @Override
    public String toString () {
        return this.getClass().getSimpleName()+"[task: "+ task +    //NOI18N
                ", phase: "+getPhase()+", priority: "+priority+"]";      //NOI18N
    }

}
