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

package org.netbeans.modules.visage.source.parsing;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.netbeans.api.visage.source.CancellableTask;
import org.netbeans.api.visage.source.CompilationInfo;
import org.netbeans.api.visage.source.VisageSource;
import org.netbeans.modules.parsing.api.Source;
import org.netbeans.modules.parsing.impl.Utilities;
import org.netbeans.modules.parsing.spi.ParserResultTask;

public final class VisageTaskProcessor {

    private static Map<CancellableTask<CompilationInfo>,ParserResultTask<?>> tasks =
            new HashMap<CancellableTask<CompilationInfo>,ParserResultTask<?>>();

    public static void addPhaseCompletionTask(VisageSource javaFXSource, CancellableTask<CompilationInfo> task,
            VisageSource.Phase phase, VisageSource.Priority priority) throws IOException
    {
        final Collection<Source> sources = javaFXSource.sources();
        assert sources.size() == 1;
        int pp = translatePriority(priority);
        assert !tasks.keySet().contains(task);
        final ParserResultTask<?> cTask = new VisageSourceCancelableTask(javaFXSource, task, phase.toCompilationPhase(), pp);
        tasks.put(task, cTask);
        Utilities.addParserResultTask(cTask, sources.iterator().next());

    }

    public static void removePhaseCompletionTask(VisageSource javaFXSource, CancellableTask<CompilationInfo> task) {
        final Collection<Source> sources = javaFXSource.sources();
        assert sources.size() == 1;
        final ParserResultTask<?> cTask = tasks.remove(task);
        assert cTask != null;
        Utilities.removeParserResultTask(cTask, sources.iterator().next());
    }

    public static void rescheduleTask (VisageSource javaFXSource, CancellableTask<CompilationInfo> task) {
        final Collection<Source> sources = javaFXSource.sources();
        assert sources.size() == 1;
        final ParserResultTask<?> cTask = tasks.get(task);
        if (cTask != null)
            Utilities.rescheduleTask(cTask, sources.iterator().next());
    }

    private static int translatePriority (VisageSource.Priority priority) {
        assert priority != null;
        int tmp;
        if (priority == VisageSource.Priority.MAX) {
            tmp = 0;
        }
        else if (priority == VisageSource.Priority.MIN) {
            tmp = Integer.MAX_VALUE;
        }
        else {
            tmp = priority.ordinal() * 100;
        }
        return tmp;
    }

}
