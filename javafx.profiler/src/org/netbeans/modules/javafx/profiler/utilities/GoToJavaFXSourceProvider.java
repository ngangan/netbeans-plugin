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

package org.netbeans.modules.javafx.profiler.utilities;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicBoolean;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.project.Project;
import org.netbeans.modules.profiler.spi.GoToSourceProvider;
import javax.lang.model.element.ElementKind;
import org.openide.filesystems.FileObject;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.project.JavaFXProject;
import org.netbeans.lib.profiler.ProfilerLogger;
import org.netbeans.api.javafx.editor.ElementOpen;
import org.netbeans.api.javafx.source.ClasspathInfo.PathKind;
import org.netbeans.api.javafx.source.JavaFXSource.Phase;
import org.openide.util.lookup.ServiceProvider;

@ServiceProvider(service=GoToSourceProvider.class)
public class GoToJavaFXSourceProvider extends GoToSourceProvider {
    // field to indicate whether source find was successfull
    final AtomicBoolean result = new AtomicBoolean(false);

    @Override
    public boolean openSource(final Project project, final String className, final String methodName, String signature, int line) {
        if (!(project instanceof JavaFXProject))
            return false;

        final CountDownLatch latch = new CountDownLatch(1);

        JavaFXSource source = JavaFXProjectUtilities.getSources((JavaFXProject)project);
        final FileObject fo = source.getCpInfo().getClassPath(PathKind.SOURCE).findResource(getFXFileName(className));

        if (fo == null) return false; // can go only to symbols from sources

        // cut interface suffix out of the signature
        final String sig = JavaFXProjectUtilities.cutIntfSuffix(signature);
        final JavaFXSource js = JavaFXSource.forFileObject(fo);

        try {
            js.runUserActionTask(new Task<CompilationController>() {

                public void run(CompilationController controller) throws Exception {
                    controller.moveToPhase(Phase.ANALYZED);
                    ElementHandle eh = new ElementHandle(ElementKind.METHOD, new String[] {className, methodName, sig});
                    result.set(ElementOpen.open(fo, eh));
                    latch.countDown();
                }
            }, true);
            latch.await();
        } catch (InterruptedException e)  {
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            ProfilerLogger.log(e);
            result.set(false);
        }

        return result.get();
    }

    private static String getFXFileName(String className) {
        String classNameIntern = className.replace('.', '/');
        int innerIndex = classNameIntern.indexOf("$");
        if (innerIndex > -1) {
            classNameIntern = classNameIntern.substring(0, innerIndex);
        }
        return classNameIntern.concat(".fx");
    }
}
