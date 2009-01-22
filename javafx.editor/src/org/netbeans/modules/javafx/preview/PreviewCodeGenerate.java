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

package org.netbeans.modules.javafx.preview;

import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.tools.JavaFileObject;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.ClassOutputBuffer;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.JavaFXSource.Phase;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.modules.javafx.editor.JavaFXDocument;
import org.netbeans.modules.javafx.preview.CodeUtils.Context;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStateInvalidException;
import org.openide.loaders.DataObject;
import org.openide.util.Exceptions;
import org.openide.util.RequestProcessor;

public class PreviewCodeGenerate implements CancellableTask<CompilationInfo> {
    
    private FileObject file;
    private AtomicBoolean cancel = new AtomicBoolean();

    public PreviewCodeGenerate(FileObject file) {
         this.file = file;
    }

    public void cancel() {
        cancel.set(true);
    }

    public void run(CompilationInfo info) throws Exception {
        cancel.set(false);
        process(info);
    }
    
    public static void process(final CompilationInfo info) throws ClassNotFoundException, Exception {
        final FileObject fo = info.getFileObject();
        DataObject od = DataObject.find(fo);
        EditorCookie ec = od.getCookie(EditorCookie.class);
        final JavaFXDocument doc = (JavaFXDocument) ec.openDocument();
        if (doc.executionAllowed()) {
            RequestProcessor.getDefault().post(new Runnable() {
                public void run() {
                    JavaFXSource javaFXSource = info.getJavaFXSource();
                    try {
                        javaFXSource.runUserActionTask(new Task<CompilationController>() {

                            public void run(CompilationController controller) throws Exception {
                                if (controller.toPhase(Phase.CODE_GENERATED).lessThan(Phase.CODE_GENERATED)) {
                                    Logger.getLogger(PreviewCodeGenerate.class.getName()).log(Level.SEVERE, "Can not generate code for preview!"); // NI18N
                                    return;
                                }
                                
                                if (info.getClassBytes() != null) {
                                    Map<String, byte[]> classBytes = new HashMap<String, byte[]>();
                                    for (JavaFileObject jfo : info.getClassBytes()) {
                                        byte[] cb = ((ClassOutputBuffer) jfo).getClassBytes();
                                        if (cb != null) {
                                            classBytes.put(((ClassOutputBuffer) jfo).getBinaryName(), cb);
                                        }
                                    }

                                    ClassPath sourceCP = ClassPath.getClassPath(fo, ClassPath.SOURCE);
                                    ClassPath compileCP = ClassPath.getClassPath(fo, ClassPath.COMPILE);
                                    ClassPath executeCP = ClassPath.getClassPath(fo, ClassPath.EXECUTE);
                                    ClassPath bootCP = ClassPath.getClassPath(fo, ClassPath.BOOT);

                                    String className = sourceCP.getResourceName(info.getFileObject(), '.', false);                              // NOI18N
                                    String fileName = fo.getNameExt();

                                    final PreviewSideServerFace previewSideServerFace = Bridge.getPreview(doc);
                                    if (previewSideServerFace != null) {
                                        previewSideServerFace.run(new Context(classBytes, className, fileName, toURLs(sourceCP), toURLs(executeCP), toURLs(bootCP)));
                                    }
                                }
                            }
                        }, true);
                    } catch (IOException ex) {
                        Exceptions.printStackTrace(ex);
                    }
                }
            });
        }
    }
    
    private static URL[] toURLs(ClassPath classPath) {
        URL urls[] = new URL[classPath.getRoots().length];
        for (int j = 0; j < classPath.getRoots().length; j++)
            try {
                urls[j] = classPath.getRoots()[j].getURL();
            } catch (FileStateInvalidException ex) {
                Exceptions.printStackTrace(ex);
            }
        return urls;
    }
}
