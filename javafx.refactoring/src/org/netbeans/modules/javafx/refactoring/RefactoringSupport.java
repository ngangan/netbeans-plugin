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

package org.netbeans.modules.javafx.refactoring;

import java.io.IOException;
import java.lang.reflect.Method;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import org.netbeans.api.fileinfo.NonRecursiveFolder;
import org.netbeans.api.java.source.CompilationController;
import org.netbeans.api.java.source.CompilationInfo;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.Task;
import org.netbeans.api.java.source.TreePathHandle;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.refactoring.repository.ClassModelFactory;
import org.netbeans.modules.javafx.refactoring.repository.ElementDef;
import org.netbeans.modules.javafx.refactoring.repository.GlobalDef;
import org.netbeans.modules.refactoring.api.AbstractRefactoring;
import org.openide.filesystems.FileObject;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
final public class RefactoringSupport {
    final private static Logger LOGGER = Logger.getLogger(RefactoringSupport.class.getName());
    final private static boolean DEBUG = LOGGER.isLoggable(Level.FINE);
    
    public static ClassModelFactory classModelFactory(AbstractRefactoring refactoring) {
        ClassModelFactory cmf = refactoring.getContext().lookup(ClassModelFactory.class);
        if (cmf == null) {
            cmf = new ClassModelFactory();
            refactoring.getContext().add(cmf);
        }
        return cmf;
    }

    public static ClassIndex classIndex(AbstractRefactoring refactoring) {
        ClassIndex ci = refactoring.getContext().lookup(ClassIndex.class);
        if (ci == null) {
            ClasspathInfo cpInfo = classpathInfo(refactoring);
            if (cpInfo != null) {
                ci = cpInfo.getClassIndex();
                refactoring.getContext().add(ci);
            }
        }
        return ci;
    }

    public static ClasspathInfo classpathInfo(AbstractRefactoring refactoring) {
        ClasspathInfo cpInfo = refactoring.getContext().lookup(ClasspathInfo.class);
        if (cpInfo == null) {
            FileObject src = refactoring.getRefactoringSource().lookup(FileObject.class);
            if (src == null) {
                TreePathHandle tph = refactoring.getRefactoringSource().lookup(TreePathHandle.class);
                if (tph != null) {
                    src = tph.getFileObject();
                }
            }
            if (src == null) {
                NonRecursiveFolder nrf = refactoring.getRefactoringSource().lookup(NonRecursiveFolder.class);
                if (nrf != null) {
                    src = nrf.getFolder();
                }
            }
            if (src == null) {
                src = refactoring.getContext().lookup(FileObject.class);
            }
            if (src != null) {
                cpInfo = ClasspathInfo.create(src);
                refactoring.getContext().add(cpInfo);
            }
        }
        return cpInfo;
    }

    public static String getRefId(ElementHandle<? extends Element> eeh) {
        switch (eeh.getKind()) {
            case PACKAGE:
            case CLASS:
            case INTERFACE:
            case ENUM: {
                return eeh.getSignatures()[0];
            }
            case FIELD:
            case LOCAL_VARIABLE:
            case PARAMETER:
            case METHOD:
            case ENUM_CONSTANT: {
                return eeh.getSignatures()[0] + "#" + eeh.getSignatures()[1] + "#" + eeh.getSignatures()[2];
            }
            default: {
                return eeh.toString();
            }
        }
    }

    static {
        // preload the classes clashing with the java.source version of javac
        try {
            Class.forName("javax.lang.model.element.Element"); //NOI18N
            Class.forName("javax.lang.model.util.Elements"); // NOI18N
            Class.forName("javax.lang.model.element.PackageElement"); // NOI18N
        } catch (ClassNotFoundException e) {
        }
    }

    public static ElementDef fromJava(final TreePathHandle tph) {
        final ElementDef[] refdef = new ElementDef[]{null};
        if (tph != null) {
            JavaSource js = JavaSource.forFileObject(tph.getFileObject());
            try {
                js.runUserActionTask(new org.netbeans.api.java.source.Task<org.netbeans.api.java.source.CompilationController>() {

                    public void run(org.netbeans.api.java.source.CompilationController cc) throws Exception {
                        if (cc.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED) == JavaSource.Phase.ELEMENTS_RESOLVED) {
                            Method m = tph.getClass().getMethod("resolveElement", org.netbeans.api.java.source.CompilationInfo.class); // NOI18N
                            Object e = m.invoke(tph, cc);
                            refdef[0] = fromJava(e, cc);
                        }
                    }
                }, false);
            } catch (IOException e) {
            }
        }
        return refdef[0];
    }

    public static ElementDef fromJava(Object element, CompilationController cc) {
        ElementDef edef = null;
        try {
            if (cc.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED) == JavaSource.Phase.ELEMENTS_RESOLVED) {
                Class elementClass = element.getClass().getClassLoader().loadClass("javax.lang.model.element.Element"); // NOI18N
                Class elementsClass = element.getClass().getClassLoader().loadClass("javax.lang.model.util.Elements"); // NOI18N
                Class packageElementClass = element.getClass().getClassLoader().loadClass("javax.lang.model.element.PackageElement"); // NOI18N

                Method m = org.netbeans.api.java.source.ElementHandle.class.getMethod("create", elementClass); // NOI18N
                Method nMethod = elementClass.getMethod("getSimpleName"); // NOI18N
                org.netbeans.api.java.source.ElementHandle jeh = (org.netbeans.api.java.source.ElementHandle)m.invoke(org.netbeans.api.java.source.ElementHandle.class, element);
                Object elements = cc.getClass().getMethod("getElements").invoke(cc); // NOI18N
                Method pMethod = elementsClass.getMethod("getPackageOf", elementClass); // NOI18N
                Object pe = pMethod.invoke(elements, element);
                String pName = packageElementClass.getMethod("getQualifiedName").invoke(pe).toString(); // NOI18N

                ElementHandle eh = ElementHandle.fromJava(jeh);
                if (eh != null) {
                    String simpleName = nMethod.invoke(element).toString();
                    edef = new GlobalDef(
                        simpleName,
                        eh.getKind(),
                        pName,
                        -1, -1, -1, -1,
                        RefactoringSupport.getRefId(eh),
                        null
                    );
                }
            }
        } catch (Throwable ex) {
            // basically ignore
            ex.printStackTrace();
        }
        return edef;
    }

    public static TreePathHandle toJava(ElementHandle eh, FileObject javaFile) {
        final TreePathHandle[] tph = new TreePathHandle[1];

        final org.netbeans.api.java.source.ElementHandle ehj = eh.toJava();

        JavaSource js = JavaSource.forFileObject(javaFile);
        if (js != null) {
            try {
                js.runUserActionTask(new Task<CompilationController>() {

                    public void run(CompilationController cc) throws Exception {
                        if (cc.toPhase(JavaSource.Phase.ELEMENTS_RESOLVED) == JavaSource.Phase.ELEMENTS_RESOLVED) {
                            Object e = ehj.getClass().getMethod("resolve", CompilationInfo.class).invoke(ehj, cc); // NOI18N
                            Class elementClass = e.getClass().getClassLoader().loadClass("javax.lang.model.element.Element"); // NOI18N
                            tph[0] = (TreePathHandle) TreePathHandle.class.getMethod("create", elementClass, CompilationInfo.class).invoke(null, elementClass.cast(e), cc);
                        }
                    }
                }, false);
            } catch (IOException e) {
            }
        } else {
            if (DEBUG) {
                LOGGER.log(Level.FINE, "Can not obtain JavaSource for {0}", javaFile.getPath());
            }
        }

        return tph[0];
    }
}
