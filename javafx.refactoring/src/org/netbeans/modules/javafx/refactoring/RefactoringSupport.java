/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring;

import com.sun.source.util.TreePath;
import java.io.IOException;
import java.lang.reflect.Method;
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
            case METHOD: {
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

                        Method m = tph.getClass().getMethod("resolveElement", org.netbeans.api.java.source.CompilationInfo.class); // NOI18N
                        Object e = m.invoke(tph, cc);
                        Class elementClass = e.getClass().getClassLoader().loadClass("javax.lang.model.element.Element"); // NOI18N
                        Class elementsClass = e.getClass().getClassLoader().loadClass("javax.lang.model.util.Elements"); // NOI18N
                        Class packageElementClass = e.getClass().getClassLoader().loadClass("javax.lang.model.element.PackageElement"); // NOI18N

                        m = org.netbeans.api.java.source.ElementHandle.class.getMethod("create", elementClass); // NOI18N
                        Method nMethod = elementClass.getMethod("getSimpleName"); // NOI18N
                        try {
                            org.netbeans.api.java.source.ElementHandle jeh = (org.netbeans.api.java.source.ElementHandle)m.invoke(org.netbeans.api.java.source.ElementHandle.class, e);
                            Object elements = cc.getClass().getMethod("getElements").invoke(cc); // NOI18N
                            Method pMethod = elementsClass.getMethod("getPackageOf", elementClass); // NOI18N
                            Object pe = pMethod.invoke(elements, e);
                            String pName = packageElementClass.getMethod("getQualifiedName").invoke(pe).toString(); // NOI18N

                            ElementHandle eh = ElementHandle.fromJava(jeh);
                            if (eh != null) {
                                String simpleName = nMethod.invoke(e).toString();
                                refdef[0] = new GlobalDef(
                                    simpleName,
                                    eh.getKind(),
                                    pName,
                                    -1, -1, -1, -1,
                                    RefactoringSupport.getRefId(eh),
                                    null
                                );
                            }
                        } catch (Throwable ex) {
                            // basically ignore
                            ex.printStackTrace();
                        }
                    }
                }, false);
            } catch (IOException e) {
            }
        }
        return refdef[0];
    }

    public static TreePathHandle toJava(ElementHandle eh, FileObject javaFile) {
        final TreePathHandle[] tph = new TreePathHandle[1];

        final org.netbeans.api.java.source.ElementHandle ehj = eh.toJava();

        JavaSource js = JavaSource.forFileObject(javaFile);
        try {
            js.runUserActionTask(new Task<CompilationController>() {

                public void run(CompilationController cc) throws Exception {
                    Object e = ehj.getClass().getMethod("resolve", CompilationInfo.class).invoke(ehj, cc); // NOI18N
                    Class elementClass = e.getClass().getClassLoader().loadClass("javax.lang.model.element.Element"); // NOI18N
                    tph[0] = (TreePathHandle) TreePathHandle.class.getMethod("create", elementClass, CompilationInfo.class).invoke(null, elementClass.cast(e), cc);
                }
            }, false);
        } catch (IOException e) {
        }

        return tph[0];
    }
}
