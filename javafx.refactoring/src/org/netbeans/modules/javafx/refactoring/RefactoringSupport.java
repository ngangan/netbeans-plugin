/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring;

import javax.lang.model.element.Element;
import org.netbeans.api.fileinfo.NonRecursiveFolder;
import org.netbeans.api.java.source.TreePathHandle;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.refactoring.repository.ClassModelFactory;
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
}
