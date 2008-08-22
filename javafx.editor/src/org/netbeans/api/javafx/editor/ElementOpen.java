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
package org.netbeans.api.javafx.editor;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree;
import com.sun.tools.javac.code.Symbol;
import org.netbeans.modules.javafx.editor.*;
import java.io.IOException;
import java.lang.reflect.Constructor;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.queries.SourceForBinaryQuery;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.JavaFXSource.Phase;
import org.netbeans.api.javafx.source.JavaFXSourceUtils;
import org.netbeans.api.javafx.source.Task;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;

/**
 *
 * @author nenik
 */
public final class ElementOpen {

    private ElementOpen() {
    }

    public static void open(final FileObject srcFile, int offset) throws Exception {
        GoToSupport.doOpen(srcFile, (int) offset);
    }
    
    public static void open(final FileObject srcFile, final ElementHandle elh) throws Exception {
        if (srcFile == null) {
            return;
        }

        JavaFXSource js = JavaFXSource.forFileObject(srcFile);
        if (js == null) {
            return;
        }
        
        js.runUserActionTask(new Task<CompilationController>() {
            public void run(CompilationController controller) throws Exception {
                if (controller.toPhase(Phase.ANALYZED).lessThan(Phase.ANALYZED)) {
                    return; // convert the element to local element
                }
                Element el = elh.resolve(controller);
                if (el == null) {
                    return;
                }
                System.err.println("real=" + el);

                JavaFXTreePath elpath = controller.getPath(el);
                Tree tree = elpath != null ? elpath.getLeaf() : null;

                if (tree != null) {
                    long startPos = controller.getTrees().getSourcePositions().getStartPosition(controller.getCompilationUnit(), tree);

                    if (startPos != -1l) {
                        GoToSupport.doOpen(srcFile, (int) startPos);
                    }
                }
            }
        }, true);
        
    }
    

    /**
     * Opens the source file containing given {@link Element} and seek to
     * the declaration.
     * 
     * @param cpInfo ClasspathInfo which should be used for the search
     * @param el     declaration to open
     */
    public static void open(final FileObject srcFile, final Element el) throws Exception {
        ElementHandle elh = ElementHandle.create(el);
        open(srcFile, elh);
    }
    
    public static void open(final CompilationInfo comp, final Element el) throws Exception {
        TypeElement tel = getEnclosingClassElement(el);
        ElementHandle elh = ElementHandle.create(el);

        if (!comp.getJavafxTypes().isJFXClass((Symbol) tel)) { // java
            openThroughJavaSupport(comp.getJavaFXSource().getFileObject(), elh);
        }
        // Find the source file
        final FileObject srcFile = getFile(el, comp);
        open(srcFile, el);
    }


    private static TypeElement getEnclosingClassElement(Element el) {
        while (el != null && el.getKind() != ElementKind.CLASS) {
            el = el.getEnclosingElement();
        }
        if (el == null) {
            return null;
        }
        while (el.getEnclosingElement() != null && el.getEnclosingElement().getKind() == ElementKind.CLASS) {
            el = el.getEnclosingElement();
        }
        return (TypeElement) el; // or null
    }

    private static FileObject getFile(Element elem, final CompilationInfo comp) {
        FileObject ref = comp.getJavaFXSource().getFileObject();
        ClasspathInfo cpi = ClasspathInfo.create(ref);
        return JavaFXSourceUtils.getFile(elem, cpi);
    }
    
    // All of the following code is a hack to call through to the java
    // implementation of open support, where we need to pass "their"
    // implementation of Element/ElementHandle.
    // All of the Element serialization logic is copied and adapted from
    // javasource's ElementHandle and ClassFileUtils
    @SuppressWarnings("unchecked")
    private static void openThroughJavaSupport(FileObject reference, ElementHandle elh) throws Exception {
        org.netbeans.api.java.source.ClasspathInfo cpi =
                org.netbeans.api.java.source.ClasspathInfo.create(reference);
        // Load the right version of the ElementKind class and convert our instance to it
        Class ekClass = org.netbeans.api.java.source.ElementHandle.class.getClassLoader().loadClass("javax.lang.model.element.ElementKind");
        Object ekInstance = Enum.valueOf(ekClass, elh.getKind().name());

        String[] sig = elh.getSignatures();
        Class strArrClass = sig.getClass();

        Constructor ehCtor = org.netbeans.api.java.source.ElementHandle.class.getDeclaredConstructor(ekClass, strArrClass);
        ehCtor.setAccessible(true);
        org.netbeans.api.java.source.ElementHandle eh = (org.netbeans.api.java.source.ElementHandle) ehCtor.newInstance(ekInstance, sig);

        org.netbeans.api.java.source.ui.ElementOpen.open(cpi, eh);
    }

}
