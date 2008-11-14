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

package org.netbeans.api.javafx.source;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import org.netbeans.api.javafx.source.ClassIndex.Impl;
import org.netbeans.api.javafx.source.JavaFXSource.Phase;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.URLMapper;
import org.openide.util.Exceptions;
import org.openide.util.RequestProcessor;

/**
 *
 * @author nenik
 */
class ClassIndexManager {
    private static final Map<URL, ClassIndex.Impl> instances = new HashMap<URL, ClassIndex.Impl> ();
    private static boolean invalid;
    private static RequestProcessor PROCESSOR = new RequestProcessor("JavaFX class index manager");

    public static synchronized ClassIndex.Impl getUsagesQuery (final URL root) {
        assert root != null;
        if (invalid) {
            return null;
        }        
        return instances.get (root);
    }
    
    public static synchronized ClassIndex.Impl createUsagesQuery (final URL root, final boolean source) throws IOException {
        assert root != null;
        if (invalid) {
            return null;
        }        
        ClassIndex.Impl qi = instances.get (root);
        if (qi == null) {  
            qi = new ClassIndex.Impl();
            instances.put(root, qi);

            // fill in asynchronously
            final ClassIndex.Impl target = qi;
            PROCESSOR.post(new Runnable() {
                public void run() {
                    fillIn(target, root, source);
                }
            });
        }
        return qi;
    }

    private static void fillIn(Impl target, URL root, boolean source) {
        if (!source) return; // covered by the java index anyway
        FileObject rootFO = URLMapper.findFileObject(root);
        if (rootFO == null) return;

        Set<ClassIndex.TypeHolder> types = new HashSet<ClassIndex.TypeHolder>();
        Set<String> packages = new HashSet<String>();
        collect(rootFO, types, packages, "");
        target.setTypes(types);
    }

    private static void collect(FileObject dir, final Set<ClassIndex.TypeHolder> types, Set<String> packages, String prefix) {
        for (FileObject act : dir.getChildren()) {
            if (act.isData() && act.hasExt("fx")) {
                try {
                    JavaFXSource.forFileObject(act).runUserActionTask(new CancellableTask<CompilationController>() {

                        public void cancel() {// ignore
                        }

                        public void run(CompilationController cc) throws Exception {
                            cc.moveToPhase(Phase.ANALYZED);
                            Element rootElement = getRootElement(cc);
                            if (rootElement != null) collect(rootElement, types);
                        }
                        
                        private void collect(Element elem, Set<ClassIndex.TypeHolder> types) {
                            if (elem.getKind() != ElementKind.CLASS) return;

                            //elem2typeHolder:
                            ElementHandle eh = ElementHandle.create(elem);
                            String sig = eh.getSignatures()[0];
                            int lastDot = sig.lastIndexOf('.');
                            int lastDollar = sig.lastIndexOf('$');
                            if (lastDollar > lastDot) lastDot = lastDollar;
                            String prefix = (lastDot >= 0) ? sig.substring(0, lastDot+1) : "";
                            String name = (lastDot >= 0) ? sig.substring(lastDot+1) : sig;
                            types.add(new ClassIndex.TypeHolder(prefix, name));

                            // traverse children
                            for (Element sub : elem.getEnclosedElements()) collect(sub, types);
                        }
                        
                        private Element getRootElement(final CompilationInfo info) {
                            return new JavaFXTreePathScanner<Element,Void>() {
                                public @Override Element visitClassDeclaration(ClassDeclarationTree arg0, Void arg1) {
                                    return info.getTrees().getElement(getCurrentPath());
                                }

                            }.scan(info.getCompilationUnit(), null);
                        }
                    }, true);
                } catch (IOException ex) {
                    Exceptions.printStackTrace(ex);
                }
            } else if (act.isFolder()) {
                String pkgName = prefix + act.getName();
                packages.add(pkgName);
                collect(act, types, packages, pkgName + ".");
            }
        }
    }
    
}
