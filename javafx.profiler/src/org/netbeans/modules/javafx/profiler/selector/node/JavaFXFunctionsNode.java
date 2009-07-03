/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
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

package org.netbeans.modules.javafx.profiler.selector.node;

import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Vector;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import org.netbeans.modules.javafx.profiler.utilities.JavaFXProjectUtilities;
import org.netbeans.modules.profiler.selector.spi.nodes.ContainerNode;
import org.netbeans.modules.profiler.selector.spi.nodes.SelectorNode;
import org.netbeans.modules.profiler.selector.spi.nodes.SelectorChildren;
import org.netbeans.modules.profiler.selector.spi.nodes.GreedySelectorChildren;
import org.netbeans.modules.profiler.selector.spi.nodes.IconResource;
import org.openide.filesystems.FileObject;
import org.openide.util.NbBundle;

/**
 *
 * @author cms
 */
public class JavaFXFunctionsNode extends ContainerNode {    
    
    private static class Children extends GreedySelectorChildren<JavaFXFunctionsNode> {

        protected List<?extends SelectorNode> prepareChildren(final JavaFXFunctionsNode parent) {
            final List<JavaFXFunctionNode> functionNodes = new ArrayList<JavaFXFunctionNode>();

            try {
                FileObject fo = JavaFXProjectUtilities.getFile(classElement, parent.cpInfo);
                JavaFXSource source = JavaFXSource.forFileObject(JavaFXProjectUtilities.getFile(classElement, parent.cpInfo));

//                final FileObject fo = source.getCpInfo().getClassPath(ClasspathInfo.PathKind.SOURCE).findResource(getFXFileName(className));

                // workaround for library class nodes
                if (source == null) {
                    Vector<FileObject> v = new Vector<FileObject>();
                    v.add(JavaFXProjectUtilities.getFile(classElement, parent.cpInfo));
                    source = JavaFXSource.create(parent.cpInfo, v);
                }

                final JavaFXSource js = source;
                source.runUserActionTask(new CancellableTask<CompilationController>() {
                        public void cancel() {
                        }

                        public void run(CompilationController controller)
                                 throws Exception {
                            if (JavaFXSource.Phase.ANALYZED.compareTo(controller.toPhase(JavaFXSource.Phase.ANALYZED))<=0) {
                                classElement = parent.getClassHandle().resolve(controller);
                                List<? extends Element> methods = controller.getElements().getAllMembers((TypeElement)classElement);
                                for (int k = 0; k < methods.size(); k++){
                                    Element tek = methods.get(k);
                                    if (classElement.equals(tek.getEnclosingElement()) && 
                                            ((tek.getKind() == ElementKind.METHOD) ||
                                            (tek.getKind() == ElementKind.CONSTRUCTOR) ||
                                            (tek.getKind() == ElementKind.STATIC_INIT))) {
                                        ((ExecutableElement)tek).getReturnType();
                                        
                                        JavaFXFunctionNode functionNode = new JavaFXFunctionNode(js.getCpInfo(), tek, parent);
                                        if (functionNode.getSignature() != null) {
                                            functionNodes.add(functionNode);
                                        }
                                    }
                                }
                            }
                        }
                    }, true);
                Collections.sort(functionNodes, JavaFXFunctionNode.COMPARATOR);
            } catch (IllegalArgumentException ex) {
                ex.printStackTrace();
            } catch (IOException ex) {
                ex.printStackTrace();
            }

            return functionNodes;
        }
    }

    private final ClasspathInfo cpInfo;
    private static Element classElement;

    public JavaFXFunctionsNode(final ClasspathInfo cpInfo, final JavaFXClassNode parent) {
        super(NbBundle.getMessage(JavaFXFunctionsNode.class, "Functions_DisplayName"), IconResource.METHODS_ICON, parent); // NOI18N
        classElement = parent.getClassElement();
        this.cpInfo = cpInfo;
    }

    public ElementHandle<TypeElement> getClassHandle() {
        return ((JavaFXClassNode) getParent()).getClassHandle();
    }

    protected SelectorChildren getChildren() {
        return new Children();
    }
}
