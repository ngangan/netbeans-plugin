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

package org.netbeans.modules.visage.profiler.selector.node;

import org.netbeans.api.visage.source.CancellableTask;
import org.netbeans.api.visage.source.ClasspathInfo;
import org.netbeans.api.visage.source.CompilationController;
import org.netbeans.api.visage.source.VisageSource;
import org.netbeans.lib.profiler.client.ClientUtils;
import java.io.IOException;
import java.util.Comparator;
import java.util.Vector;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.swing.Icon;
import org.openide.filesystems.FileObject;
import org.netbeans.modules.visage.profiler.utilities.VisageProjectUtilities;
import org.netbeans.modules.profiler.selector.spi.nodes.SelectorNode;
import org.netbeans.modules.profiler.selector.spi.nodes.SelectorChildren;
import org.netbeans.modules.profiler.selector.spi.nodes.IconResource;


/**
 *
 * @author cms
 */
public class VisageFunctionNode extends SelectorNode {
    public static final Comparator<VisageFunctionNode> COMPARATOR = new Comparator<VisageFunctionNode>() {
        public int compare(VisageFunctionNode o1, VisageFunctionNode o2) {
            return o1.toString().compareTo(o2.toString());
        }
    };

    private ClientUtils.SourceCodeSelection rootMethod;
    private String signature = "";   // NOI18N

    /** Creates a new instance of MethodNode */
    public VisageFunctionNode(ClasspathInfo cpInfo, final Element method, VisageFunctionsNode parent) {
        super(method.toString(), method.getSimpleName().toString(), getIcon(method), SelectorChildren.LEAF, parent);


        VisageSource js = VisageSource.forFileObject(VisageProjectUtilities.getFile(getEnclosingClass(method), cpInfo));

        // workaround for library class nodes
        if (js == null) {
            Vector<FileObject> v = new Vector<FileObject>();
            v.add(VisageProjectUtilities.getFile(getEnclosingClass(method), cpInfo));
            js = VisageSource.create(cpInfo, v);
        }
        
        try {
            js.runUserActionTask(new CancellableTask<CompilationController>() {
                    public void cancel() {
                    }

                    public void run(CompilationController controller)
                             throws Exception {
                        if (controller.toPhase(VisageSource.Phase.ANALYZED) == VisageSource.Phase.ANALYZED) {
                            if (method instanceof ExecutableElement)
                                signature = VisageProjectUtilities.getVMMethodSignature((ExecutableElement)method, controller);
                        } else {
                            System.err.println("Hups");
                        }
                    }
                }, true);
        } catch (IOException ex) {
            ex.printStackTrace();
        }

        if (signature != null) {
        rootMethod = new ClientUtils.SourceCodeSelection(VisageProjectUtilities.getBinaryName((Element)getEnclosingClass(method), (Element)getEnclosingClass(getEnclosingClass(method))),
                                                         method.getSimpleName().toString(), signature);
        }
    }

    @Override
    public boolean getAllowsChildren() {
        return false;
    }

    @Override
    public int getChildCount() {
        return 0;
    }

    @Override
    public boolean isLeaf() {
        return true;
    }

    @Override
    public ClientUtils.SourceCodeSelection getSignature() {
        return rootMethod;
    }

    private TypeElement getEnclosingClass(Element element) {
        Element parentElem = element.getEnclosingElement();

        if (parentElem != null) {
            if ((parentElem.getKind() == ElementKind.CLASS) || (parentElem.getKind() == ElementKind.ENUM)) {
                return (TypeElement) parentElem;
            } else {
                return getEnclosingClass(parentElem);
            }
        }

        return null;
    }

    private static Icon getIcon(Element method) {
        Icon icon;

        if (method.getModifiers().contains(Modifier.STATIC)) {
            if (method.getModifiers().contains(Modifier.PUBLIC)) {
                icon = IconResource.METHOD_PUBLIC_STATIC_ICON;
            } else if (method.getModifiers().contains(Modifier.PROTECTED)) {
                icon = IconResource.METHOD_PROTECTED_STATIC_ICON;
            } else if (method.getModifiers().contains(Modifier.PRIVATE)) {
                icon = IconResource.METHOD_PRIVATE_STATIC_ICON;
            } else {
                icon = IconResource.METHOD_PACKAGE_STATIC_ICON;
            }
        } else {
            if (method.getModifiers().contains(Modifier.PUBLIC)) {
                icon = IconResource.METHOD_PUBLIC_ICON;
            } else if (method.getModifiers().contains(Modifier.PROTECTED)) {
                icon = IconResource.METHOD_PROTECTED_ICON;
            } else if (method.getModifiers().contains(Modifier.PRIVATE)) {
                icon = IconResource.METHOD_PRIVATE_ICON;
            } else {
                icon = IconResource.METHOD_PACKAGE_ICON;
            }
        }
        return icon;
    }
}
