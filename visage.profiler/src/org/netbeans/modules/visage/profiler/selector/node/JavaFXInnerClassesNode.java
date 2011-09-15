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

import org.netbeans.modules.profiler.selector.spi.nodes.ContainerNode;
import org.netbeans.modules.profiler.selector.spi.nodes.GreedySelectorChildren;
import org.netbeans.modules.profiler.selector.spi.nodes.IconResource;
import org.netbeans.modules.profiler.selector.spi.nodes.SelectorChildren;
import org.netbeans.modules.profiler.selector.spi.nodes.SelectorNode;
import org.netbeans.api.visage.source.CancellableTask;
import org.netbeans.api.visage.source.ClasspathInfo;
import org.netbeans.api.visage.source.CompilationController;
import org.netbeans.api.visage.source.VisageSource;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.visage.source.ElementHandle;
import org.netbeans.modules.visage.profiler.utilities.VisageProjectUtilities;
import org.netbeans.modules.profiler.selector.spi.nodes.InnerClassesNode;
import org.openide.util.NbBundle;

/**
 *
 * @author cms
 */
public class VisageInnerClassesNode extends ContainerNode {

    private static class Children extends GreedySelectorChildren<VisageInnerClassesNode> {

        protected List<?extends SelectorNode> prepareChildren(final VisageInnerClassesNode parent) {
            final List<VisageClassNode> classNodes = new ArrayList<VisageClassNode>();

            try {
                VisageSource js = VisageSource.forFileObject(VisageProjectUtilities.getFile(classElement, parent.cpInfo));
                js.runUserActionTask(new CancellableTask<CompilationController>() {
                        public void cancel() {
                        }

                        public void run(CompilationController controller)
                                 throws Exception {
                            if (VisageSource.Phase.ANALYZED.compareTo(controller.toPhase(VisageSource.Phase.ANALYZED))<=0) {
                                classElement = parent.getClassHandle().resolve(controller);

                                List<? extends Element> methods = controller.getElements().getAllMembers((TypeElement)classElement);
                                for (int k = 0; k < methods.size(); k++){
                                    Element tek = methods.get(k);
                                    if (classElement.equals(tek.getEnclosingElement()) && tek.getKind().isClass()) {
                                        VisageClassNode classNode = new VisageClassNode(parent.cpInfo, IconResource.CLASS_ICON, (TypeElement)tek, parent);
                                        if (classNode.getSignature() != null) {
                                            classNodes.add(classNode);
                                        }
                                    }
                                }
                            }
                        }
                    }, true);
                Collections.sort(classNodes, VisageClassNode.COMPARATOR);
            } catch (IllegalArgumentException ex) {
                ex.printStackTrace();
            } catch (IOException ex) {
                ex.printStackTrace();
            }

            return classNodes;
        }        
    }
        
    private final ClasspathInfo cpInfo;
    private static Element classElement;
    

    /** Creates a new instance of VisageInnerClassesNode */
    public VisageInnerClassesNode(final ClasspathInfo cpInfo, final VisageClassNode parent) {
        super(NbBundle.getMessage(InnerClassesNode.class, "InnerClasses_DisplayName"), IconResource.CLASS_ICON, parent); // NOI18N
        classElement = parent.getClassElement();
        this.cpInfo = cpInfo;
    }

    public ElementHandle<TypeElement> getClassHandle() {
        return ((VisageClassNode) getParent()).getClassHandle();
    }

    protected SelectorChildren getChildren() {
        return new Children();
    }
}
