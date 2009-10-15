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

import org.netbeans.api.javafx.source.ClassIndex.SearchScope;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.lib.profiler.client.ClientUtils;
import org.netbeans.modules.profiler.selector.spi.nodes.IconResource;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.ElementFilter;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.profiler.selector.spi.nodes.ContainerNode;
import org.netbeans.modules.profiler.selector.spi.nodes.SelectorChildren;
import org.netbeans.modules.profiler.selector.spi.nodes.SelectorNode;


/**
 *
 * @author cms
 */
public class JavaFXPackageNode extends ContainerNode {
    
    private static class JavaFXPackageChildren extends SelectorChildren<JavaFXPackageNode> {
        protected List<SelectorNode> prepareChildren(JavaFXPackageNode parent) {
            List<SelectorNode> nodes = new ArrayList<SelectorNode>();
            List<JavaFXClassNode> classes = getClasses(parent);
            List<JavaFXPackageNode> packages = getSubpackages(parent);
            nodes.addAll(classes);
            nodes.addAll(packages);

            return nodes;
        }

        private List<JavaFXClassNode> getClasses(final JavaFXPackageNode parent) {
            final List<JavaFXClassNode> nodes = new ArrayList<JavaFXClassNode>();

            try {
                parent.getJFXSource().runUserActionTask(new CancellableTask<CompilationController>() {
                    public void cancel() {
                    }

                    public void run(CompilationController controller)
                             throws Exception {
                        String pkgName = parent.getName();
                        if (pkgName.equals(DEFAULT_NAME)) {
                            pkgName = "";
                        }
                        PackageElement pelem = controller.getElements().getPackageElement(pkgName);

                        if (pelem != null) {
                            List<? extends Element> ee = pelem.getEnclosedElements();
                            for (TypeElement type : ElementFilter.typesIn(pelem.getEnclosedElements())) {
                                System.err.println(type);
                                if ((type.getKind() == ElementKind.CLASS) || (type.getKind() == ElementKind.ENUM)) {
                                    nodes.add(new JavaFXClassNode(parent.cpInfo, IconResource.CLASS_ICON, type, parent));
                                }
                            }
                        } else {
                            LOGGER.log(Level.FINEST, "Package name {0} resulted into a NULL element", parent.getName()); // NOI18N
                        }
                    }
                }, true);
            } catch (IOException ex) {
                LOGGER.severe(ex.getLocalizedMessage());
            }
            Collections.sort(nodes, JavaFXClassNode.COMPARATOR);

            return nodes;
        }

        private List<JavaFXPackageNode> getSubpackages(final JavaFXPackageNode parent) {
            ClassIndex index = parent.cpInfo.getClassIndex();
            List<JavaFXPackageNode> nodes = new ArrayList<JavaFXPackageNode>();

            for (String pkgName : index.getPackageNames(parent.getName() + ".", true, parent.scope)) { // NOI18N
                nodes.add(new JavaFXPackageNode(parent.cpInfo, pkgName, parent, parent.scope, parent.getJFXSource(), parent.isLibraryNode()));
            }

            Collections.sort(nodes, COMPARATOR);

            return nodes;
        }
    }

    public static final String DEFAULT_NAME = "<default>"; // NOI18N
    static final Comparator COMPARATOR = new Comparator<JavaFXPackageNode>() {
        public int compare(JavaFXPackageNode o1, JavaFXPackageNode o2) {
            if (o1.getNodeName().equals(JavaFXPackageNode.DEFAULT_NAME)) {
                return -1;
            }

            return o1.toString().compareTo(o2.toString());
        }
    };

    private static final Logger LOGGER = Logger.getLogger(JavaFXPackageNode.class.getName());

    private final ClientUtils.SourceCodeSelection signature;
    private final ClasspathInfo cpInfo;
    private final Set<SearchScope> scope;
    private final String name;
    private final JavaFXSource js;
    private final boolean isLibraryNode;

    public JavaFXPackageNode(final ClasspathInfo cpInfo, String name, final ContainerNode parent, final Set<SearchScope> scope, final JavaFXSource js, final boolean isLibraryNode) {
        super(stripName(defaultizeName(name)), IconResource.PACKAGE_ICON, parent);
        this.name = name;
        this.cpInfo = cpInfo;
        this.signature = new ClientUtils.SourceCodeSelection(name + ".**", null, null); // NOI18N
        this.scope = scope;
        this.js = js;
        this.isLibraryNode = isLibraryNode;
    }

    public String getName() {
        return name;
    }

    public boolean isLibraryNode() {
        return isLibraryNode;
    }

    JavaFXSource getJFXSource() {
        return js;
    }

    @Override
    public ClientUtils.SourceCodeSelection getSignature() {
        return signature;
    }

    protected SelectorChildren getChildren() {
        return new JavaFXPackageChildren();
    }

    ClasspathInfo getCpInfo() {
        return cpInfo;
    }

    private static String defaultizeName(String name) {
        return ((name == null) || (name.length() == 0)) ? DEFAULT_NAME : name;
    }

    private static String stripName(String name) {
        int lastDot = name.lastIndexOf('.'); // NOI18N

        if (lastDot > -1) {
            return name.substring(lastDot + 1);
        }

        return name;
    }
}
