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

import com.sun.visage.api.tree.ClassDeclarationTree;
import com.sun.visage.api.tree.VisageTreePathScanner;
import org.netbeans.api.visage.source.ClassIndex.SearchScope;
import org.netbeans.api.visage.source.CancellableTask;
import org.netbeans.api.visage.source.CompilationController;
import org.netbeans.api.visage.source.VisageSource;
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
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.ElementFilter;
import org.netbeans.api.visage.source.ClassIndex;
import org.netbeans.api.visage.source.ClasspathInfo;
import org.netbeans.api.visage.source.ElementHandle;
import org.netbeans.api.visage.source.VisageSourceUtils;
import org.netbeans.modules.profiler.selector.spi.nodes.ContainerNode;
import org.netbeans.modules.profiler.selector.spi.nodes.SelectorChildren;
import org.netbeans.modules.profiler.selector.spi.nodes.SelectorNode;


/**
 *
 * @author cms
 */
public class VisagePackageNode extends ContainerNode {
    
    private static class VisagePackageChildren extends SelectorChildren<VisagePackageNode> {
        protected List<SelectorNode> prepareChildren(VisagePackageNode parent) {
            List<SelectorNode> nodes = new ArrayList<SelectorNode>();
            List<VisageClassNode> classes = getClasses(parent);
            List<VisagePackageNode> packages = getSubpackages(parent);
            nodes.addAll(classes);
            nodes.addAll(packages);

            return nodes;
        }

        private List<VisageClassNode> getClasses(final VisagePackageNode parent) {
            final List<VisageClassNode> nodes = new ArrayList<VisageClassNode>();

            try {
                parent.getVSGSource().runUserActionTask(new CancellableTask<CompilationController>() {
                    public void cancel() {
                    }

                    public void run(final CompilationController cc)
                             throws Exception {
                        String pkgName = parent.getName();

                        for(ElementHandle<TypeElement> teh : parent.cpInfo.getClassIndex().getDeclaredTypes((pkgName.equals(DEFAULT_NAME) ? "" : pkgName.replace(".", "\\.") + "\\.") + ".*", ClassIndex.NameKind.REGEXP, parent.scope)) {
                            TypeElement e = teh.resolve(cc);
                            if (e == null || e.getNestingKind() != NestingKind.TOP_LEVEL) continue;
                            PackageElement pe = VisageSourceUtils.getEnclosingPackageElement(e);
                            if ((pe.isUnnamed() && pkgName.equals(DEFAULT_NAME)) ||
                                (pe.getQualifiedName().contentEquals(pkgName))) {
                                nodes.add(new VisageClassNode(parent.cpInfo, IconResource.CLASS_ICON, e, parent));
                            }
                        }
//
//                        PackageElement pelem = cc.getElements().getPackageElement(pkgName);
//
//                        if (pelem != null) {
//                            List<? extends Element> ee = pelem.getEnclosedElements();
//                            for (TypeElement type : ElementFilter.typesIn(pelem.getEnclosedElements())) {
//                                System.err.println(type);
//                                if ((type.getKind() == ElementKind.CLASS) || (type.getKind() == ElementKind.ENUM)) {
//                                    nodes.add(new VisageClassNode(parent.cpInfo, IconResource.CLASS_ICON, type, parent));
//                                }
//                            }
//                        } else {
//                            LOGGER.log(Level.FINEST, "Package name {0} resulted into a NULL element", parent.getName()); // NOI18N
//                        }
                    }
                }, true);
            } catch (IOException ex) {
                LOGGER.severe(ex.getLocalizedMessage());
            }
            Collections.sort(nodes, VisageClassNode.COMPARATOR);

            return nodes;
        }

        private List<VisagePackageNode> getSubpackages(final VisagePackageNode parent) {
            ClassIndex index = parent.cpInfo.getClassIndex();
            List<VisagePackageNode> nodes = new ArrayList<VisagePackageNode>();

            for (String pkgName : index.getPackageNames(parent.getName() + ".", true, parent.scope)) { // NOI18N
                nodes.add(new VisagePackageNode(parent.cpInfo, pkgName, parent, parent.scope, parent.getVSGSource(), parent.isLibraryNode()));
            }

            Collections.sort(nodes, COMPARATOR);

            return nodes;
        }
    }

    public static final String DEFAULT_NAME = "<default>"; // NOI18N
    static final Comparator COMPARATOR = new Comparator<VisagePackageNode>() {
        public int compare(VisagePackageNode o1, VisagePackageNode o2) {
            if (o1.getNodeName().equals(VisagePackageNode.DEFAULT_NAME)) {
                return -1;
            }

            return o1.toString().compareTo(o2.toString());
        }
    };

    private static final Logger LOGGER = Logger.getLogger(VisagePackageNode.class.getName());

    private final ClientUtils.SourceCodeSelection signature;
    private final ClasspathInfo cpInfo;
    private final Set<SearchScope> scope;
    private final String name;
    private final VisageSource js;
    private final boolean isLibraryNode;

    public VisagePackageNode(final ClasspathInfo cpInfo, String name, final ContainerNode parent, final Set<SearchScope> scope, final VisageSource js, final boolean isLibraryNode) {
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

    VisageSource getVSGSource() {
        return js;
    }

    @Override
    public ClientUtils.SourceCodeSelection getSignature() {
        return signature;
    }

    protected SelectorChildren getChildren() {
        return new VisagePackageChildren();
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
