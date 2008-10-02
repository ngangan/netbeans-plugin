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

import org.netbeans.api.java.source.ClassIndex.SearchScope;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.lib.profiler.client.ClientUtils;
import org.netbeans.modules.profiler.selector.spi.nodes.IconResource;
import org.openide.filesystems.FileObject;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.ElementFilter;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.modules.javafx.project.JavaFXProject;
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
            nodes.addAll(classes);

            return nodes;
        }

        private List<JavaFXClassNode> getClasses(final JavaFXPackageNode parent) {
            final List<JavaFXClassNode> nodes = new ArrayList<JavaFXClassNode>();
            FileObject[] files = parent.getFO().getChildren();
            JavaFXSource js = JavaFXSource.forFileObject(files[0]);                               

            try {
                js.runUserActionTask(new CancellableTask<CompilationController>() {
                        public void cancel() {
                        }

                        public void run(CompilationController controller)
                                 throws Exception {
                            if (JavaFXSource.Phase.ANALYZED.compareTo(controller.toPhase(JavaFXSource.Phase.ANALYZED))<=0) {
                            
                                PackageElement pelem = controller.getElements().getPackageElement(parent.getName());

                                if (pelem != null) {
                                    for (TypeElement type : ElementFilter.typesIn(pelem.getEnclosedElements())) {
                                        if ((type.getKind() == ElementKind.CLASS) || (type.getKind() == ElementKind.ENUM)) {
                                            nodes.add(new JavaFXClassNode(parent.cpInfo, IconResource.CLASS_ICON, type, parent));
                                        }
                                    }
                                } else {
                                    LOGGER.log(Level.FINEST, "Package name {0} resulted into a NULL element", parent.getName()); // NOI18N
                                }
                            }
                        }
                    }, true);
            } catch (IOException ex) {
                LOGGER.severe(ex.getLocalizedMessage());
            }
            Collections.sort(nodes, JavaFXClassNode.COMPARATOR);

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
    private final FileObject fo;
    private final JavaFXProject project;
    
    /** Creates a new instance of JavaFXPackageNode */
    public JavaFXPackageNode(final ClasspathInfo cpInfo, final FileObject fo, final ContainerNode parent, final Set<SearchScope> scope, final JavaFXProject project) {
        super(stripName(defaultizeName(fo.getName())), IconResource.PACKAGE_ICON, parent);        
        this.project = project;
        this.fo = fo;
        ClassPath cp = ClassPath.getClassPath(fo, ClassPath.SOURCE);
        name = cp.getResourceName(fo, '.', false);
        this.cpInfo = cpInfo;
        // this.name = name;
        this.signature = new ClientUtils.SourceCodeSelection(name + ".*", null, null); // NOI18N
        this.scope = scope;
    }

    public String getName() {
        return name;
    }
    
    private FileObject getFO() {
        return fo;
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
