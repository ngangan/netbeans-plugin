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

import java.io.IOException;
import org.netbeans.api.javafx.source.ClassIndex.SearchScope;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.PackageElement;
import javax.lang.model.util.Elements;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.modules.javafx.profiler.utilities.JavaFXProjectUtilities;
import org.netbeans.modules.javafx.project.JavaFXProject;
import org.netbeans.modules.profiler.selector.spi.nodes.ContainerNode;
import org.netbeans.modules.profiler.selector.spi.nodes.SelectorChildren;
import org.netbeans.modules.profiler.selector.spi.nodes.SelectorNode;
import org.openide.filesystems.FileObject;

/**
 *
 * @author cms
 */
public class JavaFXProjectPackages extends SelectorChildren<ContainerNode> {
    
    JavaFXProject project;
    JavaFXSource source;

    public static enum PackageType {
        Libraries, Source;
    }

    private final JavaFXProjectPackages.PackageType packageType;
    private final Set<SearchScope> scope = new HashSet<SearchScope>();
    private final boolean subprojects;

    public JavaFXProjectPackages(final JavaFXProjectPackages.PackageType type, final JavaFXProject project, final boolean includeSubprojects) {
        this.packageType = type;
        this.project = project;

        switch (type) {
            case Source:
                scope.add(SearchScope.SOURCE);

                break;
            case Libraries:
                scope.add(SearchScope.DEPENDENCIES);

                break;
        }

        this.subprojects = includeSubprojects;
    }

    protected List<SelectorNode> prepareChildren(ContainerNode parent) {
        final List<SelectorNode> pkgs = new ArrayList<SelectorNode>();

        source = JavaFXSource.forFileObject(JavaFXProjectUtilities.getSourceFiles(project).get(0));

        final ClasspathInfo cpInfo = source.getCpInfo();
        ClassIndex index = cpInfo.getClassIndex();

        if (scope.contains(SearchScope.DEPENDENCIES)) {
            for (String pkgName : index.getPackageNames("", true, scope)) { // NOI18N
                final String pkgNameFinal = pkgName;
                final ContainerNode parentFinal = parent;
                try {
                    source.runUserActionTask(new CancellableTask<CompilationController>() {
                        public void cancel() {
                        }

                        public void run(CompilationController controller)
                                 throws Exception {
                            if (JavaFXSource.Phase.ANALYZED.compareTo(controller.toPhase(JavaFXSource.Phase.ANALYZED))<=0) {
                                PackageElement pelem = controller.getElements().getPackageElement(pkgNameFinal);

                                if (pelem != null) {
                                    pkgs.add(new JavaFXPackageNode(cpInfo, pkgNameFinal, parentFinal, scope, source));
                                }
                            }
                        }
                    }, true);
                } catch (IOException ex) {
                    // TBD
                }
            }
            Collections.sort(pkgs, JavaFXPackageNode.COMPARATOR);
            return pkgs;
        } else {
            return collectPackages(parent, cpInfo, project.getFOSourceRoots(), pkgs);
        }
    }

   private List<SelectorNode> collectPackages(ContainerNode parent, ClasspathInfo cpInfo, FileObject[] roots, List<SelectorNode> pkgs) {

        for (int i = 0; i < roots.length; i++) {
            if (roots[i].isFolder()) {
                pkgs = collectPackages(parent, cpInfo, roots[i], pkgs);
            }
        }
        return pkgs;
   }

   private List<SelectorNode> collectPackages(ContainerNode parent, ClasspathInfo cpInfo, FileObject root, List<SelectorNode> pkgs) {
        FileObject[] files = root.getChildren();

        if (files != null) {
            for (int i = 0; i < files.length; i++) {
                if (!files[i].isFolder()) {
                    if (JavaFXProjectUtilities.SOURCES_TYPE_JAVA.equalsIgnoreCase(files[i].getExt()) ||
                        JavaFXProjectUtilities.SOURCES_TYPE_JAVAFX.equalsIgnoreCase(files[i].getExt())) {
                        JavaFXPackageNode node = new JavaFXPackageNode(cpInfo, root.getName(), parent, scope, source);
                        if (!pkgs.contains(node))
                            pkgs.add(node);
                    }
                } else {
                    // this is a folder. Make recursive call.
                    pkgs = collectPackages(parent, cpInfo, files[i], pkgs);
                }
            }
        }
        return pkgs;
    }
}
