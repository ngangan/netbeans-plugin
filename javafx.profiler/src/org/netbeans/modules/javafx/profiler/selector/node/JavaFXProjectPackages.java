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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.netbeans.api.javafx.source.ClasspathInfo;
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
    public static enum PackageType {//~ Enumeration constant initializers ------------------------------------------------------------------------------------

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
        List<SelectorNode> pkgs = new ArrayList<SelectorNode>();

        ClasspathInfo cpInfo = JavaFXProjectUtilities.createClassPathInfo((JavaFXProject)project);
        
        FileObject[] roots = project.getFOSourceRoots();

        pkgs = collectPackages(parent, cpInfo, roots, pkgs);

        Collections.sort(pkgs, JavaFXPackageNode.COMPARATOR);

        return pkgs;
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
                        JavaFXPackageNode node = new JavaFXPackageNode(cpInfo, root, parent, scope, project);
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
