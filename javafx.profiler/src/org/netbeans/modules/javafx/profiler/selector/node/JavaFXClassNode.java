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

package org.netbeans.modules.javafx.profiler.selector.node;

import org.netbeans.modules.profiler.selector.spi.nodes.SelectorChildren;
import java.util.Comparator;
import javax.lang.model.element.Element;
import org.netbeans.lib.profiler.client.ClientUtils;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.profiler.selector.spi.nodes.ContainerNode;
import javax.lang.model.element.TypeElement;
import javax.swing.Icon;
import org.netbeans.modules.javafx.profiler.utilities.JavaFXProjectUtilities;


/**
 *
 * @author cms
 */
public class JavaFXClassNode extends ContainerNode {

    private ClasspathInfo cpInfo;
    private ElementHandle<TypeElement> classHandle;

    public JavaFXClassNode(ClasspathInfo cpInfo, Icon icon, TypeElement classElement, ContainerNode parent) {
        super(classElement.getSimpleName().toString(), icon, parent);
        this.classElement = classElement;
        this.cpInfo = cpInfo;
        signature = new ClientUtils.SourceCodeSelection(JavaFXProjectUtilities.getBinaryName(classElement, classElement.getEnclosingElement()), "*", ""); // NOI18N
        this.classHandle = ElementHandle.create(classElement);
    }

    public ElementHandle<TypeElement> getClassHandle() {
        return classHandle;
    }

    public Element getClassElement() {
        return classElement;
    }
    public ClasspathInfo getCpInfo() {
        return cpInfo;
    }

    public static final Comparator COMPARATOR = new Comparator<JavaFXClassNode>() {
        public int compare(JavaFXClassNode o1, JavaFXClassNode o2) {
            return o1.toString().compareTo(o2.toString());
        }
    };

    private TypeElement classElement;
    private final ClientUtils.SourceCodeSelection signature;
    

    @Override
    public ClientUtils.SourceCodeSelection getSignature() {
        return signature;
//        return new ClientUtils.SourceCodeSelection(((TypeElement)classElement).getQualifiedName().toString(),
//                                                         JavaFXProjectUtilities.MAGIC_METHOD_NAME, JavaFXProjectUtilities.MAGIC_METHOD_SIGNATURE);
    }

    protected SelectorChildren getChildren() {
        boolean isLibraryNode = false;
        if (parent instanceof JavaFXPackageNode) {
            isLibraryNode = ((JavaFXPackageNode)parent).isLibraryNode();
        }
        return new JavaFXClassChildren(isLibraryNode);
    }    
}
