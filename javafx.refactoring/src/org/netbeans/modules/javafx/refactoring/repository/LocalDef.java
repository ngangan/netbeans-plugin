/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Sun Microsystems, Inc. All rights reserved.
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
 *
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

package org.netbeans.modules.javafx.refactoring.repository;

import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javafx.tree.JFXTree;
import com.sun.tools.javafx.tree.JavafxTreeInfo;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class LocalDef extends ElementDef {
    private int localId;

    public LocalDef(int localId, String name, ElementKind kind, String enclosingPkg, int startPos, int endPos, int startFQN, int endFQN, ClassModel parent) {
        super(name, kind, enclosingPkg, startPos, endPos, startFQN, endFQN, parent);
        this.localId = localId;
    }

    public String getRefId() {
        return getName() + "@" + getStartPos();
    }

    @Override
    public ElementHandle createHandle() {
        return ElementDef.NULL.createHandle();
    }

    @Override
    public boolean isIndexable() {
        return false;
    }

    @Override
    public Element resolveElement(CompilationController cc) {
        final Element[] rslt = new Element[1];
        new JavaFXTreePathScanner<Void, Void>() {
            private int localCntr = 0;
            @Override
            public Void visitVariable(VariableTree node, Void p) {
                Element e = JavafxTreeInfo.symbolFor((JFXTree)node);
                if (e != null && (e.getKind() == ElementKind.PARAMETER || e.getKind() == ElementKind.LOCAL_VARIABLE)) {
                    if (localCntr++ == localId) {
                        rslt[0] = e;
                        return null;
                    }
                }
                return super.visitVariable(node, p);
            }
        }.scan(cc.getCompilationUnit(), null);
        return rslt[0];
    }


}
