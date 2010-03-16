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

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class TypeImportEntry extends ImportEntry {
    private boolean isStatic;
    private String packageName, typeName;

    public TypeImportEntry(String packageName, String typeName) {
        this(packageName, typeName, false, -1, -1, -1, -1);
    }

    public TypeImportEntry(String packageName, String typeName, boolean staticImport, int startPos, int endPos, int startFQN, int endFQN) {
        super(startPos, endPos, startFQN, endFQN);
        isStatic = staticImport;
        this.packageName = packageName;
        this.typeName = typeName;
    }

    @Override
    public String getPackageName() {
        return packageName;
    }

    @Override
    public String getTypeName() {
        return typeName;
    }

    @Override
    protected boolean checkContains(ImportEntry otherEntry) {
        return getPackageName().equals(otherEntry.getPackageName()) && getTypeName().equals(otherEntry.getTypeName());
    }

    @Override
    public String toString() {
        return "import " + getTypeName() + (isStatic ? ".*" : ""); // NOI18N
    }

    @Override
    protected boolean computeEquals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final TypeImportEntry other = (TypeImportEntry) obj;
        if (this.isStatic != other.isStatic) {
            return false;
        }
        if ((this.typeName == null) ? (other.typeName != null) : !this.typeName.equals(other.typeName)) {
            return false;
        }
        return true;
    }

    @Override
    protected int computeHash() {
        int hash = 3;
        hash = 73 * hash + (this.isStatic ? 1 : 0);
        hash = 73 * hash + (this.typeName != null ? this.typeName.hashCode() : 0);
        return hash;
    }
}
