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

import org.openide.filesystems.FileObject;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class Usage implements Referencable {
    public static enum Kind {
        REFERENCE, SUBTYPE
    }

    private int startPos, endPos;
    private ElementDef def;
    private ClassModel enclosing;
    private Kind kind;

    public Usage(int startPos, int endPos, ElementDef def) {
        this(startPos, endPos, Kind.REFERENCE, def);
    }

    public Usage(int startPos, int endPos, Kind kind, ElementDef def) {
        this.startPos = startPos;
        this.endPos = endPos;
        this.def = def;
        this.kind = kind;
    }

    public int getEndPos() {
        return endPos;
    }

    public int getStartPos() {
        return startPos;
    }

    public ElementDef getDef() {
        return def;
    }
    
    public FileObject getFile() {
        return enclosing != null ? enclosing.getSourceFile() : null;
    }

    public Kind getKind() {
        return kind;
    }

    void setClassModel(ClassModel cm) {
        this.enclosing = cm;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final Usage other = (Usage) obj;
        if (this.startPos != other.startPos) {
            return false;
        }
        if (this.endPos != other.endPos) {
            return false;
        }
        if (!this.getRefId().equals(other.getRefId())) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 67 * hash + this.startPos;
        hash = 67 * hash + this.endPos;
        hash = 67 * hash + this.getRefId().hashCode();
        return hash;
    }

    public String getRefId() {
        return def.getRefId();
    }

    @Override
    public String toString() {
        return getDef().toString() + " [" + startPos + ", " + endPos + "]";
    }
}
