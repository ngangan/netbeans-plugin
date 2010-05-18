/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;
import javax.lang.model.element.ElementKind;
import org.openide.filesystems.FileObject;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class ClassModel {
    final private Set<ElementDef> defs = new HashSet<ElementDef>();
    final private Set<Usage> usages = new HashSet<Usage>();
    final private Set<ImportEntry> imports = new HashSet<ImportEntry>();

    private ElementDef packageDef = PackageDef.DEFAULT;
    private int packagePos = 0, importPos = 0;

    private FileObject sourceFile;

    ClassModel(FileObject sourceFile) {
        this.sourceFile = sourceFile;
    }

    void addImport(ImportEntry entry) {
        imports.add(entry);
        if (packagePos == 0 || entry.getStartPos() < packagePos) {
            packagePos = entry.getStartPos();
        }
        if (importPos < entry.getEndPos()) {
            importPos = entry.getEndPos();
        }
    }

    void addDef(ElementDef def) {
        defs.add(def);
    }

    void setPackageDef(ElementDef pd, int startPos, int endPos) {
        packageDef = pd != null ? pd : PackageDef.DEFAULT;
        if (packageDef != PackageDef.DEFAULT) {
            packagePos = startPos;
            importPos = endPos;
        }
    }

    Set<ImportEntry> getImports() {
        return Collections.unmodifiableSet(imports);
    }

    public FileObject getSourceFile() {
        return sourceFile;
    }

    public PackageDef getPackageDef() {
        return (PackageDef)packageDef;
    }

    public void addUsage(Usage ref) {
        usages.add(ref);
        ref.setClassModel(this);
    }

    public Set<ElementDef> getElementDefs() {
        return getElementDefs(EnumSet.allOf(ElementKind.class));
    }

    public Set<ElementDef> getElementDefs(Set<ElementKind> kinds) {
        Set<ElementDef> ret = new HashSet<ElementDef>();
        for(ElementDef ed : defs) {
            if (kinds.contains(ed.getKind())) {
                ret.add(ed);
            }
        }
        return Collections.unmodifiableSet(ret);
    }

    public ElementDef getDefForPos(int pos) {
        if (packageDef.getStartPos() <= pos && packageDef.getEndPos() >= pos) {
            return packageDef;
        }
        
        for(Usage ref : usages) {
            if (ref.getKind() != Usage.Kind.REFERENCE) continue;
            
            if (ref.getStartPos() <= pos && ref.getEndPos() >= pos) {
                return ref.getDef();
            }
        }

        return ElementDef.NULL;
    }

    public Usage getDefaultUsage(ElementDef def) {
        Usage usg = new Usage(def.getStartFQN(), def.getEndFQN(), Usage.Kind.REFERENCE, def);
        usg.setClassModel(this);
        return usg;
    }

    public Collection<Usage> getUsages() {
        return getUsages(null);
    }

    public Collection<Usage> getUsages(ElementDef def) {
        return getUsages(def, EnumSet.allOf(Usage.Kind.class));
    }

    public Collection<Usage> getUsages(ElementDef def, Set<Usage.Kind> kinds) {
        Collection<Usage> usgs = new ArrayList<Usage>();

        String defRefId = def != null ? def.getRefId() : null;
        for(Usage ref : usages) {
            if (defRefId == null || (ref.getRefId().equals(defRefId) && kinds.contains(ref.getKind()))) {
                usgs.add(ref);
            }
        }
        return usgs;
    }

    public int getPackagePos() {
        return packagePos;
    }

    public int getImportPos() {
        return importPos;
    }

    public ImportSet getImportSet() {
        return new ImportSet(this);
    }
}
