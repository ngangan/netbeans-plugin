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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.ElementKind;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class ImportSet {
    static public class Touple<T1, T2> {
        private T1 t1;
        private T2 t2;

        public Touple(T1 t1, T2 t2) {
            this.t1 = t1;
            this.t2 = t2;
        }

        public T1 getT1() {
            return t1;
        }

        public void setT1(T1 t1) {
            this.t1 = t1;
        }

        public T2 getT2() {
            return t2;
        }

        public void setT2(T2 t2) {
            this.t2 = t2;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final Touple<T1, T2> other = (Touple<T1, T2>) obj;
            if (this.t1 != other.t1 && (this.t1 == null || !this.t1.equals(other.t1))) {
                return false;
            }
            if (this.t2 != other.t2 && (this.t2 == null || !this.t2.equals(other.t2))) {
                return false;
            }
            return true;
        }

        @Override
        public int hashCode() {
            int hash = 3;
            hash = 79 * hash + (this.t1 != null ? this.t1.hashCode() : 0);
            hash = 79 * hash + (this.t2 != null ? this.t2.hashCode() : 0);
            return hash;
        }
    }
    private String pkgName;
    private ClassModel cm;
    final private Map<String, String> renames = new HashMap<String, String>();

    public ImportSet(ClassModel cm) {
        this.pkgName = cm.getPackageDef().getName();
        this.cm = cm;
    }

    public String getPkgName() {
        return pkgName;
    }

    public void setPkgName(String pkgName) {
        this.pkgName = pkgName;
    }

    public Set<ImportEntry> getDeclared() {
        return cm.getImports();
    }

    public void addRename(String oldFQN, String newFQN) {
        renames.put(oldFQN, newFQN);
    }

    public Set<Touple<ElementDef, ImportEntry>> getMissing() {
        Set<Touple<ElementDef, ImportEntry>> missing = new HashSet<Touple<ElementDef, ImportEntry>>();
        OUTER:
        for(Usage usg : cm.getUsages()) {
            ElementKind usgKind = usg.getDef().getKind();
            if (usgKind.isClass() || usgKind.isInterface()) {
                String typeFQN = usg.getDef().createHandle().getQualifiedName();
                String typeOrigFQN = typeFQN;
                if (renames.containsKey(typeFQN)) {
                    typeFQN = renames.get(typeFQN);
                    if (typeFQN.startsWith(".")) { // moving class to default package; the renamed type starts with "."
                        typeFQN = typeFQN.substring(1);
                    }
                }
                String typeSimple = usg.getDef().getName();

                int pkgLen = typeOrigFQN.length() - typeSimple.length();

                String fqnOrigPkg = pkgLen > 0 ? typeOrigFQN.substring(0, pkgLen -1) : ""; // NOI18N
                pkgLen = typeFQN.length() - typeSimple.length();

                if (pkgLen > 0) {
                    String fqnPkg = pkgLen > 0 ? typeFQN.substring(0, pkgLen - 1) : ""; // NOI18N
                    TypeImportEntry tie = new TypeImportEntry(fqnPkg, typeFQN);
                    TypeImportEntry tieOrig = new TypeImportEntry(fqnOrigPkg, typeOrigFQN);

                    if (fqnPkg.startsWith("java.lang") || fqnPkg.startsWith("javafx.lang")) continue; // NOI18N

                    if (renames.containsKey(fqnPkg)) {
                        fqnPkg = renames.get(fqnPkg);
                    }
                    for(ImportEntry decl : cm.getImports()) {
                        if (renames.containsKey(decl.getPackageName()) || renames.containsKey(decl.getTypeName())) {
                            if (decl.contains(tieOrig)) continue OUTER;
                        } else {
                            if (decl.contains(tie)) continue OUTER;
                        }
                    }
                    if (!fqnPkg.equals(pkgName)) {
                        missing.add(new Touple<ElementDef, ImportEntry>(usg.getDef(), tie));
                    }
                }
            }
        }
        return missing;
    }

    public Set<ImportEntry> getUnused() {
        Set<ImportEntry> unused = new HashSet<ImportEntry>();
        unused.addAll(cm.getImports());

        for(Usage usg : cm.getUsages()) {
            ElementKind usgKind = usg.getDef().getKind();
            switch (usgKind) {
                case CLASS:
                case INTERFACE:
                case ENUM: {
                    String typeFQN = usg.getDef().createHandle().getQualifiedName();
                    String typeOrigFQN = typeFQN;

                    if (renames.containsKey(typeFQN)) {
                        typeFQN = renames.get(typeFQN);
                    }
                    String typeSimple = usg.getDef().getName();

                    int pkgLen = typeOrigFQN.length() - typeSimple.length();

                    if (pkgLen > 0) {
                        String fqnOrigPkg = pkgLen > 0 ? typeOrigFQN.substring(0, pkgLen -1) : ""; // NOI18N
                        pkgLen = typeFQN.length() - typeSimple.length();

                        String fqnPkg = pkgLen > 0 ? typeFQN.substring(0, pkgLen - 1) : ""; // NOI18N

                        TypeImportEntry tie = new TypeImportEntry(fqnPkg, typeFQN);
                        TypeImportEntry tieOrig = new TypeImportEntry(fqnOrigPkg, typeOrigFQN);

                        for(Iterator<ImportEntry> iter=unused.iterator();iter.hasNext();) {
                            ImportEntry decl = iter.next();
                            if (!pkgName.equals(fqnPkg)){
                                if (renames.containsKey(decl.getPackageName()) || renames.containsKey(decl.getTypeName())) {
                                    if (decl.contains(tieOrig)) {
                                        iter.remove();
                                    }
                                } else {
                                    if (decl.contains(tie)) {
                                        iter.remove();
                                    }
                                }
                            }
                        }
                    }
                    break;
                }
                case METHOD:
                case FIELD: {
                    String typeFQN = SourceUtils.getEnclosingTypeName(usg.getDef());
                    TypeImportEntry tie = new TypeImportEntry("", typeFQN, true, -1, -1, -1, -1); // NOI18N
                    if (renames.containsKey(typeFQN)) {
                        typeFQN = renames.get(typeFQN);
                    }
                    TypeImportEntry tieRenamed = new TypeImportEntry("", typeFQN, true, -1, -1, -1, -1); // NOI18N
                    for(Iterator<ImportEntry> iter=unused.iterator();iter.hasNext();) {
                        ImportEntry decl = iter.next();
                        if (renames.containsKey(decl.getTypeName())) {
                            if (decl.contains(tie)) {
                                iter.remove();
                            }
                        } else {
                            if (decl.contains(tieRenamed)) {
                                iter.remove();
                            }
                        }
                    }
                }
            }
            if (unused.isEmpty()) break; // no need to do more checks; all imports are used
        }
        return unused;
    }

    public boolean isImported(String packageName, String typeName) {
        ImportEntry checking = new TypeImportEntry(packageName, packageName + "." + typeName); // NOI18N
        for(ImportEntry ie : cm.getImports()) {
            if (ie.contains(checking)) {
                return true;
            }
        }
        return false;
    }
//
//    public ImportEntry getImport(String packageName, String typeName) {
//        String fqn = packageName + "." + typeName;
//        for(ImportEntry ie : cm.getImports()) {
//            if (ie.getPackageName().equals(packageName) && ie.getTypeName().equals(fqn)) {
//                return ie;
//            }
//        }
//        return null;
//    }
}
