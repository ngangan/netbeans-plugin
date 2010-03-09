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

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.NestingKind;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
abstract public class ElementDef implements Referencable {
    final static protected  Logger LOGGER = Logger.getLogger(ElementDef.class.getName());

    private int startPos, endPos, startFQN, endFQN;
    private String name;
    private ElementKind kind;
    private NestingKind nestingKind;
    private Set<ElementDef> overridden = new HashSet<ElementDef>();
    private ClassModel enclosing;
    private boolean synthetic;
    private String pkgName;

    public ElementDef(String name, ElementKind kind, String enclosingPkg, int startPos, int endPos, int startFQN, int endFQN, boolean synth, ClassModel parent) {
        this(name, kind, NestingKind.MEMBER, enclosingPkg, startPos, endPos, startFQN, endFQN, synth, parent);
    }

    public ElementDef(String name, ElementKind kind, NestingKind nestingKind, String enclosingPkg, int startPos, int endPos, int startFQN, int endFQN, boolean synth, ClassModel parent) {
        this.startPos = startPos;
        this.endPos = endPos;
        this.startFQN = startFQN;
        this.endFQN = endFQN;
        this.name = name;
        this.pkgName = enclosingPkg;
        this.kind = kind;
        this.enclosing = parent;
        this.synthetic = synth;
        this.nestingKind = nestingKind;
    }

    public ElementDef(String name, ElementKind kind, String enclosingPkg, int startPos, int endPos, int startFQN, int endFQN, ClassModel parent) {
        this(name, kind, NestingKind.MEMBER, enclosingPkg, startPos, endPos, startFQN, endFQN, parent);
    }

    public ElementDef(String name, ElementKind kind, NestingKind nestingKind, String enclosingPkg, int startPos, int endPos, int startFQN, int endFQN, ClassModel parent) {
        this(name, kind, nestingKind, enclosingPkg, startPos, endPos, startFQN, endFQN, false, parent);
    }

    final public static ElementDef NULL = new ElementDef("", ElementKind.OTHER, NestingKind.ANONYMOUS, "",  -1, -1, -1, -1, null){
        final private ElementHandle NULL_HANDLE = new ElementHandle(ElementKind.OTHER, new String[]{""});
        public String getRefId() {
            return "<null>"; // NOI18N
        }

        @Override
        public ElementHandle createHandle() {
            return NULL_HANDLE;
        }

        @Override
        public boolean isIndexable() {
            return false;
        }

        @Override
        public Element resolveElement(CompilationController cc) {
            return null;
        }
    };

    final public int getEndFQN() {
        return endFQN;
    }

    final public int getEndPos() {
        return endPos;
    }

    final public String getName() {
        return name;
    }

    final public String getPackageName() {
        return pkgName;
    }

    final public int getStartFQN() {
        return startFQN;
    }

    final public int getStartPos() {
        return startPos;
    }

    final public ElementKind getKind() {
        return kind;
    }

    final public NestingKind getNestingKind() {
        return nestingKind;
    }

    final public boolean overrides(ElementDef def) {
        return overridden.contains(def);
    }

    final public void addOverridenDef(ElementDef def) {
        overridden.add(def);
    }

    final public boolean isOverriding() {
        return !overridden.isEmpty();
    }

    final public Set<ElementDef> getOverridden() {
        return Collections.unmodifiableSet(overridden);
    }

    final public boolean isSynthetic() {
        return synthetic;
    }

    public Element getElement() {
        if (enclosing == null) return null;
        
        final Element[] rslt = new Element[1];
        try {
            JavaFXSource.forFileObject(enclosing.getSourceFile()).runUserActionTask(new Task<CompilationController>() {

                public void run(CompilationController cc) throws Exception {
                    rslt[0] = resolveElement(cc);
                }
            }, true);
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, null, e);
        }
        return rslt[0];
    }

    abstract public ElementHandle createHandle();
    abstract public boolean isIndexable();
    abstract public Element resolveElement(CompilationController cc);

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final ElementDef other = (ElementDef) obj;
        if ((this.getRefId() == null) ? (other.getRefId() != null) : !this.getRefId().equals(other.getRefId())) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 5;
        hash = 83 * hash + (this.getRefId() != null ? this.getRefId().hashCode() : 0);
        return hash;
    }

        @Override
    public String toString() {
        return getRefId() + enclosing != null ? " (in " + enclosing.getSourceFile().getPath() + ")" : ""; // NOI18N
    }
}
