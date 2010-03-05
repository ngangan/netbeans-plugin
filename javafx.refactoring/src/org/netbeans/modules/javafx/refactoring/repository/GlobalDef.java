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

import java.util.StringTokenizer;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.NestingKind;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class GlobalDef extends ElementDef {
    private String refId;
    private boolean indexable = true;

    public GlobalDef(String name, ElementKind kind, NestingKind nestingKind, int startPos, int endPos, int startFQN, int endFQN, String refId, boolean synth, ClassModel parent) {
        super(name, kind, nestingKind, startPos, endPos, startFQN, endFQN, synth, parent);
        this.refId = refId;
    }

    public GlobalDef(String name, ElementKind kind, int startPos, int endPos, int startFQN, int endFQN, String refId, boolean synth, ClassModel parent) {
        super(name, kind, startPos, endPos, startFQN, endFQN, synth, parent);
        this.refId = refId;
    }

    public GlobalDef(String name, ElementKind kind, NestingKind nestingKind, int startPos, int endPos, int startFQN, int endFQN, String refId, ClassModel parent) {
        this(name, kind, nestingKind, startPos, endPos, startFQN, endFQN, refId, false, parent);
    }
    
    public GlobalDef(String name, ElementKind kind, int startPos, int endPos, int startFQN, int endFQN, String refId, ClassModel parent) {
        this(name, kind, startPos, endPos, startFQN, endFQN, refId, false, parent);
    }

    public String getRefId() {
        return refId;
    }

    @Override
    public ElementHandle createHandle() {
        StringTokenizer st = new StringTokenizer(refId, "#");
        int cnt = 0;
        switch (getKind()) {
            case PACKAGE:
            case CLASS:
            case INTERFACE:
            case ENUM:
            case ANNOTATION_TYPE:
            case OTHER: {
                cnt = 1;
                break;
            }
            case INSTANCE_INIT:
            case STATIC_INIT: {
                cnt = 2;
                break;
            }
            case FIELD:
            case ENUM_CONSTANT:
            case METHOD:
            case CONSTRUCTOR: {
                cnt = 3;
                break;
            }
        }
        String[] sigs = new String[cnt];
        for(int i=0;i<cnt;i++) {
            sigs[i] = st.nextToken();
        }
        return new ElementHandle(getKind(), sigs);
    }

    @Override
    public boolean isIndexable() {
        return indexable;
    }

    public void setIndexable(boolean indexable) {
        this.indexable = indexable;
    }

    @Override
    public Element resolveElement(CompilationController cc) {
        ElementHandle eh = createHandle();
        if (eh != null && eh != ElementDef.NULL.createHandle()) {
            return eh.resolve(cc);
        }
        return null;
    }
}
