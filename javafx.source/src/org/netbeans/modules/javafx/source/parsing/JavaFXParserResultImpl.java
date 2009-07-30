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
 * 
 * Contributor(s):
 * 
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.source.parsing;

import com.sun.javafx.api.tree.UnitTree;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javafx.api.JavafxcTaskImpl;
import com.sun.tools.javafx.api.JavafxcTrees;
import com.sun.tools.javafx.code.JavafxTypes;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Logger;
import javax.lang.model.util.Elements;
import javax.lang.model.util.Types;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationPhase;
import org.netbeans.api.javafx.source.ElementUtilities;
import org.netbeans.api.javafx.source.TreeUtilities;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.modules.javafx.source.ApiSourcePackageAccessor;
import org.netbeans.modules.parsing.api.Snapshot;

/**
 *
 * @author Miloslav Metelka
 */
public class JavaFXParserResultImpl {

    // -J-Dorg.netbeans.api.javafx.source.parsing.JavaFXParserResult.level=FINEST
    private static final Logger LOG = Logger.getLogger(JavaFXParserResultImpl.class.getName());

    private final AtomicBoolean invalid = new AtomicBoolean();

    private final JavaFXParser parser;

    private ElementUtilities elementUtilities;

    private TreeUtilities treeUtilities;

    JavaFXParserResultImpl(JavaFXParser parser) {
        this.parser = parser;
    }

    public Snapshot getSnapshot() {
        return parser.snapshot();
    }

    public ClasspathInfo getClasspathInfo() {
        return parser.classpathInfo();
    }

    public ElementUtilities getElementUtilities() {
        if (null == elementUtilities) {
            elementUtilities = ApiSourcePackageAccessor.get().createElementUtilities(this);
        }
        return elementUtilities;
    }

    public TreeUtilities getTreeUtilities() {
        if (treeUtilities == null) {
            treeUtilities = ApiSourcePackageAccessor.get().createTreeUtilities(this);
        }
        return treeUtilities;
    }

    public CompilationPhase getPhase() {
        return parser.currentPhase;
    }

    public CompilationPhase toPhase(CompilationPhase phase) throws IOException {
        assert (phase != null);
        if (parser.currentPhase.lessThan(phase)) {
            parser.toPhase(phase);
            if (parser.currentPhase.lessThan(phase)) {
                phase = parser.currentPhase;
            }
        }
        return phase;
    }

    public Context getContext() {
        return parser.javafxcTaskImpl.getContext();
    }

    public UnitTree getCompilationUnit() {
        if (parser.currentPhase.lessThan(CompilationPhase.PARSED))
            throw new IllegalStateException("Cannot call getCompilationInfo() if current phase < JavaFXSource.Phase.PARSED. You must call toPhase(Phase.PARSED) first.");//NOI18N
        return parser.compilationUnit;
    }

    public List<Diagnostic> getDiagnostics() {
        return parser.getDiagnostics();
    }

    public boolean isErrors() {
        return parser.isErrors();
    }

    public Iterable<? extends JavaFileObject> getClassBytes() {
        return parser.classBytes;
    }

    public JavafxcTaskImpl getJavafxcTaskImpl() {
        return parser.javafxcTaskImpl;
    }

    public Types getTypes() {
        return getJavafxcTaskImpl().getTypes();
    }

    public JavafxTypes getJavafxTypes() {
        return JavafxTypes.instance(getContext());
    }

    public Elements getElements() {
        return getJavafxcTaskImpl().getElements();
    }

    public JavafxcTrees getTrees() {
        return JavafxcTrees.instance(getJavafxcTaskImpl());
    }

    public TokenHierarchy getTokenHierarchy() {
        return getSnapshot().getTokenHierarchy();
    }

    public boolean isValid() {
        return !invalid.get();
    }

    public void invalidate() {
        invalid.set(true);
    }

}
