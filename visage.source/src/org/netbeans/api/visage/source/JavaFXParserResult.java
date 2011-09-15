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

package org.netbeans.api.visage.source;

import com.sun.visage.api.tree.UnitTree;
import com.sun.tools.mjavac.util.Context;
import com.sun.tools.visage.api.JavafxcTaskImpl;
import com.sun.tools.visage.api.JavafxcTool;
import com.sun.tools.visage.api.JavafxcTrees;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import javax.tools.Diagnostic;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import org.netbeans.modules.visage.source.ApiSourcePackageAccessor;
import org.netbeans.modules.visage.source.parsing.VisageParserResultImpl;
import org.netbeans.modules.visage.source.parsing.VisageParserTask;
import org.netbeans.modules.parsing.api.ParserManager;
import org.netbeans.modules.parsing.api.Source;
import org.netbeans.modules.parsing.spi.ParseException;
    import org.netbeans.modules.parsing.spi.Parser.Result;

/**
 * Parser result allows to obtain FX-specific data about parsed source.
 *
 * @author Miloslav Metelka
 */
public final class VisageParserResult extends Result {

    static {
        ApiSourcePackageAccessor.set(new Accessor());
    }

    /**
     * Create result. {@link #toPhase(org.netbeans.api.visage.source.CompilationPhase)}
     * may be used to move to a concrete phase.
     *
     * @param snapshot non-null snapshot to be parsed.
     * @param classpathInfo class path info or null to use default class path.
     * @return non-null result
     */
    public static VisageParserResult create(Source source, ClasspathInfo classpathInfo) throws ParseException {
        VisageParserTask task = new VisageParserTask(classpathInfo);
        ParserManager.parse(Collections.singletonList(source), task);
        return task.result();
    }

    /**
     * Minimum priority that should be returned by <code>task.getPriority()</code>
     * if it requires attribute analysis.
     */
    public static final int ANALYZED_PRIORITY = 2000;

    private VisageParserResultImpl impl;

    VisageParserResult(VisageParserResultImpl impl) {
        super(impl.getSnapshot());
        this.impl = impl;
    }

    public ClasspathInfo getClasspathInfo() {
        return impl.getClasspathInfo();
    }

    public CompilationPhase getPhase() {
        return impl.getPhase();
    }

    public CompilationPhase toPhase(CompilationPhase phase) throws IOException {
        return impl.toPhase(phase);
    }

    public Context getContext() {
        return impl.getContext();
    }

    public UnitTree getCompilationUnit() {
        return impl.getCompilationUnit();
    }

    public ElementUtilities getElementUtilities() {
        return impl.getElementUtilities();
    }

    public TreeUtilities getTreeUtilities() {
        return impl.getTreeUtilities();
    }

    public List<Diagnostic> getDiagnostics() {
        return impl.getDiagnostics();
    }

    public boolean isErrors() {
        return impl.isErrors();
    }

    public Iterable<? extends JavaFileObject> getClassBytes() {
        return impl.getClassBytes();
    }

    public JavafxcTaskImpl getJavafxcTaskImpl() {
        return impl.getJavafxcTaskImpl();
    }

    public JavafxcTrees getTrees() {
        return impl.getTrees();
    }

    public boolean isValid() {
        return impl.isValid();
    }

    @Override
    protected void invalidate() {
        impl.invalidate();
    }

    VisageParserResultImpl impl() {
        return impl;
    }

    private static final class Accessor extends ApiSourcePackageAccessor {

        @Override
        public VisageParserResult createResult(VisageParserResultImpl impl) {
            return new VisageParserResult(impl);
        }

        @Override
        public JavaFileManager getFileManager(ClasspathInfo cpInfo, JavafxcTool tool) {
            return cpInfo.getFileManager(tool);
        }

        @Override
        public ElementUtilities createElementUtilities(VisageParserResultImpl resultImpl) {
            return new ElementUtilities(resultImpl);
        }

        @Override
        public TreeUtilities createTreeUtilities(VisageParserResultImpl resultImpl) {
            return new TreeUtilities(resultImpl);
        }

        @Override
        public void registerSourceTaskFactoryManager() {
            VisageSourceTaskFactoryManager.register();
        }

    }
}
