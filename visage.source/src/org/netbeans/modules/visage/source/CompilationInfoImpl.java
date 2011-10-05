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

package org.netbeans.modules.visage.source;

import com.sun.source.tree.CompilationUnitTree;
import com.sun.tools.mjavac.util.Context;
import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.visage.source.ClasspathInfo;
import org.netbeans.api.visage.source.VisageSource;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.modules.visage.source.parsing.VisageParserResultImpl;
import org.netbeans.modules.parsing.api.Snapshot;
import org.visage.api.tree.UnitTree;
import org.visage.tools.api.VisagecTaskImpl;

/**
 * Don't use! Should be private, needs some repackaging....
 * @author nenik
 */
public class CompilationInfoImpl {

    static final Logger LOGGER = Logger.getLogger(CompilationInfoImpl.class.getName());
    
    //final VisageSource source;
    private final VisageParserResultImpl parserResultImpl;

    public CompilationInfoImpl(VisageParserResultImpl parserResult) {
        this.parserResultImpl = parserResult;
    }

    public ClasspathInfo getClasspathInfo() {
        return parserResultImpl.getClasspathInfo();
    }

    /**
     * Returns the current phase of the {@link VisageSource}.
     * @return {@link VisageSource.Phase} the state which was reached by the {@link VisageSource}.
     */
    public VisageSource.Phase getPhase() {
        return VisageSource.Phase.from(parserResultImpl.getPhase());
    }

    /**
     * Returns the visagec tree representing the source file.
     * @return {@link CompilationUnitTree} the compilation unit containing
     * the top level classes contained in the, visage source file.
     * 
     * @throws java.lang.IllegalStateException  when the phase is less than {@link VisageSource.Phase#PARSED}
     */
    public UnitTree getCompilationUnit() {
        return parserResultImpl.getCompilationUnit();
    }

    public VisagecTaskImpl getVisagecTaskImpl() {
        return parserResultImpl.getVisagecTaskImpl();
    }
    
    public TokenHierarchy getTokenHierarchy() {
        return parserResultImpl.getSnapshot().getTokenHierarchy();
    }

    public Snapshot getSnapshot() {
        return parserResultImpl.getSnapshot();
    }
    
    /** Moves the state to required phase. If given state was already reached 
     * the state is not changed. The method will throw exception if a state is 
     * illegal required. Acceptable parameters for thid method are <BR>
     * <LI>{@link org.netbeans.api.java.source.JavaSource.Phase.PARSED}
     * <LI>{@link org.netbeans.api.java.source.JavaSource.Phase.ELEMENTS_RESOLVED}
     * <LI>{@link org.netbeans.api.java.source.JavaSource.Phase.RESOLVED}
     * <LI>{@link org.netbeans.api.java.source.JavaSource.Phase.UP_TO_DATE}   
     * @param phase The required phase
     * @return the reached state
     * @throws IllegalArgumentException in case that given state can not be 
     *         reached using this method
     * @throws IOException when the file cannot be red
     */    
    public VisageSource.Phase toPhase(VisageSource.Phase phase) throws IOException {
        return VisageSource.Phase.from(parserResultImpl.toPhase(phase.toCompilationPhase()));
    }

    public Context getContext() {
        return parserResultImpl.getContext();
    }

    /**
     * Returns the errors in the file represented by the {@link JavaSource}.
     * @return an list of {@link Diagnostic} 
     */
    public List<Diagnostic> getDiagnostics() {
        return parserResultImpl.getDiagnostics();
    }
    
    public boolean isErrors() {
        return parserResultImpl.isErrors();
    }

    public Iterable<? extends JavaFileObject> getClassBytes() {
        return parserResultImpl.getClassBytes();
    }

    public VisageParserResultImpl parserResultImpl() {
        return parserResultImpl;
    }

}
