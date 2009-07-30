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

package org.netbeans.modules.javafx.source;

import com.sun.javafx.api.tree.UnitTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javafx.api.JavafxcTaskImpl;
import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.modules.javafx.source.parsing.JavaFXParserResultImpl;
import org.netbeans.modules.parsing.api.Snapshot;

/**
 * Don't use! Should be private, needs some repackaging....
 * @author nenik
 */
public class CompilationInfoImpl {

    static final Logger LOGGER = Logger.getLogger(CompilationInfoImpl.class.getName());
    
    //final JavaFXSource source;
    private final JavaFXParserResultImpl parserResultImpl;

    public CompilationInfoImpl(JavaFXParserResultImpl parserResult) {
        this.parserResultImpl = parserResult;
    }

    public ClasspathInfo getClasspathInfo() {
        return parserResultImpl.getClasspathInfo();
    }

    /**
     * Returns the current phase of the {@link JavaFXSource}.
     * @return {@link JavaFXSource.Phase} the state which was reached by the {@link JavaFXSource}.
     */
    public JavaFXSource.Phase getPhase() {
        return JavaFXSource.Phase.from(parserResultImpl.getPhase());
    }

    /**
     * Returns the javafxc tree representing the source file.
     * @return {@link CompilationUnitTree} the compilation unit containing
     * the top level classes contained in the, javafx source file.
     * 
     * @throws java.lang.IllegalStateException  when the phase is less than {@link JavaFXSource.Phase#PARSED}
     */
    public UnitTree getCompilationUnit() {
        return parserResultImpl.getCompilationUnit();
    }

    public JavafxcTaskImpl getJavafxcTaskImpl() {
        return parserResultImpl.getJavafxcTaskImpl();
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
    public JavaFXSource.Phase toPhase(JavaFXSource.Phase phase) throws IOException {
        return JavaFXSource.Phase.from(parserResultImpl.toPhase(phase.toCompilationPhase()));
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

    public JavaFXParserResultImpl parserResultImpl() {
        return parserResultImpl;
    }

}
