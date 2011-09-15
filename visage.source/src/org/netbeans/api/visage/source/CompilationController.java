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

import org.netbeans.modules.visage.source.CompilationInfoImpl;
import java.io.IOException;
import org.openide.util.Exceptions;

/**
 *
 * @author nenik
 */
public final class CompilationController extends CompilationInfo {

    public static CompilationController create(VisageParserResult result) {
        if (result == null)
            throw new IllegalArgumentException("Null result");
        CompilationInfoImpl impl = new CompilationInfoImpl(result.impl());
        return new CompilationController(impl);
    }

    CompilationController(CompilationInfoImpl impl) {
        super(impl);
    }

    /** Moves the state to required phase. If given state was already reached 
     * the state is not changed. The method will throw exception if a state is 
     * illegal required. Acceptable parameters for thid method are <BR>
     * <LI>{@link VisageSource.Phase.PARSED}
     * <LI>{@link VisageSource.Phase.ELEMENTS_RESOLVED}
     * <LI>{@link VisageSource.Phase.RESOLVED}
     * <LI>{@link VisageSource.Phase.UP_TO_DATE}   
     * @param phase The required phase
     * @return the reached state
     * @throws IllegalArgumentException in case that given state can not be 
     *         reached using this method
     * @throws IOException when the file cannot be red
     */    
    public VisageSource.Phase toPhase(VisageSource.Phase phase ) throws IOException {
        return impl().toPhase(phase);
    }
        
    public void runUserActionTask(final Task<? super CompilationController> task) throws IOException {
        if (task == null) {
            throw new IllegalArgumentException ("Task cannot be null");     //NOI18N
        }

        try {
            task.run(this);
        } catch (Exception ex) {
          // XXX better handling
          Exceptions.printStackTrace(ex);
        } finally {
        }
    }

    public void runWhenScanFinished(Task<CompilationController> task) throws IOException {
        if (task == null) {
            throw new IllegalArgumentException ("Task cannot be null");     //NOI18N
        }

        try {
            task.run(this);
        } catch (Exception ex) {
          // XXX better handling
          Exceptions.printStackTrace(ex);
        } finally {
        }

    }

}
