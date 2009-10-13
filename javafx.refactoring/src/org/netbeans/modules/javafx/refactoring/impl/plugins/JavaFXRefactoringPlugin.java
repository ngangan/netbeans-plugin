/*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 *  Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 * 
 *  The contents of this file are subject to the terms of either the GNU
 *  General Public License Version 2 only ("GPL") or the Common
 *  Development and Distribution License("CDDL") (collectively, the
 *  "License"). You may not use this file except in compliance with the
 *  License. You can obtain a copy of the License at
 *  http://www.netbeans.org/cddl-gplv2.html
 *  or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 *  specific language governing permissions and limitations under the
 *  License.  When distributing the software, include this License Header
 *  Notice in each file and include the License file at
 *  nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Sun in the GPL Version 2 section of the License file that
 *  accompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 * 
 *  Contributor(s):
 * 
 *  Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.refactoring.impl.plugins;

import java.io.IOException;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.modules.refactoring.api.Problem;
import org.netbeans.modules.refactoring.spi.ProgressProviderAdapter;
import org.netbeans.modules.refactoring.spi.RefactoringPlugin;

/**
 *
 * @author Jaroslav Bachorik
 */
abstract public class JavaFXRefactoringPlugin extends ProgressProviderAdapter implements RefactoringPlugin {
    private static interface CheckRunner {
        Problem check(CompilationInfo info);
    }

    final private Object sourceInitLock = new Object();
    // @GuardedBy sourceInitLock
    volatile private JavaFXSource source = null;
    // @GuardedBy sourceInitLock
    volatile private ClassIndex classIndex = null;

    final public Problem preCheck() {
        return runCheck(new CheckRunner() {

            public Problem check(CompilationInfo info) {
                return preCheck(info);
            }
        });
    }

    final public Problem checkParameters() {
        return runCheck(new CheckRunner() {

            public Problem check(CompilationInfo info) {
                return checkParameters(info);
            }
        });
    }

    final public Problem fastCheckParameters() {
        return runCheck(new CheckRunner() {

            public Problem check(CompilationInfo info) {
                return fastCheckParameters(info);
            }
        });
    }

    /**
     * Will take care of providing a correct {@linkplain JavaFXSource} for 
     * the element being refactored
     * @return Returns an initialized {@linkplain JavaFXSource} instance
     */
    abstract protected JavaFXSource prepareSource();
    abstract protected Problem preCheck(CompilationInfo info);
    abstract protected Problem checkParameters(CompilationInfo info);
    abstract protected Problem fastCheckParameters(CompilationInfo info);

    /**
     * Cached {@linkplain ClassIndex} instance corresponding to the
     * {@linkplain JavaFXSource} instance of the element to be refactored<br>
     * It is much faster than obtaining the class index from the same 
     * {@linkplain ClasspathInfo} repeatedly
     * @return Returns the initialized instance of {@linkplain ClassIndex}
     */
    final protected ClassIndex getClassIndex() {
        initSource();
        return classIndex;
    }

    /**
     * Cached {@linkplain JavaFXSource} instance representing the source of
     * the element to be refactored<br>
     * This is more efficient than creating the instance for the same source
     * file repeatedly.
     * @return Returns the initialized instance of {@linkplain JavaFXSource}
     */
    final protected JavaFXSource getSource() {
        initSource();
        return source;
    }

    protected ClassIndex prepareIndex() {
        return null;
    }

    protected static final Problem createProblem(Problem result, boolean isFatal, String message) {
        Problem problem = new Problem(isFatal, message);
        if (result == null) {
            return problem;
        } else if (isFatal) {
            problem.setNext(result);
            return problem;
        } else {
            //problem.setNext(result.getNext());
            //result.setNext(problem);

            // [TODO] performance
            Problem p = result;
            while (p.getNext() != null)
                p = p.getNext();
            p.setNext(problem);
            return result;
        }
    }

    private Problem runCheck(final CheckRunner runner) {
        final Problem[] problem = new Problem[1];
        initSource();
        
        try {
            source.runUserActionTask(new Task<CompilationController>() {
                public void run(CompilationController cc) throws Exception {
                    problem[0] = runner.check(cc);
                }
            }, true);
        } catch (IOException e) {
            problem[0] = new Problem(true, e.getLocalizedMessage() != null ? e.getLocalizedMessage() : "");
        }
        return problem[0];
    }

    private void initSource() {
        synchronized(sourceInitLock) {
            if (source == null) {
                source = prepareSource();
                classIndex = prepareIndex();
                classIndex = (classIndex == null) ? (source != null ? source.getClasspathInfo().getClassIndex() : null) : null;
            }
        }
    }


}
