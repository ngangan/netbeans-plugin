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

    abstract protected JavaFXSource getSource();
    abstract protected Problem preCheck(CompilationInfo info);
    abstract protected Problem checkParameters(CompilationInfo info);
    abstract protected Problem fastCheckParameters(CompilationInfo info);

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
        JavaFXSource jfxs = getSource();

        try {
            jfxs.runUserActionTask(new Task<CompilationController>() {
                public void run(CompilationController cc) throws Exception {
                    problem[0] = runner.check(cc);
                }
            }, true);
        } catch (IOException e) {
            problem[0] = new Problem(true, e.getLocalizedMessage() != null ? e.getLocalizedMessage() : "");
        }
        return problem[0];
    }
}
