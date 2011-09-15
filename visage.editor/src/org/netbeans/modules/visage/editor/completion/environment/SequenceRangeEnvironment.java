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

package org.netbeans.modules.visage.editor.completion.environment;

import com.sun.tools.visage.tree.VSGSequenceRange;
import org.netbeans.api.visage.lexer.VSGTokenId;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.visage.editor.completion.VisageCompletionEnvironment;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.netbeans.api.lexer.TokenHierarchy;

/**
 * Env for range expressions like [1..10 step 2]
 *
 * @author Miloslav Metelka
 */
public class SequenceRangeEnvironment extends VisageCompletionEnvironment<VSGSequenceRange> {

    private static final Logger logger = Logger.getLogger(SequenceRangeEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    enum State {
        INIT,
        AFTER_RANGE_START,
        AFTER_DOT_DOT,
        AFTER_RANGE_END,
        AFTER_STEP;
    };

    @Override
    protected void inside(VSGSequenceRange t) throws IOException {
        int start = (int)sourcePositions.getStartPosition(root, t);
        TokenSequence<VSGTokenId> ts = ((TokenHierarchy<?>)controller.getTokenHierarchy()).tokenSequence(VSGTokenId.language());
        ts.move(start);
        State state = State.INIT;
        while (ts.moveNext() && ts.offset() <= offset) {
            switch (ts.token().id()) {
                case DECIMAL_LITERAL:
                case IDENTIFIER:
                    switch (state) {
                        case INIT:
                            state = State.AFTER_RANGE_START;
                            break;
                        case AFTER_DOT_DOT:
                            state = State.AFTER_RANGE_END;
                            break;
                    }
                    break;
                case DOTDOT:
                    state = State.AFTER_DOT_DOT; // Do not check for the current state
                    break;
                case STEP:
                    state = State.AFTER_STEP;
                    break;
            }
        }
        // Sequences may be [constantOrVar .. constantOrVar [step] constantOrVar]
        localResult(null);
        switch (state) {
            case AFTER_RANGE_END:
                addKeyword("step", " ", true); // NOI18N
                break;
        }
    }

}
