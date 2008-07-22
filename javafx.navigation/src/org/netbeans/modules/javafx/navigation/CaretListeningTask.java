/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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
package org.netbeans.modules.javafx.navigation;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.Tree.JavaFXKind;
import com.sun.source.util.TreePath;
import java.io.IOException;
import java.util.EnumSet;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.swing.SwingUtilities;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.JavaFXSource.Phase;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.lexer.TokenId;
import org.netbeans.api.lexer.TokenSequence;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;

/**
 * This task is called every time the caret position changes in a Java editor.
 * <p>
 * The task finds the TreePath of the Tree under the caret, converts it to
 * an Element and then shows the declartion of the element in Declaration window
 * and javadoc in the Javadoc window.
 *
 * @author Sandip V. Chitale (Sandip.Chitale@Sun.Com)
 */
public class CaretListeningTask implements CancellableTask<CompilationInfo> {

    private CaretListeningFactory caretListeningFactory;
    private FileObject fileObject;
    private boolean canceled;
    private static Element lastEhForNavigator;
    private static final Set<JFXTokenId> TOKENS_TO_SKIP = EnumSet.of(
            JFXTokenId.WS,
            JFXTokenId.COMMENT,
            JFXTokenId.LINE_COMMENT);

    CaretListeningTask(CaretListeningFactory whichElementJavaSourceTaskFactory, FileObject fileObject) {
        this.caretListeningFactory = whichElementJavaSourceTaskFactory;
        this.fileObject = fileObject;
    }

    public void run(CompilationInfo compilationInfo) {
        resume();

        boolean navigatorShouldUpdate = ClassMemberPanel.getInstance() != null; // XXX set by navigator visible
        if (isCancelled()) {
            return;
        }

        int lastPosition = CaretListeningFactory.getLastPosition(fileObject);

        TokenHierarchy tokens = compilationInfo.getTokenHierarchy();
        TokenSequence ts = tokens.tokenSequence();
        int offset = ts.move(lastPosition);
        if (ts.moveNext() && ts.token() != null) {
            Token token = ts.token();
            TokenId tid = token.id();

            if (shouldGoBack(token.toString(), offset < 0 ? 0 : offset)) {
                if (ts.movePrevious()) {
                    token = ts.token();
                    tid = token.id();
                }
            }

            if (TOKENS_TO_SKIP.contains(tid)) {
                skipTokens(ts, TOKENS_TO_SKIP);
            }
            lastPosition = ts.offset();
        }

        if (ts.token() != null && ts.token().length() > 1) {
            // it is magic for TreeUtilities.pathFor to proper tree
            ++lastPosition;
        }

        try {
            // TODO dirty hack
            ((CompilationController) compilationInfo).toPhase(Phase.ANALYZED);
        } catch (IOException ex) {
            Exceptions.printStackTrace(ex);
        }

        // Find the TreePath for the caret position
        JavaFXTreePath tp = compilationInfo.getTreeUtilities().pathFor(lastPosition);
        // if cancelled, return
        if (isCancelled()) {
            return;
        }

        // Update the navigator
        if (navigatorShouldUpdate) {
            updateNavigatorSelection(compilationInfo, tp);
        }

    }

    /**
     * After this method is called the task if running should exit the run
     * method immediately.
     */
    public final synchronized void cancel() {
        canceled = true;
    }

    protected final synchronized boolean isCancelled() {
        return canceled;
    }

    protected final synchronized void resume() {
        canceled = false;
    }

    private void updateNavigatorSelection(CompilationInfo ci, JavaFXTreePath tp) {
        // Try to find the declaration we are in
        final Element e = outerElement(ci, tp);

        if (e != null) {
            if (lastEhForNavigator != null && e.getSimpleName().equals(lastEhForNavigator.getSimpleName())) {
                return;
            }

            lastEhForNavigator = e;
            SwingUtilities.invokeLater(new Runnable() {

                public void run() {
                    final ClassMemberPanel cmp = ClassMemberPanel.getInstance();
                    if (cmp != null) {
                        cmp.selectElement(e);
                    }
                }
            });
        }

    }

    private static Element outerElement(CompilationInfo ci, JavaFXTreePath tp) {
        Element e = null;

        while (tp != null) {
            final Tree leaf = tp.getLeaf();
            switch (leaf.getJavaFXKind()) {
                case CLASS_DECLARATION:
                case FUNCTION_DEFINITION:
                case COMPILATION_UNIT:
                    e = ci.getTrees().getElement(tp);
                    break;
                case VARIABLE:
                    e = ci.getTrees().getElement(tp);

                    if (SpaceMagicUtils.hasSpiritualInvocation(e)) {
                        return e;
                    }

                    if (e != null && !e.getKind().isField()) {
                        e = null;
                    }
                    break;
            }
            if (e != null) {
                break;
            }
            tp = tp.getParentPath();
        }

        return e;
    }

    private void skipTokens(TokenSequence ts, Set<JFXTokenId> typesToSkip) {

        while (ts.moveNext()) {
            if (!typesToSkip.contains(ts.token().id())) {
                return;
            }
        }

        return;
    }

    private boolean shouldGoBack(String s, int offset) {

        int nlBefore = 0;
        int nlAfter = 0;

        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) == '\n') { // NOI18N
                if (i < offset) {
                    nlBefore++;
                } else {
                    nlAfter++;
                }

                if (nlAfter > nlBefore) {
                    return true;
                }
            }
        }

        if (nlBefore < nlAfter) {
            return false;
        }

        return offset < (s.length() - offset);

    }
}
