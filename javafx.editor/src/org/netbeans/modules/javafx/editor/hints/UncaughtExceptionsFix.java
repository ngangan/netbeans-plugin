/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2008 Sun
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

package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.SourcePositions;
import java.util.Iterator;
import javax.swing.text.Document;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.modules.javafx.editor.hints.UncaughtExceptionsModel.Hint;
import org.netbeans.spi.editor.hints.ChangeInfo;
import org.netbeans.spi.editor.hints.Fix;
import org.openide.util.NbBundle;

/**
 *
 * @author karol harezlak
 */
class UncaughtExceptionsFix implements Fix {

    private Document document;
    private Hint hint;
    private CompilationInfo compilationInfo;

    public UncaughtExceptionsFix(Document document, Hint hint, CompilationInfo compilationInfo) {
        assert document != null;
        this.document = document;
        this.hint = hint;
        this.compilationInfo = compilationInfo;
    }

    @Override
    public String getText() {
        return NbBundle.getMessage(this.getClass(), "TITLE_UNCOUGHT_EXCEPTION"); //NOI18N
    }

    @Override
    public ChangeInfo implement() throws Exception {
        //TODO Unique ex var name
        String exceptionName = "ex";
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        Iterator iterator = hint.getExceptions().iterator();
        StringBuilder block = new StringBuilder();
        if (hint.getCatchTree() == null) {
            int start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), hint.getTree());
            int lenght = hint.getTree().toString().length() + 1;
            String method = document.getText(start, lenght);
            block.append("try {\n    ") //NOI18N
                    .append(method).append("\n}"); //NOI18N
            
            while (iterator.hasNext()) {
                //TODO Unique ex var name
                block.append(" catch(").append(exceptionName).append(" : ").append(iterator.next()).append(") {\n") //NOI18N
                        .append("    ").append(exceptionName).append(".printStackTrace();\n") //NOI18N
                        .append("}"); //NOI18N
                if (!iterator.hasNext()) {
                    block.append("\n"); //NOI18N
                }
            }
            document.remove(start, lenght);
            document.insertString(start, block.toString(), null);
        } else {

            int end = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), hint.getCatchTree());
            while (iterator.hasNext()) {
                block.append(" catch(").append(exceptionName).append(" : ").append(iterator.next()).append(") {\n") //NOI18N
                        .append("    ").append(exceptionName).append(".printStackTrace();\n") //NOI18N
                        .append("}"); //NOI18N
                if (!iterator.hasNext()) {
                    block.append("\n"); //NOI18N
                }
            }
            document.insertString(end, block.toString(), null);
        }
        return null;
    }
}
