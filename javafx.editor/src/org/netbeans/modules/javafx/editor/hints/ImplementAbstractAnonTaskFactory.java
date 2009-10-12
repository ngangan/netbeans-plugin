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

import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.tools.javac.code.Symbol.MethodSymbol;
import com.sun.tools.javafx.code.JavafxClassSymbol;
import java.util.*;
import javax.lang.model.element.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.openide.util.Exceptions;

/**
 *
 * @author karol harezlak
 */
public final class ImplementAbstractAnonTaskFactory extends AbstractOverrideTask {

    @Override
    protected Tree getTree(CompilationInfo compilationInfo, Element currentClass, Map<Element, Tree> position) {
        Tree currentTree = position.get(currentClass);
        if (findPositionAtTheEnd(compilationInfo, currentTree) < 0) {
            return null;
        }
        return currentTree;
    }

    @Override
    protected int findPositionAtTheEnd(CompilationInfo compilationInfo, Tree tree) {
        SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
        int start = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), tree);
        int end = (int) sourcePositions.getEndPosition(compilationInfo.getCompilationUnit(), tree);
        int length = end - start;
        if (start < 0 || end < 0) {
            return -1;
        }
        Document document = compilationInfo.getDocument();
        if (document == null) {
            return -1;
        }
        try {
            String text = document.getText(start, length);
            int index = text.indexOf("{"); //NOI18N
            if (index > 0 ) {
                return start + index + 1;
            } else {
                return -1;
            }
        } catch (BadLocationException ex) {
            Exceptions.printStackTrace(ex);
        }
        return -1;
    }

    @Override
    protected JavaFXTreePathScanner<Void, Void> getVisitor(CompilationInfo compilationInfo, Map<Element, Collection<Tree>> classTrees, Map<Element, List<MethodSymbol>> overridenMethods, Collection<JavafxClassSymbol> imports, Map<Element, Tree> position) {
        return new OverrideAnonVisitor(compilationInfo, classTrees, overridenMethods, imports, position);
    }

    @Override
    protected String getHintsControllerString() {
        return "AnonOverride"; //NOI18N
    }
}
