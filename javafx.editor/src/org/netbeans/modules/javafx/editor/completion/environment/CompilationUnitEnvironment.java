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

package org.netbeans.modules.javafx.editor.completion.environment;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.UnitTree;
import com.sun.tools.javafx.tree.JFXClassDeclaration;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import javax.lang.model.element.Modifier;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeMirror;
import javax.tools.Diagnostic;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.javafx.editor.completion.JavaFXCompletionEnvironment;
import org.netbeans.modules.javafx.editor.completion.JavaFXCompletionItem;
import org.netbeans.modules.javafx.editor.completion.JavaFXCompletionProvider;
import static org.netbeans.modules.javafx.editor.completion.JavaFXCompletionQuery.*;

/**
 *
 * @author David Strupl
 */
public class CompilationUnitEnvironment extends JavaFXCompletionEnvironment<UnitTree> {
    
    private static final Logger logger = Logger.getLogger(FunctionDefinitionEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    @Override
    protected void inside(UnitTree ut) throws IOException {
        if (LOGGABLE) log("inside CompilationUnitTree " + ut);
        Tree pkg = root.getPackageName();
        if (pkg == null || offset <= sourcePositions.getStartPosition(root, root)) {
            addKeywordsForCU(ut);
            return;
        }
        if (offset <= sourcePositions.getStartPosition(root, pkg)) {
            addPackages(getPrefix());
        } else {
            TokenSequence<JFXTokenId> first = findFirstNonWhitespaceToken((int) sourcePositions.getEndPosition(root, pkg), offset);
            if (first != null && first.token().id() == JFXTokenId.SEMI) {
                addKeywordsForCU(ut);
                if (!hasPublicDeclarations(ut)) {
                    addPackages("");
                    addLocalAndImportedTypes(null, null, null, false, null);
                    addLocalMembersAndVars(null);
                    addLocalAndImportedFunctions();
                }
            }
        }
    }

    private boolean hasPublicDeclarations(UnitTree ut) {
        if (LOGGABLE) log("hasPublicDeclarations");
        for (Tree tr : ut.getTypeDecls()) {
            if (tr.getJavaFXKind() == Tree.JavaFXKind.CLASS_DECLARATION) {
                JFXClassDeclaration cl = (JFXClassDeclaration)tr;
                if (LOGGABLE) log("   cl " + cl);
                JavaFXTreePath tp = JavaFXTreePath.getPath(root, cl);
                TypeMirror tm = controller.getTrees().getTypeMirror(tp);
                DeclaredType dt = (DeclaredType)tm;
                Element e = dt.asElement();
                for (Element ele : e.getEnclosedElements()) {
                    if (LOGGABLE) log("   ele " + ele);
                    if (ele.getSimpleName().toString().equals("run")) {
                        if (LOGGABLE) log("   returning true because of run found " + ele.getSimpleName());
                        return true;
                    }
                    if (ele.getModifiers().contains(Modifier.PUBLIC)) {
                        if (!ele.getSimpleName().toString().contains("$")) {
                            if (LOGGABLE) log("   returning true because of " + ele.getSimpleName());
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    private void addKeywordsForCU(UnitTree ut) {
        List<String> kws = new ArrayList<String>();
        kws.add(ABSTRACT_KEYWORD);
        kws.add(CLASS_KEYWORD);
        kws.add(VAR_KEYWORD);
        kws.add(DEF_KEYWORD);
        kws.add(FUNCTION_KEYWORD);
        kws.add(PUBLIC_KEYWORD);
        kws.add(IMPORT_KEYWORD);
        boolean beforeAnyClass = true;
        for (Tree t : root.getTypeDecls()) {
            if (t.getJavaFXKind() == Tree.JavaFXKind.CLASS_DECLARATION) {
                int pos = (int) sourcePositions.getEndPosition(root, t);
                if (pos != Diagnostic.NOPOS && offset >= pos) {
                    beforeAnyClass = false;
                }
            }
        }
        if (beforeAnyClass) {
            Tree firstImport = null;
            for (Tree t : root.getImports()) {
                firstImport = t;
                break;
            }
            Tree pd = root.getPackageName();
            if ((pd != null && offset <= sourcePositions.getStartPosition(root, root)) || (pd == null && (firstImport == null || sourcePositions.getStartPosition(root, firstImport) >= offset))) {
                kws.add(PACKAGE_KEYWORD);
            }
        }
        for (String kw : kws) {
            if (JavaFXCompletionProvider.startsWith(kw, prefix)) {
                addResult(JavaFXCompletionItem.createKeywordItem(kw, SPACE, query.anchorOffset, false));
            }
        }
        if (!hasPublicDeclarations(ut)) {
            addKeywordsForStatement();
        }
    }

    
    private static void log(String s) {
        if (LOGGABLE) {
            logger.fine(s);
        }
    }
}
