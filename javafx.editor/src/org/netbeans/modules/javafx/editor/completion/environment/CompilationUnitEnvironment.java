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
import com.sun.tools.javafx.tree.JFXFunctionDefinition;
import com.sun.tools.javafx.tree.JFXVar;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Modifier;
import javax.tools.Diagnostic;

import org.netbeans.api.javafx.editor.SafeTokenSequence;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.modules.javafx.editor.completion.JavaFXCompletionEnvironment;
import org.netbeans.modules.javafx.editor.completion.JavaFXCompletionItem;
import org.netbeans.modules.javafx.editor.completion.JavaFXCompletionProvider;
import static org.netbeans.modules.javafx.editor.completion.JavaFXCompletionQuery.*;

/**
 *
 * @author David Strupl
 */
public class CompilationUnitEnvironment extends JavaFXCompletionEnvironment<UnitTree> {
    
    private static final Logger logger = Logger.getLogger(CompilationUnitEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    @Override
    protected void inside(UnitTree ut) throws IOException {
        if (LOGGABLE) log("inside CompilationUnitTree " + ut); // NOI18N
        Tree pkg = root.getPackageName();
        boolean hasPublicDecls = hasPublicDeclarations(ut);
        if (pkg == null || offset <= sourcePositions.getStartPosition(root, root)) {
            addKeywordsForCU(ut);
            if (!hasPublicDecls) {
                addKeywordsForStatement();
            }
            return;
        }
        if (offset <= sourcePositions.getStartPosition(root, pkg)) {
            addPackages(getPrefix());
        } else {
            SafeTokenSequence<JFXTokenId> first = findFirstNonWhitespaceToken((int) sourcePositions.getEndPosition(root, pkg), offset);
            if (first != null && first.token().id() == JFXTokenId.SEMI) {
                addKeywordsForCU(ut);
                if (!hasPublicDecls) {
                    addKeywordsForStatement();
                    addKeyword(EXTENDS_KEYWORD, prefix, false);
                    addPackages(""); // NOI18N
                    addLocalAndImportedTypes(null, null, null, false, null);
                    addLocalMembersAndVars(null);
                    addLocalAndImportedFunctions();
                }
            }
        }
    }

    private boolean hasPublicDeclarations(UnitTree ut) {
        if (LOGGABLE) log("hasPublicDeclarations"); // NOI18N
        for (Tree tr : ut.getTypeDecls()) {
            if (tr.getJavaFXKind() == Tree.JavaFXKind.CLASS_DECLARATION) {
                JFXClassDeclaration cl = (JFXClassDeclaration)tr;
                if (LOGGABLE) log("   cl " + cl); // NOI18N
                JavaFXTreePath tp = JavaFXTreePath.getPath(root, cl);
                if (controller.getTreeUtilities().isSynthetic(tp)) {
                    if (LOGGABLE) log("       isSynthetic "); // NOI18N
                    for (Tree t : cl.getClassMembers()) {
                        if (LOGGABLE) log("   t == " + t); // NOI18N
                        if (t instanceof JFXFunctionDefinition) {
                            JFXFunctionDefinition fd = (JFXFunctionDefinition)t;
                            if (LOGGABLE) log("   fd == " + fd); // NOI18N
                            JavaFXTreePath fp = JavaFXTreePath.getPath(root, fd);
                            if (controller.getTreeUtilities().isSynthetic(fp)) {
                                if (LOGGABLE) log("  ignoring " + fd + " because it is syntetic"); // NOI18N
                                continue;
                            }
                            if (fd.getModifiers().getFlags().contains(Modifier.PUBLIC)) {
                                if (LOGGABLE) log("   returning true because of " + fd); // NOI18N
                                return true;
                            }
                        }
                        if (t instanceof JFXVar) {
                            JFXVar v = (JFXVar)t;
                            if (LOGGABLE) log("   v == " + v); // NOI18N
                            if (v.getModifiers().getFlags().contains(Modifier.PUBLIC)) {
                                if (LOGGABLE) log("   returning true because of " + v); // NOI18N
                                return true;
                            }
                        }
                        if (t instanceof JFXClassDeclaration) {
                            JFXClassDeclaration inner = (JFXClassDeclaration)t;
                            if (LOGGABLE) log("   inner == " + inner); // NOI18N
                            if (inner.getModifiers().getFlags().contains(Modifier.PUBLIC)) {
                                if (LOGGABLE) log("   returning true because of " + inner); // NOI18N
                                return true;
                            }
                        }
                    }
                } else {
                    if (cl.getModifiers().getFlags().contains(Modifier.PUBLIC)) {
                        if (LOGGABLE) log("   returning true because the class is public"); // NOI18N
                        return true;
                    }
                }
            }
        }
        if (LOGGABLE) log("hasPublicDeclarations returning false at the very end"); // NOI18N
        return false;
    }

    private void addKeywordsForCU(UnitTree ut) {
        addAccessModifiers(null);
        addVarAccessModifiers(null, true);
        addFunctionModifiers(null);
        addClassModifiers(null);
        addKeyword(CLASS_KEYWORD, SPACE, false);
        addKeyword(VAR_KEYWORD, SPACE, false);
        addKeyword(DEF_KEYWORD, SPACE, false);
        addKeyword(FUNCTION_KEYWORD, SPACE, false);
        addKeyword(IMPORT_KEYWORD, SPACE, false);
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
                addKeyword(PACKAGE_KEYWORD, SPACE, false);
            }
        }
    }

    
    private static void log(String s) {
        if (LOGGABLE) {
            logger.fine(s);
        }
    }
}
