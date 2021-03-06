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

import org.netbeans.api.visage.lexer.VisageTokenId;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.visage.editor.completion.VisageCompletionEnvironment;

import javax.lang.model.element.Modifier;
import static javax.lang.model.element.Modifier.*;
import java.io.IOException;
import java.util.EnumSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.visage.api.tree.ModifiersTree;
import org.visage.api.tree.Tree;
import org.visage.api.tree.VisageTreePath;

/**
 *
 * @author David Strupl
 */
public class ModifiersTreeEnvironment extends VisageCompletionEnvironment<ModifiersTree> {
    
    private static final Logger logger = Logger.getLogger(ModifiersTreeEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    @Override
    protected void inside(ModifiersTree t) throws IOException {
        if (LOGGABLE) log("inside ModifiersTree " + t); // NOI18N
        ModifiersTree mods = t;
        Set<Modifier> m = EnumSet.noneOf(Modifier.class);
        final TokenSequence<VisageTokenId> ts = getController().getTreeUtilities().tokensFor(mods, getSourcePositions());
        VisageTokenId lastNonWhitespaceTokenId = null;
        while (ts.moveNext() && ts.offset() < offset) {
            lastNonWhitespaceTokenId = ts.token().id();
            switch (lastNonWhitespaceTokenId) {
                case PUBLIC:
                    m.add(PUBLIC);
                    break;
                case PROTECTED:
                    m.add(PROTECTED);
                    break;
                case PRIVATE:
                    m.add(PRIVATE);
                    break;
                case STATIC:
                    m.add(STATIC);
                    break;
                case ABSTRACT:
                    m.add(ABSTRACT);
                    break;
            }
        }
        VisageTreePath parentPath = path.getParentPath();
        Tree parent = parentPath.getLeaf();
        VisageTreePath grandParentPath = parentPath.getParentPath();
        Tree grandParent = grandParentPath != null ? grandParentPath.getLeaf() : null;
        if (parent.getVisageKind() == Tree.VisageKind.CLASS_DECLARATION) {
            addAccessModifiers(m);
            addClassModifiers(m);
        } else if (parent.getVisageKind() != Tree.VisageKind.VARIABLE || grandParent == null || grandParent.getVisageKind() == Tree.VisageKind.CLASS_DECLARATION) {
            addAccessModifiers(m);
            addVarAccessModifiers(m, false);
        } else if (parent.getVisageKind() == Tree.VisageKind.VARIABLE && grandParent.getVisageKind() == Tree.VisageKind.FUNCTION_DEFINITION) {
            // no access modifiers for local variables within a function
        } else {
            localResult(null);
            addKeywordsForStatement();
        }
    }   

    private static void log(String s) {
        if (LOGGABLE) {
            logger.fine(s);
        }
    }
}
