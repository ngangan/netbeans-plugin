/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
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

package org.netbeans.modules.javafx.editor.completion;

import com.sun.javafx.api.tree.JavaFXTreePath;
import java.io.IOException;
import java.util.*;
import java.util.logging.Logger;
import java.util.logging.Level;
import javax.swing.text.AbstractDocument;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import com.sun.javafx.api.tree.Tree;
import org.netbeans.api.editor.EditorRegistry;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.*;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.spi.editor.completion.*;
import org.netbeans.spi.editor.completion.support.AsyncCompletionTask;

/**
 *
 * @author Dusan Balek
 */
public class JavaFXCompletionProvider implements CompletionProvider {
    
    private static final Logger logger = Logger.getLogger(JavaFXCompletionProvider.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);
    
    private static final boolean autoMode = Boolean.getBoolean("org.netbeans.modules.editor.java.completionAutoMode"); // NOI18N
    private static final String ERROR = "<error>"; //NOI18N
    
    public int getAutoQueryTypes(JTextComponent component, String typedText) {
        if (".".equals(typedText) || (autoMode && JavaFXCompletionQuery.isJavaIdentifierPart(typedText))) { // NOI18N
            if (isJavaFXContext(component, component.getSelectionStart() - 1))
                return COMPLETION_QUERY_TYPE;
        }
        return 0;
    }
    
    public static boolean startsWith(String theString, String prefix) {
        if (theString == null || theString.length() == 0 || ERROR.equals(theString))
            return false;
        if (prefix == null || prefix.length() == 0)
            return true;
        return theString.toUpperCase().startsWith(prefix.toUpperCase());
    }
    
    public static JavaFXTreePath getPathElementOfKind(Tree.JavaFXKind kind, JavaFXTreePath path) {
        return getPathElementOfKind(EnumSet.of(kind), path);
    }
    
    public static JavaFXTreePath getPathElementOfKind(EnumSet<Tree.JavaFXKind> kinds, JavaFXTreePath path) {
        while (path != null) {
            if (kinds.contains(path.getLeaf().getJavaFXKind()))
                return path;
            path = path.getParentPath();
        }
        return null;        
    }        

    @SuppressWarnings("fallthrough")
    public static boolean isJavaFXContext(final JTextComponent component, final int offset) {
        Document doc = component.getDocument();
        if (doc instanceof AbstractDocument) {
            ((AbstractDocument)doc).readLock();
        }
        try {
            TokenSequence<JFXTokenId> ts = getJavaFXTokenSequence(TokenHierarchy.get(doc), offset, doc);
            if (ts == null) {
                return false;
            }
            if (!ts.moveNext() && !ts.movePrevious()) {
                return true;
            }
            if (offset == ts.offset()) {
                return true;
            }
            switch (ts.token().id()) {
                case FLOATING_POINT_LITERAL:
                    if (ts.token().text().charAt(0) == '.') { // NOI18N
                        break;
                    }
                case DOC_COMMENT:
                case STRING_LITERAL:
                case LINE_COMMENT:
                case COMMENT:
                    return false;
            }
            return true;
        } finally {
            if (doc instanceof AbstractDocument) {
                ((AbstractDocument) doc).readUnlock();
            }
        }
    }
    
    @SuppressWarnings("unchecked")
    public static TokenSequence<JFXTokenId> getJavaFXTokenSequence(final TokenHierarchy hierarchy, final int offset, final Document doc) {
        if (hierarchy != null) {
            TokenSequence<?> ts_ = hierarchy.tokenSequence();
            while(ts_ != null && ts_.isValid() && (offset == 0 || ts_.moveNext())) {
                ts_.move(offset);
                if (ts_.language() == JFXTokenId.language()) {
                    return (TokenSequence<JFXTokenId>)ts_;
                }
                if (!ts_.moveNext() && !ts_.movePrevious()) {
                    if (LOGGABLE) log("getJavaFXTokenSequence returning null (1) for offset " + offset); // NOI18N
                    return null;
                }
                ts_ = ts_.embedded();
            }
        }
        if (LOGGABLE) log("getJavaFXTokenSequence returning null (2) for offset " + offset); // NOI18N
        return null;
    }
    
    public CompletionTask createTask(int type, JTextComponent component) {
        if ((type & COMPLETION_QUERY_TYPE) != 0 || type == TOOLTIP_QUERY_TYPE || type == DOCUMENTATION_QUERY_TYPE)
            return new AsyncCompletionTask(new JavaFXCompletionQuery(type, component.getSelectionStart(), true), component);
        return null;
    }
    
    static CompletionTask createDocTask(ElementHandle element) {
        JavaFXCompletionQuery query = new JavaFXCompletionQuery(DOCUMENTATION_QUERY_TYPE, -1, true);
        query.setElement(element);
        return new AsyncCompletionTask(query, EditorRegistry.lastFocusedComponent());
    }

    public static Set<? extends CompletionItem> query(JavaFXSource source, int queryType, int offset, int substitutionOffset) throws IOException {
        assert source != null;
        assert (queryType & COMPLETION_QUERY_TYPE) != 0;
        JavaFXCompletionQuery query = new JavaFXCompletionQuery(queryType, offset, false);
        source.runUserActionTask(query, false);
        if (offset != substitutionOffset) {
            for (JavaFXCompletionItem jci : query.results) {
                jci.substitutionOffset += (substitutionOffset - offset);
            }
        }
        return query.results;
    }
    
    private static void log(String s) {
        if (LOGGABLE) {
            logger.fine(s);
        }
    }
    
}
