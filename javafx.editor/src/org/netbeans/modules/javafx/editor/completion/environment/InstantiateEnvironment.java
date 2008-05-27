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

import com.sun.tools.javafx.tree.JFXInstanciate;
import java.io.IOException;
import java.util.Collections;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.ArrayType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.javafx.editor.completion.JavaFXCompletionEnvironment;

/**
 *
 * @author David Strupl
 */
public class InstantiateEnvironment extends JavaFXCompletionEnvironment<JFXInstanciate> {
    
    private static final Logger logger = Logger.getLogger(InstantiateEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    @Override
    protected void inside(JFXInstanciate it) throws IOException {
        int pos = (int) sourcePositions.getStartPosition(root, it);
        log("inside JFXInstanciate " + it + " pos == " + pos + "  offset == " + offset + "  prefix == " + prefix + "\n");
        if (pos < 0) {
            return;
        }
        TokenSequence<JFXTokenId> last = findLastNonWhitespaceToken(pos, offset);
        log("  last == " + last.token().id());
        if (last != null && last.token().id() == JFXTokenId.NEW) {
            addLocalAndImportedTypes(null, null, null, false);            
        } else {
            String s = it.getIdentifier().toString();
            log("  s == " + s);
            TypeElement te = findTypeElement(s);
            log("  te == " + te);
            if (te == null) {
                return;
            }
            TypeMirror tm = te.asType();
            log("  tm == " + tm + " ---- tm.getKind() == " + (tm == null ? "" : tm.getKind()));
            if (tm == null) {
                return;
            }
            addMembers(tm, false, true);
        }
    }

    @Override
    public Set<? extends TypeMirror> getSmartTypes() throws IOException {
        final JFXInstanciate it = (JFXInstanciate) path.getLeaf();
//        final TreePath treePath = new TreePath(path, it.getTree());
//        TypeMirror type = controller.getTrees().getTypeMirror(treePath);

        String s = it.getIdentifier().toString();
        TypeElement te = findTypeElement(s);
        TypeMirror type = te != null ? te.asType() : null;
        
        if (type == null) {
            return null;
        }
        
        int dim = 0;
        while (dim-- > 0) {
            if (type.getKind() == TypeKind.ARRAY) {
                type = ((ArrayType) type).getComponentType();
            } else {
                return null;
            }
        }
        
        return type != null ? Collections.singleton(type) : null;
    }

    private static void log(String s) {
        if (LOGGABLE) {
            logger.fine(s);
        }
    }
}
