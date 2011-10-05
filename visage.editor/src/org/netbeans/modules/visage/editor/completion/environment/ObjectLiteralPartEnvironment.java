/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2008-2010 Oracle and/or its affiliates. All rights reserved.
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
 * Portions Copyrighted 2008-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.visage.editor.completion.environment;

import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeKind;
import org.netbeans.modules.visage.editor.completion.VisageCompletionEnvironment;

import javax.lang.model.element.Element;
import javax.lang.model.type.TypeMirror;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.ElementKind;
import javax.lang.model.type.DeclaredType;
import org.netbeans.modules.visage.editor.completion.VisageCompletionItem;
import org.visage.api.tree.VisageTreePath;
import org.visage.tools.tree.VisageErroneous;
import org.visage.tools.tree.VisageObjectLiteralPart;

/**
 * @author David Strupl
 */
public class ObjectLiteralPartEnvironment extends VisageCompletionEnvironment<VisageObjectLiteralPart> {

    // -J-Dorg.netbeans.modules.visage.editor.completion.environment.ObjectLiteralPartEnvironment.level=FINE
    private static final Logger logger = Logger.getLogger(ObjectLiteralPartEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    @Override
    protected void inside(VisageObjectLiteralPart t) throws IOException {
        if (LOGGABLE) log("inside VisageObjectLiteralPart " + t + "  offset == " + offset); // NOI18N

        if (t.getExpression() instanceof VisageErroneous
                && tryToUseSanitizedSource()) {
            return;
        }

        // For fields with String type add double-quotes completion item
        if (path != null) {
            Element e = controller.getTrees().getElement(path);
            if (e != null && e.getKind() == ElementKind.FIELD) {
                TypeMirror type = e.asType();
                if (type.getKind() == TypeKind.DECLARED) {
                    TypeElement element = (TypeElement) ((DeclaredType) type).asElement();
                    if ("java.lang.String".contentEquals(element.getQualifiedName())) { // NOI18N
                        addResult(VisageCompletionItem.createConstantItem(
                                query.getComponent().getCaretPosition(), "\"\"", 1)); // NOI18N
                    }
                }
            }
        }
        addLocalAndImportedTypes(null, null, null, false, getSmartType(t));
        addLocalMembersAndVars(getSmartType(t));
        addValueKeywords();

    }

    private TypeMirror getSmartType(VisageObjectLiteralPart t) throws IOException {
        if (t.getExpression() == null) {
            return null;
        }
        // note: this is probably wrong
        TypeMirror type = controller.getTrees().getTypeMirror(new VisageTreePath(path, t.getExpression()));
        // it should instead extract the name of the attribute and extract
        //   the type from the named attribute
        if (LOGGABLE) log("  smart == " + type); // NOI18N
        return type;
    }

    private static void log(String s) {
        if (LOGGABLE) {
            logger.fine(s);
        }
    }
}
