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
 * Portions Copyrighted 2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.visage.editor.completion.environment;


import com.sun.tools.visage.tree.VSGSequenceIndexed;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.api.visage.lexer.VSGTokenId;
import org.netbeans.modules.visage.editor.completion.VisageCompletionEnvironment;

import javax.lang.model.type.TypeMirror;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.type.TypeKind;

/**
 *
 * @author Petr Nejedly
 */
public class SequenceIndexedEnvironment extends VisageCompletionEnvironment<VSGSequenceIndexed> {

    private static final Logger logger = Logger.getLogger(SequenceIndexedEnvironment.class.getName());
    private static final boolean LOGGABLE = logger.isLoggable(Level.FINE);

    @Override
    protected void inside(VSGSequenceIndexed t) throws IOException {
        if (LOGGABLE) {
            logger.fine("inside VSGSequenceIndexed " + t + "  offset == " + offset); // NOI18N
            TokenSequence<VSGTokenId> last = findLastNonWhitespaceToken((int) sourcePositions.getStartPosition(root, t), offset);
            logger.fine("    last(1) == " + (last == null ? "null" : last.token().id())); // NOI18N
        }
        localResult(getSmartType(t));
//        addValueKeywords();
    }

    private TypeMirror getSmartType(VSGSequenceIndexed t) throws IOException {
        // the target type is Number (int) as an sequence index
        TypeMirror type = controller.getTypes().getPrimitiveType(TypeKind.INT);
        return type;
    }
}
