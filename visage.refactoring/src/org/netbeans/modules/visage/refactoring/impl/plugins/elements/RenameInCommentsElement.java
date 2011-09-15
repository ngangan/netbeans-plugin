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


package org.netbeans.modules.javafx.refactoring.impl.plugins.elements;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import javax.swing.text.Document;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.javafx.refactoring.transformations.ReplaceTextTransformation;
import org.netbeans.modules.javafx.refactoring.transformations.Transformation;
import org.netbeans.modules.refactoring.api.RefactoringSession;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.util.NbBundle;

/**
 *
 * @author Jaroslav Bachorik
 */
public class RenameInCommentsElement extends BaseRefactoringElementImplementation {
    private String oldName, newName;

    public RenameInCommentsElement(String oldName, String newName, FileObject srcFO, RefactoringSession session) {
        super(srcFO, session);
        this.oldName = oldName;
        this.newName = newName;
    }
    
    @Override
    protected Set<Transformation> prepareTransformations(FileObject fo) {
        Set<Transformation> transforms = new HashSet<Transformation>();
        try {
            DataObject dobj = DataObject.find(fo);
            EditorCookie ec = dobj.getCookie(EditorCookie.class);
            Document doc = ec.openDocument();
            TokenHierarchy th = TokenHierarchy.get(doc);
            TokenSequence<JFXTokenId> ts = (TokenSequence<JFXTokenId>) th.tokenSequence();
            ts.moveStart();
            int pos = 0;
            while (ts.moveNext()) {
                Token<JFXTokenId> t = ts.token();
                if (t.id() == JFXTokenId.COMMENT || t.id() == JFXTokenId.DOC_COMMENT) {
                    int locPos = 0;
                    String tt = t.text().toString();
                    while (locPos > -1) {
                        locPos = tt.indexOf(oldName, locPos + 1);
                        if (locPos != -1) {
                            transforms.add(new ReplaceTextTransformation(pos + locPos, oldName, newName));
                        }
                    }
                }
                pos += t.length();
            }
        } catch (IOException e) {
        }
        return transforms;
    }

    @Override
    protected String getRefactoringText() {
        return NbBundle.getMessage(RenameInCommentsElement.class, "LBL_ReplaceOccurencesInComments", oldName, newName);
    }
}
