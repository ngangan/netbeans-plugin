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
package org.netbeans.modules.javafx.fxd.composer.editor;

import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.ext.ExtKit.ExtDefaultKeyTypedAction;
import org.netbeans.modules.javafx.fxd.composer.editor.BracketCompletion;

public class FXDKeyTypedAction extends ExtDefaultKeyTypedAction {

    @Override
    protected void insertString(BaseDocument doc, int dotPos, Caret caret, String str, boolean overwrite) throws BadLocationException {
        char insertedChar = str.charAt(0);
        if (insertedChar == '\"' || insertedChar == '\'') {
            boolean inserted = BracketCompletion.completeQuote(doc, dotPos, caret, insertedChar);
            if (inserted) {
                caret.setDot(dotPos + 1);
            } else {
                super.insertString(doc, dotPos, caret, str, overwrite);
            }
        } else {
            super.insertString(doc, dotPos, caret, str, overwrite);
            // we do support pair completion for []
            //do not support pair for {( and moving semicolon
            BracketCompletion.charInserted(doc, dotPos, caret, insertedChar);
        }
    }

    @Override
    protected void replaceSelection(JTextComponent target, int dotPos, Caret caret, String str, boolean overwrite) throws BadLocationException {
        char insertedChar = str.charAt(0);
        Document doc = target.getDocument();
        if (insertedChar == '\"' || insertedChar == '\'') {
            if (doc != null) {
                try {
                    boolean inserted = false;
                    int p0 = Math.min(caret.getDot(), caret.getMark());
                    int p1 = Math.max(caret.getDot(), caret.getMark());
                    if (p0 != p1) {
                        doc.remove(p0, p1 - p0);
                    }
                    int caretPosition = caret.getDot();
                    if (doc instanceof BaseDocument) {
                        inserted = BracketCompletion.completeQuote((BaseDocument) doc, caretPosition, caret, insertedChar);
                    }
                    if (inserted) {
                        caret.setDot(caretPosition + 1);
                    } else {
                        if (str != null && str.length() > 0) {
                            doc.insertString(p0, str, null);
                        }
                    }
                } catch (BadLocationException e) {
                    e.printStackTrace();
                }
            }
        } else {
            super.replaceSelection(target, dotPos, caret, str, overwrite);
            // we do support pair completion for []
            //do not support pair for {( and moving semicolon
            if (doc instanceof BaseDocument) {
                BracketCompletion.charInserted((BaseDocument) doc, caret.getDot() - 1, caret, insertedChar);
            }
        }
    }
}
