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

package org.netbeans.modules.javafx.palette;

import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import org.netbeans.api.javafx.source.Imports;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.GuardedDocument;
import org.netbeans.lib.editor.codetemplates.api.CodeTemplate;
import org.netbeans.lib.editor.codetemplates.api.CodeTemplateManager;
import org.netbeans.modules.editor.indent.api.Indent;
import org.openide.util.Exceptions;
import org.openide.util.NbBundle;

/**
 *
 * @author Michal Skvor
 */
public final class JavaFXPaletteUtilities {

    private static boolean  insertSnippet(String code, Class c, String templateName, JTextComponent targetComponent, String... imp) {
        Document document = targetComponent.getDocument();
        if (document instanceof GuardedDocument) {
            GuardedDocument d = (GuardedDocument) document;
            if (d.isPosGuarded(targetComponent.getCaretPosition())){
                return false;
            }
        }
        if (code == null) {
            code = NbBundle.getMessage( c, templateName );
        }
        CodeTemplateManager ctm = CodeTemplateManager.get( targetComponent.getDocument());
        CodeTemplate template = ctm.createTemporary( code );
        template.insert( targetComponent );
        for (String i : imp) {
            Imports.addImport( targetComponent, i ); 
        }
        return true;
    }
    
    public static boolean  insertSnippet(Class c, String templateName, JTextComponent targetComponent, String... imp) {
        return insertSnippet(null, c, templateName, targetComponent, imp);
    }

    public static boolean  insertSnippet(String code, JTextComponent targetComponent, String... imp) {
        return insertSnippet(code, null, null, targetComponent, imp);
    }

    public static String CARET = "&CARET&"; // NOI18N
    
    public static void insert(String s, JTextComponent target) throws BadLocationException {
        insert(s, target, true);
    }

    public static void insert(String s, JTextComponent target, boolean reformat) throws BadLocationException {
        Document d = target.getDocument();
        if( d == null || !( d instanceof BaseDocument )) {
            return;
        }
        if (s == null) {
            s = "";
        }
        BaseDocument doc = (BaseDocument) d;
        Indent indent = Indent.get(doc);
        indent.lock();
        try {
            doc.atomicLock();
            try {
                int cursor_offset = s.indexOf(CARET);
                if (cursor_offset != -1) {
                    s = s.replace( CARET, "" ); // NOI18N
                }
                int start = insert( s, target, d );
                if (cursor_offset != -1) {
                    target.setCaretPosition(start + cursor_offset);
                }
                if (reformat && start >= 0 && d instanceof BaseDocument) {
                    // format the inserted text
                    int end = start + s.length();
                    indent.reindent(start, end);
                }
            } finally {
                doc.atomicUnlock();
            }
        } finally {
            indent.unlock();
        }
    }  
    
    private static int insert(String s, JTextComponent target, Document doc) throws BadLocationException {

        int start = -1;
        try {
            //at first, find selected text range
            Caret caret = target.getCaret();
            int p0 = Math.min(caret.getDot(), caret.getMark());
            int p1 = Math.max(caret.getDot(), caret.getMark());
            doc.remove(p0, p1 - p0);

            //replace selected text by the inserted one
            start = caret.getDot();

            doc.insertString(start, s, null);
        } catch( BadLocationException ble ) {
            Exceptions.printStackTrace(ble);
        }

        return start;
    }
}
