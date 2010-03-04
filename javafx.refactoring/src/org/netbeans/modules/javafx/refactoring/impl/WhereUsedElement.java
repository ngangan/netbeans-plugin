/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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
package org.netbeans.modules.javafx.refactoring.impl;

import com.sun.javafx.api.tree.JavaFXTreePath;
import java.io.IOException;
import javax.swing.text.BadLocationException;
import javax.swing.text.Position.Bias;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.editor.GuardedDocument;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.SourceUtils;
import org.netbeans.modules.javafx.refactoring.repository.ClassModelFactory;
import org.netbeans.modules.javafx.refactoring.repository.ElementDef;
import org.netbeans.modules.javafx.refactoring.repository.Usage;
import org.netbeans.modules.refactoring.spi.SimpleRefactoringElementImplementation;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.text.DataEditorSupport;
import org.openide.text.PositionBounds;
import org.openide.util.Lookup;
import org.openide.util.lookup.Lookups;
import org.openide.util.lookup.ProxyLookup;

/**
 * An element in the refactoring preview list which holds information about the find-usages-match
 * 
 * @author Jaroslav Bachorik
 */

public class WhereUsedElement extends SimpleRefactoringElementImplementation {
    private PositionBounds bounds;
    private String displayText;
    volatile private int startPosition = -1;
    volatile private int endPosition = -1;

    private DataEditorSupport des;
    private GuardedDocument doc;

    private final Usage usage;
    private final Lookup context;

    private WhereUsedElement(Usage usage) throws IOException {
        this(usage, Lookup.EMPTY);
    }

    private WhereUsedElement(Usage usage, Lookup context) throws IOException {
        this.usage = usage;
        this.context = new ProxyLookup(context, Lookups.singleton(usage));
        init();
    }

    private WhereUsedElement(ElementDef def, FileObject file) throws IOException {
        this.usage = null;
        this.context = Lookup.EMPTY;
        init();
    }

    private void init() throws IOException {
        DataObject dobj = DataObject.find(usage.getFile());
        des = (DataEditorSupport)dobj.getCookie(EditorCookie.class);
        doc = (GuardedDocument)des.getDocument();
        if (doc == null) {
            doc = (GuardedDocument)des.openDocument();
        }

        startPosition = usage.getStartPos();
        endPosition = usage.getEndPos();
        try {
            doc.readLock();
            if (startPosition != -1) {
                int sta = Utilities.getRowFirstNonWhite(doc, startPosition);
                if (sta == -1) {
                    sta = Utilities.getRowStart(doc, startPosition);
                }
                int en = Utilities.getRowLastNonWhite(doc, startPosition);

                if (en == -1) {
                    en = Utilities.getRowEnd(doc, startPosition);
                } else {
                    // Last nonwhite - left side of the last char, not inclusive
                    en++;
                }

                displayText = SourceUtils.getHtml(doc.getText(sta, en - sta + 1), startPosition - sta);
                bounds = new PositionBounds(des.createPositionRef(startPosition, Bias.Forward), des.createPositionRef(endPosition, Bias.Forward));
            } else {
                throw new IOException("*** Can not resolve: " + usage); // NOI18N
            }
        } catch (BadLocationException e) {
            IOException ioe = new IOException(e.getLocalizedMessage());
            ioe.initCause(e);
            throw ioe;
        } finally {
            doc.readUnlock();
        }
    }

    public String getDisplayText() {
        return displayText;
    }

    public Lookup getLookup() {
        return context;
    }

    public PositionBounds getPosition() {
        return bounds;
    }

    public String getText() {
        return displayText;
    }

    public void performChange() {
    }

    public FileObject getParentFile() {
        return usage.getFile();
    }

    public static WhereUsedElement create(Usage usg, Lookup context) {
        try {
            return new WhereUsedElement(usg, context);
        } catch (IOException e) {

        }
        return null;
    }

    public static WhereUsedElement create(Usage usg) {
        try {
            return new WhereUsedElement(usg);
        } catch (IOException e) {

        }
        return null;
    }
}
