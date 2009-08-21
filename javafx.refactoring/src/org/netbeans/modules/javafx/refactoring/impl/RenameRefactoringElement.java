 /*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 *  Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 * 
 *  The contents of this file are subject to the terms of either the GNU
 *  General Public License Version 2 only ("GPL") or the Common
 *  Development and Distribution License("CDDL") (collectively, the
 *  "License"). You may not use this file except in compliance with the
 *  License. You can obtain a copy of the License at
 *  http://www.netbeans.org/cddl-gplv2.html
 *  or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 *  specific language governing permissions and limitations under the
 *  License.  When distributing the software, include this License Header
 *  Notice in each file and include the License file at
 *  nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Sun in the GPL Version 2 section of the License file that
 *  accompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 * 
 *  Contributor(s):
 * 
 *  Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.refactoring.impl;

import com.sun.javafx.api.tree.JavaFXTreePath;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Position.Bias;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.editor.GuardedDocument;
import org.netbeans.modules.javafx.refactoring.impl.javafxc.TreePathHandle;
import org.netbeans.modules.refactoring.spi.SimpleRefactoringElementImplementation;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.text.DataEditorSupport;
import org.openide.text.PositionBounds;
import org.openide.util.Lookup;

/**
 *
 * @author Jaroslav Bachorik
 */
public class RenameRefactoringElement extends SimpleRefactoringElementImplementation {
    final private static Logger LOGGER = Logger.getLogger(RenameRefactoringElement.class.getName());
    private Lookup context;
    private String oldText;
    private int startPosition = -1;
    private String newName;
    private TreePathHandle handle;
    private DataEditorSupport des;

    final public static RenameRefactoringElement create(TreePathHandle handle, String newName, String oldName, Lookup context) {
        try {
            return new RenameRefactoringElement(handle, newName, oldName, context);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, null, e);
        }
        return null;
    }

    private RenameRefactoringElement(TreePathHandle handle, String newName, String oldName, Lookup context) throws IOException {
        this.handle = handle;
        this.newName = newName;
        this.oldText = oldName;
        this.context = context;
        init();
    }

    public String getDisplayText() {
        return "Rename " + elementName() + " " + oldText + " -> " + newName;
    }

    public Lookup getLookup() {
        return context;
    }

    public FileObject getParentFile() {
        return handle.getFileObject();
    }

    public PositionBounds getPosition() {
        if (des == null || oldText == null) {
            System.err.println("kurva");
        }
        return new PositionBounds(des.createPositionRef(startPosition, Bias.Forward), des.createPositionRef(startPosition + oldText.length(), Bias.Forward));
    }

    public String getText() {
        return "Rename " + elementName();
    }

    public void performChange() {

            Document doc = des.getDocument();
            if (doc instanceof GuardedDocument) {
                ((GuardedDocument)doc).runAtomic(new Runnable() {

                    public void run() {
                        try {
                            TransformationContext tc = context.lookup(TransformationContext.class);
                            int offset = tc.getRealOffset(startPosition);
                            System.err.println("removing " + oldText.length() + "chars @" + offset);
                            des.getDocument().remove(offset, oldText.length());
                            des.getDocument().insertString(offset, newName, null);
                            context.lookup(TransformationContext.class).replaceText(startPosition, oldText.length(), newName.length());
                        } catch (BadLocationException e) {
                            e.printStackTrace();
                        }
                    }
                });
            }
    }

    private String elementName() {
        return handle.getKind().toString().toLowerCase();
    }

    private void init() throws IOException {
        DataObject dobj = DataObject.find(handle.getFileObject());
        des = (DataEditorSupport)dobj.getCookie(EditorCookie.class);
        
        JavaFXSource jfxs = JavaFXSource.forFileObject(handle.getFileObject());
        jfxs.runUserActionTask(new Task<CompilationController>() {

            public void run(CompilationController cc) throws Exception {
                JavaFXTreePath path = handle.resolve(cc);
                switch(handle.getKind()) {
                    case CLASS_DECLARATION: {
                        findClassName(path, cc);
                        break;
                    }
                    case IDENTIFIER:
                    case TYPE_CLASS:
                    case MEMBER_SELECT: {
                        findTypeName(path, cc);
                        break;
                    }
                }
            }
        }, true);
        if (startPosition == -1) {
            System.err.println(handle);
            throw new IOException();
        }
    }

    private void findClassName(JavaFXTreePath path, CompilationController cc) {
        TokenSequence<JFXTokenId> tokens = cc.getTreeUtilities().tokensFor(path.getLeaf());
        tokens.moveStart();
        boolean foundClass = false;
        while(tokens.moveNext()) {
            final Token<JFXTokenId> currentToken = tokens.token();
            if (currentToken.id() == JFXTokenId.CLASS) {
                foundClass = true;
                continue;
            }
            if (foundClass && currentToken.text().equals(oldText)) {
                startPosition = currentToken.offset(cc.getTokenHierarchy());
                break;
            }
        }
    }

    private void findTypeName(JavaFXTreePath path, CompilationController cc) {
        TokenSequence<JFXTokenId> tokens = cc.getTreeUtilities().tokensFor(path.getLeaf());
        tokens.moveStart();
        while(tokens.moveNext()) {
            final Token<JFXTokenId> currentToken = tokens.token();
            String text = currentToken.text().toString();
            if (text.equals(oldText)) {
                startPosition = currentToken.offset(cc.getTokenHierarchy());
                break;
            }
        }
    }
}
