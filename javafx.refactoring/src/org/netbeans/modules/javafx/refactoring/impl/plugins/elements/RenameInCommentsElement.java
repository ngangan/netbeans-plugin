/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
