/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

/**
 *
 * @author karol
 */
import javax.swing.text.Document;

import org.netbeans.modules.javafx.editor.JavaFXDocument;
import org.openide.cookies.EditorCookie;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;
import org.openide.filesystems.FileObject;

final class HintsUtils {

    private HintsUtils() {
    }

    static JavaFXDocument getDocument(FileObject file) {
        if (file == null || !file.isValid()) {
            return null;
        }

        DataObject dataObject = null;
        try {
            dataObject = DataObject.find(file);
        } catch (DataObjectNotFoundException e) {
            e.printStackTrace();
        }
        EditorCookie ec = dataObject != null ? dataObject.getLookup().lookup(EditorCookie.class) : null;
        if (ec == null) {
            return null;
        }
        Document document = ec.getDocument();
        if (document instanceof JavaFXDocument) {
            return (JavaFXDocument) document;
        }

        return null;
    }

}
