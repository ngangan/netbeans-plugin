/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.modules.javafx.editor.imports;

import org.netbeans.api.javafx.source.*;
import org.netbeans.modules.javafx.editor.JFXImportManager;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;

import javax.lang.model.element.Element;
import javax.swing.text.Document;
import java.io.IOException;
import java.util.HashSet;
import java.util.logging.Logger;

/**
 *
 * Algorithm:
 *  * Collect all important symbols
 *  * Collect all imports.
 *  * iterate over all symbols and verify if it is already imported or have to be.
 *  * collect resulting set of import
 *  * modify document 
 *
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.org">RKo</a>)
 */
public final class JavaFXImports implements JFXImportManager {

    private static JavaFXImports instance;
    public static final Logger logger = Logger.getLogger(JFXImportManager.class.getName());

    public synchronized static JavaFXImports getInstance() {
        if (instance == null) {
            instance = new JavaFXImports();
        }
        return instance;
    }

    private static FileObject getFileObject(Document doc) {
        DataObject od = (DataObject) doc.getProperty(Document.StreamDescriptionProperty);
        return od != null ? od.getPrimaryFile() : null;
    }


    /**
     * Fix imports within source code.
     *
     * @param document containing source code.
     */
    public void fixImports(final Document document) {
        final JavaFXSource s = JavaFXSource.forDocument(document);
        try {
            s.runUserActionTask(new Task<CompilationController>() {
                public void run(CompilationController cc) throws Exception {
                    final JavaFXSource.Phase phase = cc.toPhase(JavaFXSource.Phase.ANALYZED);
                    if (phase.lessThan(JavaFXSource.Phase.ANALYZED)) {
                        logger.warning("We did not reach required phase. Leaving without fix");
                        return;
                    }
                    final FileObject source = getFileObject(document);
                    if (source == null) {
                        throw new IllegalArgumentException("There is no associated fileobject for document.");
                    }
                    ClassIndex index = ClasspathInfo.create(source).getClassIndex();
                    HashSet<Element> elements = new HashSet<Element>(100, .9F);
                    cc.getCompilationUnit().accept(new IdentifierVisitor(cc), elements);
                    ImportsModel model = new ImportsModel(cc.getCompilationUnit().getImports(), index, cc);
                    for (Element element : elements) {
                        model.addImport(element);
                    }
                    model.optimize();
                    model.publish(document);
                }
            }, false);
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot fix imports due to underlying error.", e);
        }

    }
}
