/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.impl.plugins.elements;

import java.util.Collections;
import java.util.Set;
import org.netbeans.modules.parsing.api.indexing.IndexingManager;
import org.netbeans.modules.refactoring.spi.SimpleRefactoringElementImplementation;
import org.openide.filesystems.FileObject;
import org.openide.text.PositionBounds;
import org.openide.util.Lookup;

/**
 *
 * @author Jaroslav Bachorik <yardus@netbeans.org>
 */
public class ReindexFilesElement extends SimpleRefactoringElementImplementation {
    private FileObject parent;
    private Set<? extends FileObject> related;
    
    public ReindexFilesElement(FileObject parent, Set<FileObject> related) {
        this.parent = parent;
        this.related = Collections.unmodifiableSet(related);
    }

    public String getDisplayText() {
        return null;
    }

    public Lookup getLookup() {
        return Lookup.EMPTY;
    }

    public FileObject getParentFile() {
        return parent;
    }

    public PositionBounds getPosition() {
        return null;
    }

    public String getText() {
        return getDisplayText();
    }

    public void performChange() {
        IndexingManager.getDefault().refreshAllIndices(related.toArray(new FileObject[related.size()]));
    }

    @Override
    public void undoChange() {
        IndexingManager.getDefault().refreshAllIndices(related.toArray(new FileObject[related.size()]));
    }
}
