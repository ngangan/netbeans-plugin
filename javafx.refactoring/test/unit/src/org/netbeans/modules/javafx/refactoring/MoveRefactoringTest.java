/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring;

import java.util.ArrayList;
import java.util.List;
import javax.swing.text.Document;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.netbeans.modules.javafx.refactoring.transformations.*;

/**
 *
 * @author Jaroslav Bachorik
 */
public class MoveRefactoringTest extends SourceTestBase {

    public MoveRefactoringTest(String testName) {
        super(testName);
    }

    public void testEditorReplace() throws Exception {
        FileObject fo = FileUtil.toFileObject(getDataDir());
        fo = fo.getFileObject("org/netbeans/modules/javafx/refactoring/GSimpleEdge.fx");

        DataObject dobj = DataObject.find(fo);
        EditorCookie ec = dobj.getCookie(EditorCookie.class);
        Document d = ec.openDocument();

        List<Transformation> ts1 = new ArrayList<Transformation>();
        List<Transformation> ts2 = new ArrayList<Transformation>();
        ts1.add(new RemoveTextTransformation(199, 32));
        ts1.add(new RemoveTextTransformation(231, 47));
        ts2.add(new ReplaceTextTransformation(206, "mathematics.graph", "mathematics.graph.simple"));
        
        Transformer t = new DocumentTransformer(d);
        t.transform(ts1);
        t.transform(ts2);

        Thread.currentThread().sleep(200);
        System.out.println(d.getText(0, d.getLength()));

        t = new DocumentTransformer(d);
        t.transform(ts2);
        t.transform(ts1);
        Thread.currentThread().sleep(200);
        System.out.println(d.getText(0, d.getLength()));
    }

}
