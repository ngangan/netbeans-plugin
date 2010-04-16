/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.issues;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import javax.swing.text.Document;
import org.netbeans.modules.javafx.refactoring.SourceTestBase;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.netbeans.modules.javafx.refactoring.transformations.*;

/**
 *
 * @author Jaroslav Bachorik
 */
public class I184290 extends SourceTestBase {
    private Document d;
    private Transformer t;
    private List<Transformation> removes;
    private List<Transformation> replaces;

    private String backup = null;

    public I184290(String testName) {
        super(testName);
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        FileObject fo = FileUtil.toFileObject(getDataDir());
        fo = fo.getFileObject("org/netbeans/modules/javafx/refactoring/GSimpleEdge.fx");

        DataObject dobj = DataObject.find(fo);
        EditorCookie ec = dobj.getCookie(EditorCookie.class);
        d = ec.openDocument();
        backup = d.getText(0, d.getLength());

        removes = new ArrayList<Transformation>();
        replaces = new ArrayList<Transformation>();
        removes.add(new RemoveTextTransformation(199, 32));
        removes.add(new RemoveTextTransformation(231, 47));
        replaces.add(new ReplaceTextTransformation(206, "mathematics.graph", "mathematics.graph.simple"));

        t = new DocumentTransformer(d);
    }

    @Override
    protected void tearDown() throws Exception {
        d.remove(0, d.getLength());
        d.insertString(0, backup, null);
        d = null;
        t = null;
        removes = null;
        replaces = null;
        
        super.tearDown();
    }

    public void testRemoveReplace() throws Exception {
        t.transform(removes);
        t.transform(replaces);

        Thread.currentThread().sleep(300);

        String transformed = d.getText(0, d.getLength());
        File golden = getGoldenFile("GSimpleEdge.fx");
        FileObject fog = FileUtil.toFileObject(golden);
        String goldenTxt = fog.asText();
        assertEquals(goldenTxt, transformed);
    }

    public void testReplaceRemove() throws Exception {
        t.transform(replaces);
        t.transform(removes);
        
        Thread.currentThread().sleep(300);

        String transformed = d.getText(0, d.getLength());
        File golden = getGoldenFile("GSimpleEdge.fx");
        FileObject fog = FileUtil.toFileObject(golden);
        String goldenTxt = fog.asText();
        assertEquals(goldenTxt, transformed);
    }

}
