/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.issues;

import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import org.netbeans.modules.javafx.refactoring.SourceTestBase;
import org.netbeans.modules.javafx.refactoring.repository.ClassModel;
import org.netbeans.modules.javafx.refactoring.repository.ClassModelFactory;
import org.netbeans.modules.javafx.refactoring.repository.ElementDef;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

/**
 *
 * @author Jaroslav Bachorik
 */
public class I184353 extends SourceTestBase {
    private ClassModel cm;

    public I184353(String testName) {
        super(testName);
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        ClassModelFactory cmf = new ClassModelFactory();
        FileObject fo = FileUtil.toFileObject(getDataDir());
        fo = fo.getFileObject("org/netbeans/modules/javafx/refactoring/issues/I184353.fx");
        cm = cmf.classModelFor(fo);
    }

    public void testBClass() {
        ElementDef edef = cm.getDefForPos(75);
        assertEquals("B", edef.getName());
        assertEquals(ElementKind.CLASS, edef.getKind());
    }

    public void testAClass() {
        ElementDef edef = cm.getDefForPos(63);
        assertEquals("A", edef.getName());
        assertEquals(ElementKind.CLASS, edef.getKind());
    }
}
