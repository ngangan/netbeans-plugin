/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
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
