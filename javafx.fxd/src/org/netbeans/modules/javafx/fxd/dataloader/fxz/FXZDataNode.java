/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
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
package org.netbeans.modules.javafx.fxd.dataloader.fxz;

import java.awt.Image;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataFilter;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataNode;
import org.openide.loaders.DataObject;
import org.openide.nodes.Children;
import org.openide.nodes.Node;
import org.openide.util.ImageUtilities;
import org.openide.util.RequestProcessor;

/**
 *
 * @author Pavel Benes
 */

public final class FXZDataNode extends DataNode {
    public static final String FILE_IMAGE_ID = "org/netbeans/modules/javafx/fxd/dataloader/resources/fxzFile16.png";  //NOI18N
    public static final Image  FILE_IMAGE    = ImageUtilities.loadImage( FILE_IMAGE_ID);
    
    public FXZDataNode(DataObject obj) {
        this(obj, new DummyChildren());
    }

    private FXZDataNode(DataObject obj, DummyChildren c) {
        super(obj, c);
        c.attachFXDNode(this);
        setIconBaseWithExtension(FILE_IMAGE_ID);
    }
        
    private static Children childrenFor(FileObject fxzArchive) {
        if (!FileUtil.isArchiveFile(fxzArchive)) {
            // Maybe corrupt, etc.
            return Children.LEAF;
        }
        FileObject root = FileUtil.getArchiveRoot(fxzArchive);
        if (root != null) {
            return DataFolder.findFolder(root).createNodeChildren(DataFilter.ALL);
        } else {
            return Children.LEAF;
        }
    }
    
    /**
     * There is no nice way to lazy create delegating node's children.
     * So, in order to fix #83595, here is a little hack that schedules
     * replacement of this dummy children on addNotify call.
     */
    final static class DummyChildren extends Children implements Runnable {

        private FXZDataNode node;

        @Override
        protected void addNotify() {
            super.addNotify();
            assert node != null;
            RequestProcessor.getDefault().post(this);
        }

        private void attachFXDNode(FXZDataNode fxdDataNode) {
            this.node = fxdDataNode;
        }

        public void run() {
            node.setChildren(childrenFor(node.getDataObject().getPrimaryFile()));
        }
        
        public boolean add(final Node[] nodes) {
            // no-op
            return false;
        }

        public boolean remove(final Node[] nodes) {
            // no-op
            return false;
        }        
    }
}
    
    