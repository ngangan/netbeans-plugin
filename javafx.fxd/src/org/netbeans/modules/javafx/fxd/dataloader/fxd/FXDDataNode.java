/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.fxd.dataloader.fxd;

import java.awt.Image;
import org.openide.loaders.DataNode;
import org.openide.nodes.Children;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;

public class FXDDataNode extends DataNode {
    public static final String FILE_IMAGE_ID = "org/netbeans/modules/javafx/fxd/dataloader/resources/fxdFile16.png";  //NOI18N
    public static final Image  FILE_IMAGE    = ImageUtilities.loadImage( FILE_IMAGE_ID);
    
    public FXDDataNode(FXDDataObject obj) {
        super(obj, Children.LEAF);
        setIconBaseWithExtension(FILE_IMAGE_ID);
    }

    FXDDataNode(FXDDataObject obj, Lookup lookup) {
        super(obj, Children.LEAF, lookup);
        setIconBaseWithExtension(FILE_IMAGE_ID);
    }
        
/** Creates a property sheet. */
//    @Override
//    protected Sheet createSheet() {
//        Sheet s = super.createSheet();
//        Sheet.Set ss = s.get(Sheet.PROPERTIES);
//        if (ss == null) {
//            ss = Sheet.createPropertiesSet();
//            s.put(ss);
//        }
//        // TODO add some relevant properties: ss.put(...)
//        return s;
//    }
}
