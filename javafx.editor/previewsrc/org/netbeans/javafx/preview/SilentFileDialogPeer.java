package org.netbeans.javafx.preview;

import java.awt.peer.FileDialogPeer;
import java.io.FilenameFilter;

/**
 *
 * @author Adam
 */
public class SilentFileDialogPeer extends SilentDialogPeer implements FileDialogPeer {

    SilentFileDialogPeer(FileDialogPeer delegate) {
        super(delegate);
    }

    public void setFile(String file) {
        ((FileDialogPeer)delegate).setFile(file);
    }

    public void setDirectory(String dir) {
        ((FileDialogPeer)delegate).setDirectory(dir);
    }

    public void setFilenameFilter(FilenameFilter filter) {
        ((FileDialogPeer)delegate).setFilenameFilter(filter);
    }


}
