package org.netbeans.javafx.preview;

import java.awt.peer.DialogPeer;

/**
 *
 * @author Adam
 */
public class SilentDialogPeer extends SilentWindowPeer implements DialogPeer {

    SilentDialogPeer(DialogPeer delegate) {
        super(delegate);
    }

    public void setTitle(String title) {
        ((DialogPeer)delegate).setTitle(title);
    }

    public void setResizable(boolean resizeable) {
        ((DialogPeer)delegate).setResizable(resizeable);
    }


}
