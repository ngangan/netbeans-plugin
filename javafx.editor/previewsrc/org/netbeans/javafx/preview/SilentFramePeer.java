package org.netbeans.javafx.preview;

import java.awt.Image;
import java.awt.MenuBar;
import java.awt.Rectangle;
import java.awt.peer.FramePeer;

/**
 *
 * @author Adam
 */
public class SilentFramePeer extends SilentWindowPeer implements FramePeer {

    SilentFramePeer(FramePeer delegate) {
        super(delegate);
    }

    public void setTitle(String title) {
        ((FramePeer)delegate).setTitle(title);
    }

    public void setMenuBar(MenuBar mb) {
        ((FramePeer)delegate).setMenuBar(mb);
    }

    public void setResizable(boolean resizeable) {
        ((FramePeer)delegate).setResizable(resizeable);
    }

    public void setState(int state) {
        ((FramePeer)delegate).setState(state);
    }

    public int getState() {
        return ((FramePeer)delegate).getState();
    }

    public void setMaximizedBounds(Rectangle bounds) {
        ((FramePeer)delegate).setMaximizedBounds(bounds);
    }

    public void setBoundsPrivate(int x, int y, int width, int height) {
        ((FramePeer)delegate).setBoundsPrivate(x, y, width, height);
    }

    public void setIconImage(Image im) {
        ((FramePeer)delegate).setIconImage(im);
    }

}
