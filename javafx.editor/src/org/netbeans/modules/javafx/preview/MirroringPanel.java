/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.preview;

import java.awt.AWTEvent;
import java.awt.AWTEvent.*;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.DefaultKeyboardFocusManager;
import java.awt.EventQueue;
import java.awt.Graphics;
import java.awt.KeyboardFocusManager;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.InvocationEvent;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.awt.peer.ComponentPeer;
import java.awt.peer.DialogPeer;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import javax.swing.*;

import javax.swing.JPanel;
import org.openide.util.Exceptions;

public class MirroringPanel extends JPanel {
    JDialog         mirroredFrame = null;
    JPanel          mirroredPanel = null;
    MirroringThread mirroringTread = null;
    EventQueue      mirroredEventQueue = null;
    EventQueue      mirroringEventQueue = null;
    Object          ac = null;
    LookAndFeel     lf = null;
    BufferedImage   offscreenBuffer = null;
    ThreadGroup     threadGroup = null;
    
    public MirroringPanel(LookAndFeel lf) throws MPException {
        super();
        mirroringEventQueue = Toolkit.getDefaultToolkit().getSystemEventQueue();
        this.lf = lf;
        offscreenBuffer = (BufferedImage) createImage(getWidth(), getHeight());
        startMirroring();
    }
    
    protected JPanel createMirroredPanel() {
        return null;
    }
    
    class MirroringThread extends Thread {
        public MirroringThread(ThreadGroup tg) {
            super(tg, "SACT"); // NOI18N
        }
        
        @Override
        public void run() {
            try {
                Class<?> acc = this.getClass().getClassLoader().loadClass(STK);
                ac = acc.getDeclaredMethod(CNAPC).invoke(null);
            } catch (Exception ex) {
                Exceptions.printStackTrace(ex);
            }

            try {
                UIManager.setLookAndFeel(lf);
            } catch (UnsupportedLookAndFeelException ex) {
                Exceptions.printStackTrace(ex);
            }
            
            mirroredEventQueue = Toolkit.getDefaultToolkit().getSystemEventQueue();
            
            KeyboardFocusManager.setCurrentKeyboardFocusManager(new DefaultKeyboardFocusManager(){

              @Override
                public Window getFocusedWindow() {
                    synchronized (KeyboardFocusManager.class) {
                        return (mirroredFrame);
                    }
                }
            });

            mirroredPanel = createMirroredPanel();
            if (mirroredPanel == null) return;
            
            mirroredFrame = new JDialog() {
               ComponentPeer origDialogPeer;
               ComponentPeer proxyInstPeer;
               public void addNotify() {
                    super.addNotify();
                    if (!replacePeer()) setLocation(-2000, -2000);
               }
               boolean replacePeer() {
                    origDialogPeer = getPeer();
                    if (origDialogPeer.getClass().toString().startsWith("apple")) return false;

                    InvocationHandler handler = new InvocationHandler() {
                        public Object invoke(Object proxy, Method method, Object[] args) {
                            if (method.getName().contentEquals(SHOW)) {
                                return null;
                            }
                            
                            Object ret = null;
                            try {
                                ret = method.invoke(origDialogPeer, args);
                            } catch (Exception ex) {
                                // Linux problems
                                if (method.getName().contentEquals("requestFocus"))
                                   ret = true;
                                else
                                    ex.printStackTrace();
                            }
                            return ret;
                        }
                    };
 
                    proxyInstPeer = (DialogPeer)Proxy.newProxyInstance(
                        DialogPeer.class.getClassLoader(), new Class[] {DialogPeer.class}, handler);

                    try {
                        Field peer = Component.class.getDeclaredField(PEER);
                        peer.setAccessible(true);
                        peer.set(this, proxyInstPeer);
                    } catch (Exception ex) {
                        ex.printStackTrace();
                    }
                    return true;
                }; 
            };

            mirroredFrame.setUndecorated(true);
            mirroredFrame.setFocusableWindowState(false);
            mirroredFrame.setLayout(new BorderLayout());
            JScrollPane jsp = new JScrollPane();
            jsp.setViewportView(mirroredPanel);
            mirroredFrame.add(jsp);
            mirroredFrame.setVisible(true);
            mirroredFrame.setSize(getSize().width + mirroredFrame.getInsets().left + mirroredFrame.getInsets().right, getSize().height + mirroredFrame.getInsets().top + mirroredFrame.getInsets().bottom);
            mirroredFrame.setFocusableWindowState(true);
            
            RepaintManager.setCurrentManager(new RepaintManager() {
                @Override
                public void paintDirtyRegions() {
                    super.paintDirtyRegions();
                    if (offscreenBuffer != null) {
                        if (mirroredFrame != null) {
                            mirroredFrame.getLayeredPane().paintAll(offscreenBuffer.getGraphics());
                            mirroringEventQueue.postEvent(new InvocationEvent(Toolkit.getDefaultToolkit(), new Runnable() {
                                public void run() {
                                    repaint();
                                }
                            }));
                        }
                    }
                }
            });
        }
    }

    @Override
    protected void processMouseEvent(MouseEvent e) {
        super.processMouseEvent(e);
        onMouseEvent(e);
    }

    @Override
    protected void processMouseMotionEvent(MouseEvent e) {
        super.processMouseMotionEvent(e);
        onMouseEvent(e);
    }

    @Override
    public void setBounds(int x, int y, int w, int h) {
        super.setBounds(x, y, w, h);
        if (offscreenBuffer == null)
            offscreenBuffer = (BufferedImage) createImage(getWidth(), getHeight());
        BufferedImage newOffscreenBuffer = (BufferedImage) createImage(getWidth(), getHeight());
        newOffscreenBuffer.setData(offscreenBuffer.getRaster());
        offscreenBuffer = newOffscreenBuffer;
        if (mirroredFrame != null)
            mirroredFrame.setSize(getSize().width + mirroredFrame.getInsets().left + mirroredFrame.getInsets().right, getSize().height + mirroredFrame.getInsets().top + mirroredFrame.getInsets().bottom);
    }

    @Override
    public void setSize(int width, int height) {
        super.setSize(width, height);
        BufferedImage newOffscreenBuffer = (BufferedImage) createImage(getWidth(), getHeight());
        newOffscreenBuffer.setData(offscreenBuffer.getRaster());
        offscreenBuffer = newOffscreenBuffer;
    }

    @Override
    public void paint(Graphics g) {
        g.drawImage(offscreenBuffer, 0, 0, null);
    }
    
    void onMouseEvent(MouseEvent ev) {
        switch (ev.getID()) {
            case MouseEvent.MOUSE_ENTERED:
                SwingUtilities.getWindowAncestor(this).setFocusableWindowState(false);
                break;
            case MouseEvent.MOUSE_EXITED:
                SwingUtilities.getWindowAncestor(this).setFocusableWindowState(true);
                break;
        }
        if (mirroredEventQueue != null && mirroredFrame != null) {
            Point point =  SwingUtilities.convertPoint(ev.getComponent(), ev.getPoint(), this);
            point =  SwingUtilities.convertPoint(mirroredFrame.getLayeredPane(), point, mirroredFrame);
            mirroredEventQueue.postEvent(new MouseEvent(mirroredFrame, ev.getID(), ev.getWhen(), ev.getModifiers(), point.x, point.y, ev.getClickCount(), ev.isPopupTrigger()));

            Component mirroredComponent = SwingUtilities.getDeepestComponentAt(mirroredFrame, point.x, point.y);
            if (mirroredComponent != null)
                setCursor(mirroredComponent.getCursor());
        }
    }

    public void cleanup() {
        Window ancestor = SwingUtilities.getWindowAncestor(this);
        if (ancestor != null) ancestor.setFocusableWindowState(true);
        disableEvents(AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK);
        if (ac != null) {
            try {
                Class<?> acc = this.getClass().getClassLoader().loadClass(APC);
                acc.getDeclaredMethod(DSP).invoke(ac);
            } catch (Throwable er) {
                er.printStackTrace();
            }
        }
        mirroredEventQueue = null;
        ac = null;
    }
    
    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        cleanup();
    }
    
    private static int instanceCounter = 0;
    
    void startMirroring() throws MPException {
        threadGroup = new ThreadGroup("SACG" + instanceCounter++); // NOI18N
        mirroringTread = new MirroringThread(threadGroup);
        mirroringTread.start();
        try {
            mirroringTread.join(20000);
            if (mirroringTread.isAlive()) {
                cleanup();
                throw new MPException();
            }
        } catch (InterruptedException ex) {
            Exceptions.printStackTrace(ex);
        }
        enableEvents(AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK);
    }
 
    public class MPException extends Exception {
    }
    
    static private String STK = "sun.awt.SunToolkit";                       // NOI18N
    static private String CNAPC = "createNewAppContext";                    // NOI18N
    static private String APC = "sun.awt.AppContext";                       // NOI18N
    static private String DSP = "dispose";                                  // NOI18N
    static private String SHOW = "show";                                    // NOI18N
    static private String PEER = "peer";                                    // NOI18N
}