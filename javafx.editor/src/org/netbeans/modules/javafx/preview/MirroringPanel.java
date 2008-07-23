/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 2008 Sun Microsystems, Inc. All rights reserved.
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
 * 
 * Contributor(s):
 * 
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
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
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import javax.swing.*;
import javax.swing.JPanel;
import org.openide.util.Exceptions;
import org.netbeans.api.project.Project;

public class MirroringPanel extends JPanel implements Runnable {
    JDialog         mirroredFrame = null;
    JPanel          mirroredPanel = null;
    EventQueue      mirroredEventQueue = null;
    EventQueue      mirroringEventQueue = null;
    LookAndFeel     lf = null;
    BufferedImage   offscreenBuffer = null;
    ThreadGroup     threadGroup = null;
    Project         project = null;
    
    public MirroringPanel(Project project, LookAndFeel lf) throws Exception {
        super();
        this.project = project;
        mirroringEventQueue = Toolkit.getDefaultToolkit().getSystemEventQueue();
        this.lf = lf;
        offscreenBuffer = (BufferedImage) createImage(getWidth(), getHeight());
        startMirroring();
    }
    
    protected JPanel createMirroredPanel() {
        return null;
    }
    
    public void run() {
        try {
            EventQueue.invokeAndWait(new Runnable() {

                public void run() {
                    try {
                        UIManager.setLookAndFeel(lf);
                    } catch (UnsupportedLookAndFeelException ex) {
                        Exceptions.printStackTrace(ex);
                    }

                    mirroredEventQueue = Toolkit.getDefaultToolkit().getSystemEventQueue();

                    KeyboardFocusManager.setCurrentKeyboardFocusManager(new DefaultKeyboardFocusManager() {

                        @Override
                        public Window getFocusedWindow() {
                            synchronized (KeyboardFocusManager.class) {
                                return mirroredFrame;
                            }
                        }
                    });

                    mirroredPanel = createMirroredPanel();
                    if (mirroredPanel == null) {
                        return;
                    }
                    mirroredFrame = new JDialog() {

                        ComponentPeer origDialogPeer;
                        ComponentPeer proxyInstPeer;

                        public void addNotify() {
                            super.addNotify();
                            if (!replacePeer()) {
                                setLocation(-2000, -2000);
                            }
                        }

                        boolean replacePeer() {
                            origDialogPeer = getPeer();
                            if (origDialogPeer.getClass().getName().startsWith(APPLE)) {
                                return false; // NOI18N
                            }
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
                                        if (method.getName().contentEquals(REQUESTFOCUS)) {
                                            ret = true;
                                        } else {
                                            ex.printStackTrace();
                                        }
                                    }
                                    return ret;
                                }
                            };

                            proxyInstPeer = (DialogPeer) Proxy.newProxyInstance(
                            DialogPeer.class.getClassLoader(), new Class[] {DialogPeer.class}, handler);

                            try {
                                Field peer = Component.class.getDeclaredField(PEER);
                                peer.setAccessible(true);
                                peer.set(this, proxyInstPeer);
                            } catch (Exception ex) {
                                ex.printStackTrace();
                            }
                            return true;
                        }
                        {
                        }
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
            });
        } catch (InterruptedException ex) {
            Exceptions.printStackTrace(ex);
        } catch (InvocationTargetException ex) {
            Exceptions.printStackTrace(ex);
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
    
    private Component focusOwner = null;
    void onMouseEvent(MouseEvent ev) {
        switch (ev.getID()) {
            case MouseEvent.MOUSE_ENTERED:
                SwingUtilities.getWindowAncestor(this).setFocusableWindowState(false);
                focusOwner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                break;
            case MouseEvent.MOUSE_EXITED:
                SwingUtilities.getWindowAncestor(this).setFocusableWindowState(true);
                if (focusOwner != null) focusOwner.requestFocus();
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
        mirroredFrame.dispose();
    }
    
    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        cleanup();
    }
    
    private static int instanceCounter = 0;
    
    void startMirroring() throws Exception {
        try {
            JavaFXModel.runInAC(project, this);
        } catch (Exception ex) {
            JavaFXModel.destroyAC(project);
            throw ex;
        }
        enableEvents(AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK);
    }
 
    static private String STK = "sun.awt.SunToolkit";                       // NOI18N
    static private String CNAPC = "createNewAppContext";                    // NOI18N
    static private String APC = "sun.awt.AppContext";                       // NOI18N
    static private String DSP = "dispose";                                  // NOI18N
    static private String SHOW = "show";                                    // NOI18N
    static private String PEER = "peer";                                    // NOI18N
    static private String APPLE = "apple";                                  // NOI18N
    static private String REQUESTFOCUS = "requestFocus";                    // NOI18N
}