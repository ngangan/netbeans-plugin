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
 * Portions Copyrighted 2007 Sun Microsystems, Inc.
 */

package org.netbeans.modules.visage.editor.preview;

import java.awt.Graphics;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.modules.visage.project.VisageProject;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.netbeans.spi.project.support.ant.PropertyUtils;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.modules.InstalledFileLocator;
import org.openide.nodes.Node;
import org.openide.util.NbBundle;
import org.openide.util.RequestProcessor;
import org.openide.util.Utilities;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;

/**
 * Top component which displays Visage Preview.
 */
public final class VisagePreviewTopComponent extends TopComponent implements PropertyChangeListener {

    private static VisagePreviewTopComponent instance;
    /** path to the icon used by the component and its open action */
//    static final String ICON_PATH = "SET/PATH/TO/ICON/HERE";

    private static final String PREFERRED_ID = "VisagePreviewTopComponent"; //NOI18N
    private static final Logger log = Logger.getLogger("org.netbeans.visage.preview"); //NOI18N

    private static final File fxHome = InstalledFileLocator.getDefault().locate("visage-sdk", "org.netbeans.modules.visage.platform", false); //NOI18N
    private static final File previewLib = InstalledFileLocator.getDefault().locate("modules/ext/org-netbeans-visage-preview.jar", "org.netbeans.modules.visage.editor", false); //NOI18N

    private BufferedImage bi;
    private DataObject oldD;
    private Process pr;
    private int timer;

    private final RequestProcessor taskQueue = new RequestProcessor(VisagePreviewTopComponent.class.getName(), 1);

    private static boolean isResource(String name) {
        if (name.endsWith(".class")) return false; //NOI18N
        if (name.endsWith(".java")) return false; //NOI18N
        if (name.endsWith(".fx")) return false; //NOI18N
        if (name.endsWith(".cvsignore")) return false; //NOI18N
        if (name.endsWith(".hgignore")) return false; //NOI18N
        if (name.endsWith("vssver.scc")) return false; //NOI18N
        if (name.endsWith(".DS_Store")) return false; //NOI18N
        if (name.endsWith("~")) return false; //NOI18N
        if (name.indexOf("/CVS/") >= 0) return false; //NOI18N
        if (name.indexOf("/.svn/") >= 0) return false; //NOI18N
        if (name.indexOf("/.hg/") >= 0) return false; //NOI18N
        if (name.indexOf("/.#") >= 0) return false; //NOI18N
        if (name.indexOf("/._") >= 0) return false; //NOI18N
        if (name.endsWith("#") && name.indexOf("/#") >= 0) return false; //NOI18N
        if (name.endsWith("%") && name.indexOf("/%") >= 0) return false; //NOI18N
        if (name.endsWith("MANIFEST.MF")) return false; //NOI18N
        return true;
    }

    private void copyResources(FileObject dir1, FileObject dir2) {
        for (FileObject f : dir1.getChildren()) try {
            if (f.isFolder()) {
                dir2 = FileUtil.createFolder(dir2, f.getNameExt());
                copyResources(f, dir2);
            } else if (isResource(f.getPath())) {
                FileObject f2 = dir2.getFileObject(f.getName(), f.getExt());
                if (f2 == null || f.lastModified().after(f2.lastModified())) {
                    if (f.getExt().equals("fxz")) { //NOI18N
                        dir2 = FileUtil.createFolder(dir2, f.getNameExt());
                        InputStream is = f.getInputStream();
                        try {
                            FileUtil.extractJar(dir2, is);
                        } finally {
                            is.close();
                        }
                    } else FileUtil.copyFile(f, dir2, f.getName());
                }
            }
        } catch (IOException ioe) {
            log.severe(ioe.getLocalizedMessage());
        }
    }

    private final RequestProcessor.Task task = taskQueue.create(new Runnable() {
        public void run() {
            synchronized (VisagePreviewTopComponent.this) {
                if (pr != null) {
                    pr.destroy();
                    timer = 0;
                    task.schedule(150);
                    return;
                }
            }
            if (oldD != null) {
                oldD.removePropertyChangeListener(VisagePreviewTopComponent.this);
                oldD = null;
            }
            Node[] sel = TopComponent.getRegistry().getActivatedNodes();
            if (sel.length == 1) {
                DataObject d = sel[0].getLookup().lookup(DataObject.class);
                if (d != null) {
                    FileObject f = d.getPrimaryFile();
                    if (f.isData())  bi = null;
                    if ("fx".equals(f.getExt())) { //NOI18N
                        d.addPropertyChangeListener(VisagePreviewTopComponent.this);
                        oldD = d;
                        Project p = FileOwnerQuery.getOwner(f);
                        if (p instanceof VisageProject) {
                            PropertyEvaluator ev = ((VisageProject)p).evaluator();
                            FileObject srcRoots[] = ((VisageProject)p).getFOSourceRoots();
                            StringBuilder src = new StringBuilder();
                            String className = null;
                            File basedir = FileUtil.toFile(p.getProjectDirectory());
                            File build = PropertyUtils.resolveFile(basedir, "build/compiled"); //NOI18N
                            try {
                                FileObject buildCompiled = FileUtil.createFolder(build);
                                for (FileObject srcRoot : srcRoots) {
                                    if (src.length() > 0) src.append(';');
                                    src.append(FileUtil.toFile(srcRoot).getAbsolutePath());
                                    if (FileUtil.isParentOf(srcRoot, f)) {
                                        className = FileUtil.getRelativePath(srcRoot, f);
                                        className = className.substring(0, className.length() - 3).replace('/', '.');
                                    }
                                    copyResources(srcRoot, buildCompiled);
                                }
                            } catch (IOException ioe) {
                                log.severe(ioe.getLocalizedMessage());
                            }
                            String cp = ev.getProperty("javac.classpath"); //NOI18N
                            cp = cp == null ? "" : cp.trim();
                            String enc = ev.getProperty("source.encoding");  //NOI18N
                            if (enc == null || enc.trim().length() == 0) enc = "UTF-8"; //NOI18N
                            ArrayList<String> args = new ArrayList<String>();
                            args.add(fxHome + "/bin/visagec" + (Utilities.isWindows() ? ".exe" : "")); //NOI18N
                            args.add("-cp"); //NOI18N
                            args.add(build.getAbsolutePath() + File.pathSeparator + cp);
                            args.add("-sourcepath"); //NOI18N
                            args.add(src.toString());
                            args.add("-d"); //NOI18N
                            args.add(build.getAbsolutePath());
                            args.add("-encoding"); //NOI18N
                            args.add(enc.trim());
                            args.add(FileUtil.toFile(f).getAbsolutePath());
                            try {
                                build.mkdirs();
                                log.info(args.toString());
                                synchronized (VisagePreviewTopComponent.this) {
                                    pr = Runtime.getRuntime().exec(args.toArray(new String[args.size()]), null, basedir);
                                }
                                if (log.isLoggable(Level.INFO)) {
                                    ByteArrayOutputStream err = new ByteArrayOutputStream();
                                    InputStream in = pr.getErrorStream();
                                    try {
                                        FileUtil.copy(in, err);
                                    } catch (IOException e) {
                                        log.severe(e.getLocalizedMessage());
                                    } finally {
                                        try {
                                            in.close();
                                        } catch (IOException e) {}
                                    }
                                    log.info(err.toString());
                                }
                                pr.waitFor();
                                String jvmargs = ev.getProperty("run.jvmargs"); //NOI18N
                                String appargs = ev.getProperty("application.args");  //NOI18N
                                if (pr.exitValue() == 0) {
                                    args = new ArrayList<String>();
                                    args.add(fxHome + "/bin/visage" + (Utilities.isWindows() ? ".exe" : "")); //NOI18N
                                    args.add("-javaagent:" + previewLib.getAbsolutePath());//NOI18N
                                    args.add("-Xbootclasspath/p:" + previewLib.getAbsolutePath());//NOI18N
                                    args.add("-Dcom.apple.backgroundOnly=true"); //NOI18N
                                    args.add("-Dapple.awt.UIElement=true"); //NOI18N
                                    if (jvmargs != null) for (String ja : jvmargs.trim().split("\\s+")) if (ja.length()>0) args.add(ja); //NOI18N
                                    args.add("-cp"); //NOI18N
                                    args.add(src.toString() + File.pathSeparator + build.getAbsolutePath() + File.pathSeparator + cp); //NOI18N
                                    args.add(className);
                                    if (appargs != null) for (String aa : appargs.trim().split("\\s+")) args.add(aa); //NOI18N
                                    log.info(args.toString());
                                    synchronized (VisagePreviewTopComponent.this) {
                                        pr = Runtime.getRuntime().exec(args.toArray(new String[args.size()]), null, basedir);
                                    }
                                    if (log.isLoggable(Level.INFO)) {
                                        taskQueue.execute(new Runnable() {
                                            public void run() {
                                                ByteArrayOutputStream err = new ByteArrayOutputStream();
                                                Process p = pr;
                                                if (p == null) return;
                                                InputStream in = p.getErrorStream();
                                                try {
                                                    final byte[] BUFFER = new byte[4096];
                                                    int len;
                                                    while (pr != null && timer > 0) {
                                                        while ((len = in.available()) > 0) {
                                                            len = in.read(BUFFER, 0, BUFFER.length > len ? len : BUFFER.length);
                                                            err.write(BUFFER, 0, len);
                                                        }
                                                        Thread.sleep(50);
                                                    }
                                                } catch (IOException e) {
                                                    log.severe(e.getLocalizedMessage());
                                                } catch (InterruptedException ie) {
                                                } finally {
                                                    try {
                                                        in.close();
                                                    } catch (IOException e) {}
                                                }
                                                log.info(err.toString());
                                            }
                                        });
                                    }
                                    InputStream in = pr.getInputStream();
                                    timer = 200;
                                    while (timer-- > 0 && in.available() == 0) Thread.sleep(50);
                                    if (in.available() > 0) bi = ImageIO.read(in);
                                }
                            } catch (Exception ex) {
                                //ignore
                            } finally {
                                synchronized (VisagePreviewTopComponent.this) {
                                    if (pr != null) pr.destroy();
                                    pr = null;
                                }
                            }
                        }
                    }
                }
            }
            repaint();

        }
    });

    @Override
    public void paintComponent(Graphics g) {
        BufferedImage b = bi;
        if (b != null) g.drawImage(b, 0, 0, null);
        else {
            g.clearRect(0, 0, getWidth(), getHeight());
            String noPreview = NbBundle.getMessage(VisagePreviewTopComponent.class, "MSG_NoPreview"); //NOI18N
            Rectangle2D r = g.getFontMetrics().getStringBounds(noPreview, g);
            g.drawString(noPreview, (getWidth()-(int)r.getWidth())/2, (getHeight()-(int)r.getHeight())/2);
        }
    }

    private VisagePreviewTopComponent() {
        initComponents();
        setName(NbBundle.getMessage(VisagePreviewTopComponent.class, "CTL_VisagePreviewTopComponent")); //NOI18N
        setToolTipText(NbBundle.getMessage(VisagePreviewTopComponent.class, "HINT_VisagePreviewTopComponent")); //NOI18N
//        setIcon(Utilities.loadImage(ICON_PATH, true));
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        setLayout(new java.awt.BorderLayout());
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables
    /**
     * Gets default instance. Do not use directly: reserved for *.settings files only,
     * i.e. deserialization routines; otherwise you could get a non-deserialized instance.
     * To obtain the singleton instance, use {@link #findInstance}.
     */
    public static synchronized VisagePreviewTopComponent getDefault() {
        if (instance == null) {
            instance = new VisagePreviewTopComponent();
        }
        return instance;
    }

    /**
     * Obtain the VisagePreviewTopComponent instance. Never call {@link #getDefault} directly!
     */
    public static synchronized VisagePreviewTopComponent findInstance() {
        TopComponent win = WindowManager.getDefault().findTopComponent(PREFERRED_ID);
        if (win == null) {
            Logger.getLogger(VisagePreviewTopComponent.class.getName()).warning("Cannot find " + PREFERRED_ID + " component. It will not be located properly in the window system."); //NOI18N
            return getDefault();
        }
        if (win instanceof VisagePreviewTopComponent) {
            return (VisagePreviewTopComponent) win;
        }
        Logger.getLogger(VisagePreviewTopComponent.class.getName()).warning("There seem to be multiple components with the '" + PREFERRED_ID + "' ID. That is a potential source of errors and unexpected behavior."); //NOI18N
        return getDefault();
    }

    @Override
    public int getPersistenceType() {
        return TopComponent.PERSISTENCE_ALWAYS;
    }

    @Override
    public void componentOpened() {
        TopComponent.getRegistry().addPropertyChangeListener(this);
        task.schedule(150);
    }

    @Override
    public void componentClosed() {
        TopComponent.getRegistry().removePropertyChangeListener(this);
    }

    public void propertyChange(PropertyChangeEvent ev) {
        if (TopComponent.Registry.PROP_ACTIVATED_NODES.equals(ev.getPropertyName()) || DataObject.PROP_MODIFIED.equals(ev.getPropertyName())) {
            task.schedule(150);
        }
    }

    /** replaces this in object stream */
    @Override
    public Object writeReplace() {
        return new ResolvableHelper();
    }

    @Override
    protected String preferredID() {
        return PREFERRED_ID;
    }

    final static class ResolvableHelper implements Serializable {

        private static final long serialVersionUID = 1L;

        public Object readResolve() {
            return VisagePreviewTopComponent.getDefault();
        }
    }
}
