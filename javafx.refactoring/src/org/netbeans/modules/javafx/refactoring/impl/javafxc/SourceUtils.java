/*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 *  Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 * 
 *  The contents of this file are subject to the terms of either the GNU
 *  General Public License Version 2 only ("GPL") or the Common
 *  Development and Distribution License("CDDL") (collectively, the
 *  "License"). You may not use this file except in compliance with the
 *  License. You can obtain a copy of the License at
 *  http://www.netbeans.org/cddl-gplv2.html
 *  or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 *  specific language governing permissions and limitations under the
 *  License.  When distributing the software, include this License Header
 *  Notice in each file and include the License file at
 *  nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Sun in the GPL Version 2 section of the License file that
 *  accompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 * 
 *  Contributor(s):
 * 
 *  Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.refactoring.impl.javafxc;

import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.UnitTree;
import java.awt.Color;
import java.awt.Dialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.text.MessageFormat;
import java.util.EnumSet;
import java.util.Set;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicReference;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.swing.JLabel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.text.AttributeSet;
import javax.swing.text.StyleConstants;
import org.netbeans.api.editor.mimelookup.MimeLookup;
import org.netbeans.api.editor.mimelookup.MimePath;
import org.netbeans.api.editor.settings.FontColorSettings;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.source.ClasspathInfo.PathKind;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.ClassIndex;
import org.netbeans.api.javafx.source.ClasspathInfo;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.Task;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.ui.OpenProjects;
import org.netbeans.modules.javafx.project.JavaFXProjectConstants;
import org.netbeans.api.javafx.source.ClasspathInfoProvider;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.parsing.api.ParserManager;
import org.netbeans.modules.parsing.api.ResultIterator;
import org.netbeans.modules.parsing.api.UserTask;
import org.netbeans.modules.parsing.api.indexing.IndexingManager;
import org.netbeans.spi.java.classpath.support.ClassPathSupport;
import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Exceptions;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.RequestProcessor;

/**
 *
 * @author Jaroslav Bachorik
 */
final public class SourceUtils {
    public static final String JAVAFX_MIME_TYPE = "text/x-fx"; // NOI18N

    public static String htmlize(String input) {
        String temp = input.replace("<", "&lt;"); // NOI18N
        temp = temp.replace(">", "&gt;"); // NOI18N
        return temp;
    }

    public static String getHtml(String text) {
        StringBuffer buf = new StringBuffer();
        TokenHierarchy tokenH = TokenHierarchy.create(text, JFXTokenId.language());
        Lookup lookup = MimeLookup.getLookup(MimePath.get(JAVAFX_MIME_TYPE));
        FontColorSettings settings = lookup.lookup(FontColorSettings.class);
        TokenSequence tok = tokenH.tokenSequence();
        while (tok.moveNext()) {
            Token<JFXTokenId> token = (Token) tok.token();
            String category = token.id().primaryCategory();
            if (category == null) {
                category = "whitespace"; //NOI18N
            }
            AttributeSet set = settings.getTokenFontColors(category);
            buf.append(color(htmlize(token.text().toString()), set));
        }
        return buf.toString();
    }

    private static String color(String string, AttributeSet set) {
        if (set==null)
            return string;
        if (string.trim().length() == 0) {
            return string.replace(" ", "&nbsp;").replace("\n", "<br>"); //NOI18N
        }
        StringBuffer buf = new StringBuffer(string);
        if (StyleConstants.isBold(set)) {
            buf.insert(0,"<b>"); //NOI18N
            buf.append("</b>"); //NOI18N
        }
        if (StyleConstants.isItalic(set)) {
            buf.insert(0,"<i>"); //NOI18N
            buf.append("</i>"); //NOI18N
        }
        if (StyleConstants.isStrikeThrough(set)) {
            buf.insert(0,"<s>"); // NOI18N
            buf.append("</s>"); // NOI18N
        }
        buf.insert(0,"<font color=" + getHTMLColor(StyleConstants.getForeground(set)) + ">"); //NOI18N
        buf.append("</font>"); //NOI18N
        return buf.toString();
    }

    private static String getHTMLColor(Color c) {
        String colorR = "0" + Integer.toHexString(c.getRed()); //NOI18N
        colorR = colorR.substring(colorR.length() - 2);
        String colorG = "0" + Integer.toHexString(c.getGreen()); //NOI18N
        colorG = colorG.substring(colorG.length() - 2);
        String colorB = "0" + Integer.toHexString(c.getBlue()); //NOI18N
        colorB = colorB.substring(colorB.length() - 2);
        String html_color = "#" + colorR + colorG + colorB; //NOI18N
        return html_color;
    }

    public static boolean isJavaFXFile(FileObject f) {
        return JAVAFX_MIME_TYPE.equals(f.getMIMEType()); //NOI18N
    }

    public static boolean isFileInOpenProject(FileObject file) {
        assert file != null;
        Project p = FileOwnerQuery.getOwner(file);
        if (p == null) {
            return false;
        }
        return isOpenProject(p);
    }

    private static boolean isOpenProject(Project p) {
        return OpenProjects.getDefault().isProjectOpen(p);
    }

    public static boolean isOnSourceClasspath(FileObject fo) {
        Project p = FileOwnerQuery.getOwner(fo);
        if (p==null)
            return false;

        //workaround for 143542
        Project[] opened = OpenProjects.getDefault().getOpenProjects();
        for (Project pr : opened) {
            for (SourceGroup sg : ProjectUtils.getSources(pr).getSourceGroups(JavaFXProjectConstants.SOURCES_TYPE_JAVAFX)) {
                if (fo==sg.getRootFolder() || (FileUtil.isParentOf(sg.getRootFolder(), fo) && sg.contains(fo))) {
                    return ClassPath.getClassPath(fo, ClassPath.SOURCE) != null;
                }
            }
        }
        return false;
        //end of workaround
        //return ClassPath.getClassPath(fo, ClassPath.SOURCE)!=null;
    }

    public static boolean isClasspathRoot(FileObject fo) {
        ClassPath cp = ClassPath.getClassPath(fo, ClassPath.SOURCE);
        return cp != null ? fo.equals(cp.findOwnerRoot(fo)) : false;
    }
    
    public static ElementKind getElementKind(final TreePathHandle tph) {
        try {
            final JavaFXSource source = JavaFXSource.forFileObject(tph.getFileObject());
            final AtomicReference<ElementKind> kind = new AtomicReference<ElementKind>();
            assert source!=null:"JavaSource.forFileObject(" + tph.getFileObject().getPath() + ") \n returned null";
            source.runUserActionTask(new Task<CompilationController>() {

                public void run(CompilationController cc) throws Exception {
                    Element e = tph.resolveElement(cc);
                    kind.set(e.getKind());
                }
            }, true);
            return kind.get();
        } catch (IOException ex) {
            throw (RuntimeException) new RuntimeException().initCause(ex);
        }
    }

    public static String getPackageName(FileObject folder) {
        assert folder.isFolder() : "argument must be folder";
        ClassPath cp = ClassPath.getClassPath(folder, ClassPath.SOURCE);
        if (cp == null) {
            // see http://www.netbeans.org/issues/show_bug.cgi?id=159228
            throw new IllegalStateException(String.format("No classpath for %s.", folder)); // NOI18N
        }
        return cp.getResourceName(folder, '.', false);
    }

    public static String getPackageName(UnitTree unit) {
        assert unit!=null;
        ExpressionTree name = unit.getPackageName();
        if (name==null) {
            //default package
            return "";
        }
        return name.toString();
    }

    public static String getPackageName(URL url) {
        File f = null;
        try {
            String path = URLDecoder.decode(url.getPath(), "utf-8"); // NOI18N
            f = FileUtil.normalizeFile(new File(path));
        } catch (UnsupportedEncodingException u) {
            throw new IllegalArgumentException("Cannot create package name for url " + url); // NOI18N
        }
        String suffix = "";

        do {
            FileObject fo = FileUtil.toFileObject(f);
            if (fo != null) {
                if ("".equals(suffix))
                    return getPackageName(fo);
                String prefix = getPackageName(fo);
                return prefix + ("".equals(prefix)?"":".") + suffix; // NOI18N
            }
            if (!"".equals(suffix)) {
                suffix = "." + suffix; // NOI18N
            }
            try {
                suffix = URLDecoder.decode(f.getPath().substring(f.getPath().lastIndexOf(File.separatorChar) + 1), "utf-8") + suffix; // NOI18N
            } catch (UnsupportedEncodingException u) {
                throw new IllegalArgumentException("Cannot create package name for url " + url); // NOI18N
            }
            f = f.getParentFile();
        } while (f!=null);
        throw new IllegalArgumentException("Cannot create package name for url " + url); // NOI18N
    }

    public static PathKind toJava(ClasspathInfo.PathKind kind) {
        return PathKind.valueOf(kind.name());
    }

    public static org.netbeans.api.java.source.ClasspathInfo toJava(ClasspathInfo cpInfo) {
        return org.netbeans.api.java.source.ClasspathInfo.create(cpInfo.getClassPath(ClasspathInfo.PathKind.BOOT), cpInfo.getClassPath(ClasspathInfo.PathKind.COMPILE), cpInfo.getClassPath(ClasspathInfo.PathKind.SOURCE));
    }

    public static FileObject getFile(Element element, ClasspathInfo cpInfo) {
        ClassIndex ci = cpInfo.getClassIndex();

        Set<FileObject> files = ci.getResources(ElementHandle.create(element), EnumSet.of(ClassIndex.SearchKind.TYPE_DEFS), EnumSet.of(ClassIndex.SearchScope.SOURCE));
        if (!files.isEmpty()) return files.iterator().next();

        return null;
    }
    
    /**
     * Tests whether the initial scan is in progress.
     */
    public static boolean isScanInProgress () {
        return IndexingManager.getDefault().isIndexing();
    }

    /**
     * Waits for the end of the initial scan, this helper method 
     * is designed for tests which require to wait for end of initial scan.
     * @throws InterruptedException is thrown when the waiting thread is interrupted.
     * @deprecated use {@link JavaSource#runWhenScanFinished}
     */
    public static void waitScanFinished () throws InterruptedException {
        try {
            class T extends UserTask implements ClasspathInfoProvider {
                private final ClassPath EMPTY_PATH = ClassPathSupport.createClassPath(new URL[0]);
                private final ClasspathInfo cpinfo = ClasspathInfo.create(EMPTY_PATH, EMPTY_PATH, EMPTY_PATH);
                @Override
                public void run(ResultIterator resultIterator) throws Exception {
                    // no-op
                }

                public ClasspathInfo getClasspathInfo() {
                    return cpinfo;
                }
            }
            Future<Void> f = ParserManager.parseWhenScanFinished(JAVAFX_MIME_TYPE, new T());
            if (!f.isDone()) {
                f.get();
            }
        } catch (Exception ex) {
        }
    }

    /**
     * This is a helper method to provide support for delaying invocations of actions
     * depending on java model. See <a href="http://java.netbeans.org/ui/waitscanfinished.html">UI Specification</a>.
     * <br>Behavior of this method is following:<br>
     * If classpath scanning is not in progress, runnable's run() is called. <br>
     * If classpath scanning is in progress, modal cancellable notification dialog with specified
     * tile is opened.
     * </ul>
     * As soon as classpath scanning finishes, this dialog is closed and runnable's run() is called.
     * This method must be called in AWT EventQueue. Runnable is performed in AWT thread.
     *
     * @param runnable Runnable instance which will be called.
     * @param actionName Title of wait dialog.
     * @return true action was cancelled <br>
     *         false action was performed
     */
    public static boolean invokeAfterScanFinished(final Runnable runnable , final String actionName) {
        assert SwingUtilities.isEventDispatchThread();
        if (SourceUtils.isScanInProgress()) {
            final ActionPerformer ap = new ActionPerformer(runnable);
            ActionListener listener = new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    ap.cancel();
                    waitTask.cancel();
                }
            };
            JLabel label = new JLabel(getString("MSG_WaitScan"), javax.swing.UIManager.getIcon("OptionPane.informationIcon"), SwingConstants.LEFT);
            label.setBorder(new EmptyBorder(12,12,11,11));
            DialogDescriptor dd = new DialogDescriptor(label, actionName, true, new Object[]{getString("LBL_CancelAction", new Object[]{actionName})}, null, 0, null, listener);
            waitDialog = DialogDisplayer.getDefault().createDialog(dd);
            waitDialog.pack();
            //100ms is workaround for 127536
            waitTask = RequestProcessor.getDefault().post(ap, 100);
            waitDialog.setVisible(true);
            waitTask = null;
            waitDialog = null;
            return ap.hasBeenCancelled();
        } else {
            runnable.run();
            return false;
        }
    }

    private static String getString(String key) {
        return NbBundle.getMessage(SourceUtils.class, key);
    }

    private static String getString(String key, Object values) {
        return new MessageFormat(getString(key)).format(values);
    }

    private static Dialog waitDialog = null;
    private static RequestProcessor.Task waitTask = null;

    private static class ActionPerformer implements Runnable {
        private Runnable action;
        private boolean cancel = false;

        ActionPerformer(Runnable a) {
            this.action = a;
        }

        public boolean hasBeenCancelled() {
            return cancel;
        }

        public void run() {
            try {
                SourceUtils.waitScanFinished();
            } catch (InterruptedException ie) {
                Exceptions.printStackTrace(ie);
            }
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    if (!cancel) {
                        if (waitDialog != null) {
                            waitDialog.setVisible(false);
                            waitDialog.dispose();
                        }
                        action.run();
                    }
                }
            });
        }

        public void cancel() {
            assert SwingUtilities.isEventDispatchThread();
            // check if the scanning did not finish during cancel
            // invocation - in such case do not set cancel to true
            // and do not try to hide waitDialog window
            if (waitDialog != null) {
                cancel = true;
                waitDialog.setVisible(false);
                waitDialog.dispose();
            }
        }
    }
}
