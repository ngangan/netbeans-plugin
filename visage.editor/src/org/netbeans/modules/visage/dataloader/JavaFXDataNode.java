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

package org.netbeans.modules.visage.dataloader;

import java.awt.Image;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URL;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.queries.FileBuiltQuery;
import org.netbeans.api.queries.FileBuiltQuery.Status;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataNode;
import org.openide.loaders.DataObject;
import org.openide.nodes.Children;
import org.openide.nodes.Node;
import org.openide.nodes.PropertySupport;
import org.openide.nodes.Sheet;
import org.openide.util.HelpCtx;
import org.openide.util.ImageUtilities;
import org.openide.util.NbBundle;
import org.openide.util.RequestProcessor;
import org.openide.util.WeakListeners;

import static org.openide.util.ImageUtilities.assignToolTipToImage;
import static org.openide.util.ImageUtilities.loadImage;
import static org.openide.util.NbBundle.getMessage;

/**
 *
 * @author answer
 */

public class VisageDataNode extends DataNode implements ChangeListener{

    private static final String FX_ICON_BASE = "org/netbeans/modules/visage/dataloader/FX-filetype.png"; // NOI18N
    private static final String CLASS_ICON_BASE = "org/netbeans/modules/visage/dataloader/FX-filetype.png"; // NOI18N

    private static final String NEEDS_COMPILE_BADGE_URL = "org/netbeans/modules/visage/dataloader/resources/needs-compile.png"; // NOI18N
    private static final Image NEEDS_COMPILE;
    
    private Status status;
    private final AtomicBoolean isCompiled;
    private ChangeListener executableListener;
//    private final AtomicBoolean isExecutable;

    static{
        URL needsCompileIconURL = VisageDataNode.class.getClassLoader().getResource(NEEDS_COMPILE_BADGE_URL);
        String needsCompileTP = "<img src=\"" + needsCompileIconURL + "\">&nbsp;" + getMessage(VisageDataNode.class, "TP_NeedsCompileBadge"); // NOI18N
        NEEDS_COMPILE = assignToolTipToImage(loadImage(NEEDS_COMPILE_BADGE_URL), needsCompileTP); // NOI18N
    }

    /** Create a node for the Java data object using the default children.
    * @param jdo the data object to represent
    */
    public VisageDataNode (final DataObject jdo, boolean isVisageSource) {
        super (jdo, Children.LEAF);
        setIconBaseWithExtension(isVisageSource ? FX_ICON_BASE : CLASS_ICON_BASE);
        Logger.getLogger("TIMER").log(Level.FINE, "VisageNode", new Object[] {jdo.getPrimaryFile(), this}); // NOI18N
        
        if (isVisageSource) {
            this.isCompiled = new AtomicBoolean(true);                                        
            WORKER.post(new BuildStatusTask(this));
//            this.isExecutable = new AtomicBoolean(false);
//            WORKER.post(new ExecutableTask(this));
            
            jdo.addPropertyChangeListener(new PropertyChangeListener() {
                public void propertyChange(PropertyChangeEvent evt) {
                    if (DataObject.PROP_PRIMARY_FILE.equals(evt.getPropertyName())) {
                        Logger.getLogger("TIMER").log(Level.FINE, "VisageDataNode", new Object[]{jdo.getPrimaryFile(), this}); // NOI18N
                        WORKER.post(new Runnable() {
                            public void run() {
                                synchronized (VisageDataNode.this) {
                                    status = null;
                                    executableListener = null;
                                    WORKER.post(new BuildStatusTask(VisageDataNode.this));
  //                                  WORKER.post(new ExecutableTask(VisageDataNode.this));
                                }
                            }
                        });
                    }
                }
            });
        } else {
            this.isCompiled = null;
//            this.isExecutable = null;
        }
  
    }

    @Override
    public HelpCtx getHelpCtx() {
        return new HelpCtx("visage_editing_source"); // NOI18N
    }


/*    
    public void setName(String name) {
        RenameHandler handler = getRenameHandler();
        if (handler == null) {
            super.setName(name);
        } else {
            try {
                handler.handleRename(VisageDataNode.this, name);
            } catch (IllegalArgumentException ioe) {
                super.setName(name);
            }
        }
  
    }
*/
/*    
    private static synchronized RenameHandler getRenameHandler() {
        Collection<? extends RenameHandler> handlers = (Lookup.getDefault().lookupAll(RenameHandler.class)) ;
        if (handlers.size()==0)
            return null;
        if (handlers.size()>1)
            ErrorManager.getDefault().log(ErrorManager.WARNING, "Multiple instances of RenameHandler found in Lookup; only using first one: " + handlers); //NOI18N
        return handlers.iterator().next();
    }
*/    
    /** Create the property sheet.
     * @return the sheet
     */
    @Override
    protected final Sheet createSheet () {
        Sheet sheet = super.createSheet();
        
        //if there is any rename handler installed
        //push under our own property
//        if (getRenameHandler() != null)
//            sheet.get(Sheet.PROPERTIES).put(createNameProperty());
        
        // Add classpath-related properties.
        Sheet.Set ps = new Sheet.Set();
        ps.setName("classpaths"); // NOI18N
        ps.setDisplayName(NbBundle.getMessage(VisageDataNode.class, "LBL_VisageDataNode_sheet_classpaths")); // NOI18N
        ps.setShortDescription(NbBundle.getMessage(VisageDataNode.class, "HINT_VisageDataNode_sheet_classpaths")); // NOI18N
        ps.put(new Node.Property[] {
            new ClasspathProperty(ClassPath.COMPILE,
                    NbBundle.getMessage(VisageDataNode.class, "PROP_VisageDataNode_compile_classpath"), // NOI18N
                    NbBundle.getMessage(VisageDataNode.class, "HINT_VisageDataNode_compile_classpath")), // NOI18N
                    new ClasspathProperty(ClassPath.EXECUTE,
                    NbBundle.getMessage(VisageDataNode.class, "PROP_VisageDataNode_execute_classpath"), // NOI18N
                    NbBundle.getMessage(VisageDataNode.class, "HINT_VisageDataNode_execute_classpath")), // NOI18N
                    new ClasspathProperty(ClassPath.BOOT,
                    NbBundle.getMessage(VisageDataNode.class, "PROP_VisageDataNode_boot_classpath"), // NOI18N
                    NbBundle.getMessage(VisageDataNode.class, "HINT_VisageDataNode_boot_classpath")), // NOI18N
        });
        sheet.put(ps);
        return sheet;
    }
    
    private Node.Property createNameProperty () {
        Node.Property p = new PropertySupport.ReadWrite<String> (
                DataObject.PROP_NAME,
                String.class,
                NbBundle.getMessage (DataObject.class, "PROP_name"), // NOI18N
                NbBundle.getMessage (DataObject.class, "HINT_name") // NOI18N
                ) {
            public String getValue () {
                return VisageDataNode.this.getName();
            }
            @Override
            public Object getValue(String key) {
                if ("suppressCustomEditor".equals (key)) { //NOI18N
                    return Boolean.TRUE;
                } else {
                    return super.getValue (key);
                }
            }
            public void setValue(String val) throws IllegalAccessException,
                    IllegalArgumentException, InvocationTargetException {
                if (!canWrite())
                    throw new IllegalAccessException();
                VisageDataNode.this.setName(val);
            }
            @Override
            public boolean canWrite() {
                return VisageDataNode.this.canRename();
            }
            
        };
        
        return p;
    }
    
    /**
     * Displays one kind of classpath for this Java source.
     * Tries to use the normal format (directory or JAR names), falling back to URLs if necessary.
     */
    private final class ClasspathProperty extends PropertySupport.ReadOnly<String> {
        
        private final String id;
        
        public ClasspathProperty(String id, String displayName, String shortDescription) {
            super(id, /*XXX NbClassPath would be preferable, but needs org.openide.execution*/String.class, displayName, shortDescription);
            this.id = id;
            // XXX the following does not always work... why?
            setValue("oneline", false); // NOI18N
        }
        
        public String getValue() {
            ClassPath cp = ClassPath.getClassPath(getDataObject().getPrimaryFile(), id);
            if (cp != null) {
                StringBuffer sb = new StringBuffer();
                for (ClassPath.Entry entry : cp.entries()) {
                    URL u = entry.getURL();
                    String item = u.toExternalForm(); // fallback
                    if (u.getProtocol().equals("file")) { // NOI18N
                        item = new File(URI.create(item)).getAbsolutePath();
                    } else if (u.getProtocol().equals("jar") && item.endsWith("!/")) { // NOI18N
                        URL embedded = FileUtil.getArchiveFile(u);
                        assert embedded != null : u;
                        if (embedded.getProtocol().equals("file")) { // NOI18N
                            item = new File(URI.create(embedded.toExternalForm())).getAbsolutePath();
                        }
                    }
                    if (sb.length() > 0) {
                        sb.append(File.pathSeparatorChar);
                    }
                    sb.append(item);
                }
                return sb.toString();
            } else {
                return NbBundle.getMessage(VisageDataNode.class, "LBL_VisageDataNode_classpath_unknown"); // NOI18N
            }
        }
    }

    public void stateChanged(ChangeEvent e) {
        WORKER.post(new BuildStatusTask(this));
    }
    
    @Override
    public Image getIcon(int type) {
        Image i = super.getIcon(type);
        
        return enhanceIcon(i);
    }
    
    @Override
    public Image getOpenedIcon(int type) {
        Image i = super.getOpenedIcon(type);
        
        return enhanceIcon(i);
    }
    
    private Image enhanceIcon(Image i) {
        if (isCompiled != null && !isCompiled.get()) {
            i = ImageUtilities.mergeImages(i, NEEDS_COMPILE, 16, 0);
        }
        
        return i;
    }
    
    private static final RequestProcessor WORKER = new RequestProcessor("Visage Node Badge Processor", 1); // NOI18N
    
    private static class BuildStatusTask implements Runnable {
        private final VisageDataNode node;
        
        public BuildStatusTask(VisageDataNode node) {
            this.node = node;
        }

        public void run() {
            Status _status = null;
            synchronized (node) {
                _status = node.status;
            }            
            if (_status == null) {
                FileObject jf = node.getDataObject().getPrimaryFile();
                _status = FileBuiltQuery.getStatus(jf);                
                synchronized (node) {
                    if (_status != null && node.status == null) {
                        node.status = _status;
                        node.status.addChangeListener(WeakListeners.change(node, node.status));
                    }
                }
            }
            
            boolean newIsCompiled = _status != null ? _status.isBuilt() : true;
            boolean oldIsCompiled = node.isCompiled.getAndSet(newIsCompiled);

            if (newIsCompiled != oldIsCompiled) {
                node.fireIconChange();
                node.fireOpenedIconChange();
            }
        }
    }
    
/*    
    private static class ExecutableTask implements Runnable {
        private final VisageDataNode node;
        
        public ExecutableTask(VisageDataNode node) {
            this.node = node;
        }

        public void run() {
            ChangeListener _executableListener;
            
            synchronized (node) {
                _executableListener = node.executableListener;
            }
            
            FileObject file = node.getDataObject().getPrimaryFile();

            if (_executableListener == null) {
                _executableListener = new ChangeListener() {
                    public void stateChanged(ChangeEvent e) {
                        WORKER.post(new ExecutableTask(node));
                    }
                };
                
                try {
                    ExecutableFilesIndex.DEFAULT.addChangeListener(file.getURL(), _executableListener);
                } catch (FileStateInvalidException ex) {
                    Exceptions.printStackTrace(ex);
                }
                
                synchronized (node) {
                    if (node.executableListener == null) {
                        node.executableListener = _executableListener;
                    }
                }
            }
            
            ClassPath cp = ClassPath.getClassPath(file, ClassPath.SOURCE);
            FileObject root = cp != null ? cp.findOwnerRoot(file) : null;
            
            if (root != null) {
                try {
                    boolean newIsExecutable = ExecutableFilesIndex.DEFAULT.isMainClass(root.getURL(), file.getURL());
                    boolean oldIsExecutable = node.isExecutable.getAndSet(newIsExecutable);

                    if (newIsExecutable != oldIsExecutable) {
                        node.fireIconChange();
                        node.fireOpenedIconChange();
                    }
                } catch (FileStateInvalidException ex) {
                    Exceptions.printStackTrace(ex);
                }
            }
        }
    }
*/    
}
