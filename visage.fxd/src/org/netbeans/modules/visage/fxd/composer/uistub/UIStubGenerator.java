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

package org.netbeans.modules.visage.fxd.composer.uistub;

import com.sun.visage.tools.fxd.FXDObjectElement;
import com.sun.visage.tools.fxd.container.FXDContainer;
import com.sun.visage.tools.fxd.container.generator.FXWriter;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Date;
import java.util.StringTokenizer;
import javax.swing.SwingUtilities;
import javax.swing.text.AttributeSet;
import org.netbeans.modules.visage.fxd.composer.model.FXDFileModel;
import org.netbeans.modules.visage.fxd.dataloader.fxz.FXZDataObject;
import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.cookies.OpenCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.util.NbBundle;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.netbeans.modules.editor.structure.api.DocumentElement;
import org.netbeans.modules.visage.fxd.composer.misc.FXDComposerUtils;
import org.netbeans.modules.visage.fxd.composer.model.FXDComposerModel;
import org.netbeans.modules.visage.fxd.schemamodel.FXDSchemaModelProvider;


/**
 *
 * @author Pavel Benes
 */
public final class UIStubGenerator {
    static final String JAVAFX_EXTENSION = ".fx";    //NOI18N
    static final String UI_STUB_EXTENSION = "UI" + JAVAFX_EXTENSION;  //NOI18N
    private static final String PACKAGE_SEPARATOR = ".";      //NOI18N
    static final String PACKAGE_NOT_SET = "<enter-your-package-here>";   //NOI18N
    
    private final FXZDataObject   m_dObj;
    
    public UIStubGenerator( final FXZDataObject dObj) {
        m_dObj = dObj;
    }

    public String getFileName() {
        return m_dObj.getPrimaryFile().getName();
    }

    public String[] getEntryNames() {
        return m_dObj.getDataModel().getFXDContainer().getEntryNames();
    }

    public File getInitialDirectory() {
        try {
            FileObject fo = m_dObj.getPrimaryFile();
            return FileUtil.toFile(fo).getParentFile();
        } catch( Exception e) {
            e.printStackTrace();
            return null;
        }
    }
    
    public void generate() {
        FileObject fo = m_dObj.getPrimaryFile();
        
        if ( fo.hasExt(FXZDataObject.FXZ_EXT)) {
            File file = FileUtil.toFile(fo);
            try {
                UIStubGeneratorPanel panel = new UIStubGeneratorPanel(this);
                
                File parent = file.getParentFile();
                panel.setStubLocation( parent.getAbsolutePath());
                panel.setPackagePath( getPackagePath(parent));
                                
                DialogDescriptor dd = new DialogDescriptor(panel, NbBundle.getMessage(UIStubGenerator.class, "TITLE_UIStubGeneration"));  //NOI18N

                Dialog dlg = DialogDisplayer.getDefault().createDialog(dd);
                setDialogMinimumSize( dlg);
                dlg.setVisible(true);

                if (dd.getValue() == DialogDescriptor.OK_OPTION) {
                    String stubLoc = panel.getStubLocation();

                    String packageName = panel.getPackagePath();
                    if ( !isValidPackagePath(packageName)) {
                        NotifyDescriptor d = new NotifyDescriptor.Message(
                            NbBundle.getMessage(UIStubGenerator.class, "MSG_INVALID_UI_STUB_PACKAGE_NAME", packageName),  //NOI18N
                            NotifyDescriptor.ERROR_MESSAGE);
                        DialogDisplayer.getDefault().notify(d);
                        return;
                    }

                    File stubDir = new File(stubLoc);
                    if ( !stubDir.isDirectory()) {
                        NotifyDescriptor d = new NotifyDescriptor.Message(
                            NbBundle.getMessage(UIStubGenerator.class, "MSG_UI_STUB_FOLDER_IS_NOT_DIR", stubLoc),  //NOI18N
                            NotifyDescriptor.ERROR_MESSAGE);
                        DialogDisplayer.getDefault().notify(d);
                        return;
                    } 

                    Map<String,String> selected = panel.getSelectedEntries();

                    if ( selected.isEmpty()) {
                        NotifyDescriptor d = new NotifyDescriptor.Message(
                            NbBundle.getMessage(UIStubGenerator.class, "MSG_NO_UI_STUBS_SELECTED"),  //NOI18N
                            NotifyDescriptor.INFORMATION_MESSAGE);
                        DialogDisplayer.getDefault().notify(d);
                        return;
                    } else {
                        Set<String>    uiStubNames   = new HashSet<String>(selected.size());
                        StringBuilder  existingFiles = new StringBuilder();

                        for ( String uiStubName : selected.values()) {
                            if ( !isValidStubName(uiStubName)) {
                                NotifyDescriptor d = new NotifyDescriptor.Message(
                                    NbBundle.getMessage(UIStubGenerator.class, "MSG_INVALID_UI_STUB_NAME", uiStubName),  //NOI18N
                                    NotifyDescriptor.ERROR_MESSAGE);
                                DialogDisplayer.getDefault().notify(d);
                                return;
                            }
                            if ( !uiStubNames.add(uiStubName) ) {
                                NotifyDescriptor d = new NotifyDescriptor.Message(
                                    NbBundle.getMessage(UIStubGenerator.class, "MSG_DUPLICATED_UI_STUB_NAME", uiStubName),  //NOI18N
                                    NotifyDescriptor.ERROR_MESSAGE);
                                DialogDisplayer.getDefault().notify(d);
                                return;
                            }
                            File uiStubfile = new File( stubDir, uiStubName);
                            if ( uiStubfile.isDirectory()) {
                                NotifyDescriptor d = new NotifyDescriptor.Message(
                                    NbBundle.getMessage(UIStubGenerator.class, "MSG_UI_STUB_IS_DIR", uiStubfile.getAbsolutePath()),  //NOI18N
                                    NotifyDescriptor.ERROR_MESSAGE);
                                DialogDisplayer.getDefault().notify(d);
                                return;
                            }
                            if ( uiStubfile.exists()) {
                                existingFiles.append("\t");
                                existingFiles.append(uiStubfile.getAbsolutePath());
                                existingFiles.append("\n");
                            }
                        }

                        if ( existingFiles.length() > 0) {
                            NotifyDescriptor d = new NotifyDescriptor.Confirmation(
                                NbBundle.getMessage(UIStubGenerator.class, "MSG_FILE_EXISTS_MSG", existingFiles.toString()),  //NOI18N
                                NbBundle.getMessage(UIStubGenerator.class, "MSG_FILE_EXISTS_TITLE"), //NOI18N
                                NotifyDescriptor.YES_NO_OPTION,
                                NotifyDescriptor.WARNING_MESSAGE);
                            if ( DialogDisplayer.getDefault().notify(d) != NotifyDescriptor.YES_OPTION) {
                                return;
                            }
                        }

                        implGenerate( fo.getNameExt(), stubLoc, packageName, selected, panel.generateWarnings());
                    }
                }
            } catch( Exception e) {
                e.printStackTrace();
            }
        } else {
            throw new IllegalArgumentException( );
        }        
    }

//                        if ( stubFile.exists()) {
//                            NotifyDescriptor d = new NotifyDescriptor.Confirmation(
//                                NbBundle.getMessage(UIStubGenerator.class, "MSG_FILE_EXISTS_MSG", stubFile.getAbsoluteFile()),  //NOI18N
//                                NbBundle.getMessage(UIStubGenerator.class, "MSG_FILE_EXISTS_TITLE"), //NOI18N
//                                NotifyDescriptor.YES_NO_OPTION,
//                                NotifyDescriptor.WARNING_MESSAGE);
//                            if ( DialogDisplayer.getDefault().notify(d) != NotifyDescriptor.YES_OPTION) {
//                                return;
//                            }
//                        }
//

    private void implGenerate( final String archiveName, final String stubLocation, 
            final String packagePath, final Map<String,String> selected, final boolean generateWarning) {
        Thread th = new Thread() {
            @Override
            public void run() {
                try {
                    FXDComposerModel dataModel = m_dObj.getDataModel();
                    File uiStubFile = null;

                    for ( String entry : selected.keySet()) {
                        // force the model update if changed lately
                        FXDFileModel model = dataModel.getFXDContainer().getFileModel( entry);
                        model.updateModel();

                        AttributeHolder attributes = new AttributeHolder();
                        attributes.setStrictTypes(false);

                        visitFXDModel(model, attributes);

                        attributes.processNames();
                        uiStubFile = new File(stubLocation, selected.get(entry));
                        writeUIStub( archiveName, uiStubFile, packagePath, entry, attributes, generateWarning);
                    }

                    final File singleFile = selected.size() == 1 ? uiStubFile : null;
                    SwingUtilities.invokeLater( new Runnable() {
                        public void run() {
                            if ( singleFile == null) {
                                NotifyDescriptor d = new NotifyDescriptor.Message(
                                    NbBundle.getMessage(UIStubGenerator.class, "MSG_STUBS_CREATED"),  //NOI18N
                                    NotifyDescriptor.INFORMATION_MESSAGE);
                                    DialogDisplayer.getDefault().notify(d);
                            } else {
                                String path;
                                try {
                                    path = singleFile.getCanonicalPath();
                                } catch( IOException e) {
                                    path = singleFile.getAbsolutePath();
                                }
                                NotifyDescriptor d = new NotifyDescriptor.Confirmation(
                                        NbBundle.getMessage(UIStubGenerator.class, "MSG_STUB_CREATED_WANA_SEE", path), //NOI18N
                                        NbBundle.getMessage(UIStubGenerator.class, "TITLE_STUB_CREATED"),  //NOI18N
                                        NotifyDescriptor.YES_NO_OPTION,
                                        NotifyDescriptor.INFORMATION_MESSAGE);
                                if ( DialogDisplayer.getDefault().notify(d) == DialogDescriptor.YES_OPTION) {
                                    FileObject uiStubFO = FileUtil.toFileObject(singleFile);
                                    uiStubFO.refresh();
                                    try {
                                        DataObject uiStubDO = DataObject.find(uiStubFO);
                                        OpenCookie cookie = uiStubDO.getCookie( OpenCookie.class);
                                        cookie.open();
                                    } catch( Exception e) {
                                        e.printStackTrace();
                                        NotifyDescriptor.Exception ne = new NotifyDescriptor.Exception(e);
                                        DialogDisplayer.getDefault().notifyLater(ne);
                                    }
                                }
                            }
                        }
                    });
                } catch( Exception e) {
                    e.printStackTrace();
                    NotifyDescriptor.Exception ne = new NotifyDescriptor.Exception(e);
                    DialogDisplayer.getDefault().notifyLater(ne);
                }
            }
        };
        
        th.setPriority( Thread.MIN_PRIORITY);
        th.start();
    }

    private void visitFXDModel(FXDFileModel model, AttributeHolder attributes) throws Exception {
        FXDFileModel.ElementVisitor visitor = new ElementVisitorImpl(attributes);
        DocumentElement de = model.getDocumentModel().getRootElement().getElement(0);
        // for FXD root node process "FXD" itself and "content" only
        if (de.getName().equalsIgnoreCase(FXDSchemaModelProvider.FXD_ROOT_NAME)) {
            visitor.visitElement(de.getType(), de.getName(), de.getAttributes());
            List<DocumentElement> children = de.getChildren();
            for (DocumentElement cde : children) {
                if (cde.getName().equalsIgnoreCase(FXDSchemaModelProvider.FXD_CONTENT_NAME)) {
                    de = cde;
                    break;
                }
            }
        }
        model.visitElements(de, visitor);
    }

    private static class ElementVisitorImpl implements FXDFileModel.ElementVisitor {

        private AttributeHolder m_attributes;

        public ElementVisitorImpl(AttributeHolder attributes) {
            m_attributes = attributes;
        }

        public boolean visitElement(String elemType, String elemName, AttributeSet attrs) throws Exception {
            String id = (String) attrs.getAttribute(FXDObjectElement.ATTR_NAME_ID);
            int len;
            if (id != null && (len = id.length()) > 0) {
                if (len > 2 && id.charAt(0) == '"' && id.charAt(len - 1) == '"') { // NOI18N
                    m_attributes.add(id.substring(1, len - 1), elemName);
                } else {
                    System.err.println("Invalid node id: " + id);  //NOI18N
                }
            }
            return true;
        }
    }

    private void writeUIStub( final String archiveName, final File stubFile,
            String packagePath, String entry, AttributeHolder attrs, boolean generateWarning) throws FileNotFoundException, IOException, AttributeConflictException {
        FXWriter writer = new FXWriter(stubFile);

        String className = FXDComposerUtils.getFileName(stubFile);

        writer.write( "/*"); //NOI18N 
        writer.write( " * Generated by Visage Production Suite NetBeans plugin.");  //NOI18N 
        writer.write( " * " + stubFile.getName());  //NOI18N 
        writer.write( " *");  //NOI18N 
        writer.write( " * Created on " + new Date());  //NOI18N 
        writer.write( " */");  //NOI18N 
        writer.write( "package " + packagePath + ";");  //NOI18N 
        writer.write( "");  //NOI18N 
        writer.write( "import java.lang.*;");  //NOI18N
        writer.write( "import visage.scene.Node;");  //NOI18N 
        writer.write( "import visage.fxd.FXDNode;");  //NOI18N
        writer.write( "");  //NOI18N 

        writer.write( "public class " + className + " extends FXDNode {");  //NOI18N

        writer.increaseIndent();
        writer.write("");  //NOI18N
        String reference;
        if ( entry == null || FXDContainer.MAIN_CONTENT.equals(entry)) {
            reference = archiveName;
        } else {
            reference = archiveName + "#" + entry;
        }
        writer.write( "override public var url = \"{__DIR__}" + reference + "\";");  //NOI18N

        writer.write("");  //NOI18N 
        
        attrs.serializeDeclarations(writer, true);

        writer.write("");  //NOI18N 

        writer.write("override protected function contentLoaded() : Void {"); //NOI18N
        writer.increaseIndent();
        attrs.serializeUpdates(writer, true);
        writer.decreaseIndent();
        writer.write("}");   //NOI18N

        if ( generateWarning) {
            writer.write( "");  //NOI18N
            writer.write( "/**");  //NOI18N
            writer.write( " * Check if some element with given id exists and write ");  //NOI18N
            writer.write( " * a warning if the element could not be found.");  //NOI18N
            writer.write( " * The whole method can be removed if such warning is not required.");  //NOI18N
            writer.write( " */");  //NOI18N
            writer.write( "protected override function getObject( id:String) : Object {");
            writer.increaseIndent();
            writer.write( "var obj = super.getObject(id);"); //NOI18N
            writer.write( "if ( obj == null) {"); //NOI18N
            writer.increaseIndent();
            writer.write( "System.err.println(\"WARNING: Element with id {id} not found in {url}\");"); //NOI18N
            writer.decreaseIndent();
            writer.write("}");   //NOI18N
            writer.write( "return obj;"); //NOI18N
            writer.decreaseIndent();
            writer.write("}");   //NOI18N
        }

        writer.decreaseIndent();
        writer.write( "}");  //NOI18N 
        writer.write("");  //NOI18N 

        writer.close();
    }
                
    static String getPackagePath(final File file) {
        StringBuilder pName = new StringBuilder();

        File fp=file;
        do {
            String dirName = fp.getName();
            if (dirName.equals("src")) {  //NOI18N
                return pName.toString();
            } else {
                if ( pName.length() > 0) {
                    pName.insert(0, '.');
                }
                pName.insert( 0, dirName);
            }
        } while ((fp=fp.getParentFile()) != null);
        return PACKAGE_NOT_SET;        
    }  
    
    public static void setDialogMinimumSize(final Dialog dlg) {
        dlg.pack();
        dlg.setSize( dlg.getPreferredSize());
        
        dlg.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                int w = dlg.getWidth();
                int h = dlg.getHeight();
                final Dimension minSize = dlg.getPreferredSize();

                int _w = Math.max( w, minSize.width);
                int _h = Math.max( h, minSize.height);

                if ( w != _w || h != _h) {
                    dlg.setSize( new Dimension(_w, _h));
                }
            }
        });
    }

    private static boolean isValidStubName( final String stubFileName) {
        if ( stubFileName != null && stubFileName.length() > JAVAFX_EXTENSION.length()) {
            File file = new File(stubFileName);
            String name = file.getName();
            if ( !name.endsWith(JAVAFX_EXTENSION)) {
                return false;
            }
            if ( isJavaIdentifier( name.substring(0, name.length() - JAVAFX_EXTENSION.length()))) {
                return true;
            }
        }
        return false;
    }
    
    private static boolean isValidPackagePath( final String packagePath) {
        if ( packagePath != null && packagePath.length() > 0) {
            if ( PACKAGE_NOT_SET.endsWith(packagePath)) {
                return true;
            }
            if ( !packagePath.startsWith(PACKAGE_SEPARATOR) && !packagePath.endsWith(PACKAGE_SEPARATOR)) {
                StringTokenizer st = new StringTokenizer(packagePath, PACKAGE_SEPARATOR);
                if ( st.countTokens() > 0) {
                    while( st.hasMoreTokens()) {
                        if ( !isJavaIdentifier( st.nextToken())) {
                            return false;
                        }
                    }
                    return true;
                }
            }
        }
        return false;
    }

    private static boolean isJavaIdentifier(final String  str) {
          int n = str.length();
          if (n==0) return false;
          if (!Character.isJavaIdentifierStart(str.charAt(0)))
              return false;
          for (int i = 1; i < n; i++)
              if (!Character.isJavaIdentifierPart(str.charAt(i)))
                  return false;
          return true;
    }
}
