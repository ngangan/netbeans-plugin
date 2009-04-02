/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.uistub;

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
import org.netbeans.modules.javafx.fxd.composer.model.FXDFileModel;
import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.cookies.OpenCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.util.NbBundle;

import com.sun.javafx.tools.fxd.container.generator.*;
import org.netbeans.modules.javafx.fxd.composer.model.FXDComposerModel;


/**
 *
 * @author Pavel Benes
 */
public final class UIStubGenerator {
    private static final String JAVAFX_EXTENSION = ".fx";    //NOI18N
    private static final String UI_STUB_EXTENSION = "UI" + JAVAFX_EXTENSION;  //NOI18N
    private static final String PACKAGE_SEPARATOR = ".";      //NOI18N
    static final String PACKAGE_NOT_SET = "<enter-your-package-here>";   //NOI18N

    private static final class AttributeDescription implements NamedElement {
        private String m_type;
        private String m_name;
        
        public AttributeDescription( String type, String name) {
            m_type = type;
            m_name = name;
            //System.err.println(toString());
        }
        
        public String getName() {
            return m_name;
        }

        public String getType() {
            return m_type;
        }

        public void setName(String name) {
            m_name = name;
        }   
        
        @Override
        public String toString() {
            return String.format("AttributeDescription( type='%s', name='%s')", m_type, m_name); //NOI18N
        }
    }
    
    private final FXZDataObject   m_dObj;
    private final AttributeHolder m_attrs;
    
    public UIStubGenerator( final FXZDataObject dObj) {
        m_dObj = dObj;
        m_attrs = new AttributeHolder();
        m_attrs.setStrictTypes(false);        
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
        FileObject fo   = m_dObj.getPrimaryFile();
        String     name = fo.getName();
        
        if ( fo.hasExt(FXZDataObject.FXZ_EXT)) {
            File file = FileUtil.toFile(fo);
            try {
                UIStubGeneratorPanel panel = new UIStubGeneratorPanel(this);
                
                File parent = file.getParentFile();
                File stubFile = new File( parent, name + UI_STUB_EXTENSION);
                
                panel.setStubLocation( stubFile.getAbsolutePath());
                panel.setPackagePath( getPackagePath(stubFile));
                                
                DialogDescriptor dd = new DialogDescriptor(panel, NbBundle.getMessage(UIStubGenerator.class, "TITLE_UIStubGeneration"));  //NOI18N

                Dialog dlg = DialogDisplayer.getDefault().createDialog(dd);
                setDialogMinimumSize( dlg);
                dlg.setVisible(true);

                if (dd.getValue() == DialogDescriptor.OK_OPTION) {
                    String stubLoc = panel.getStubLocation();
                    if ( !isValidStubName(stubLoc)) {
                        NotifyDescriptor d = new NotifyDescriptor.Message(
                            NbBundle.getMessage(UIStubGenerator.class, "MSG_INVALID_UI_STUB_FILENAME", stubLoc),  //NOI18N 
                            NotifyDescriptor.ERROR_MESSAGE);
                        DialogDisplayer.getDefault().notify(d);
                        return;
                    }

                    String packageName = panel.getPackagePath();
                    if ( !isValidPackagePath(packageName)) {
                        NotifyDescriptor d = new NotifyDescriptor.Message(
                            NbBundle.getMessage(UIStubGenerator.class, "MSG_INVALID_UI_STUB_PACKAGE_NAME", packageName),  //NOI18N
                            NotifyDescriptor.ERROR_MESSAGE);
                        DialogDisplayer.getDefault().notify(d);
                        return;
                    }

                    stubFile = new File(stubLoc);
                    if ( stubFile.isDirectory()) {
                        NotifyDescriptor d = new NotifyDescriptor.Message(
                            NbBundle.getMessage(UIStubGenerator.class, "MSG_UI_STUB_FILENAME_IS_DIR", stubLoc),  //NOI18N
                            NotifyDescriptor.ERROR_MESSAGE);
                        DialogDisplayer.getDefault().notify(d);
                        return;
                    } else {
                        if ( stubFile.exists()) {
                            NotifyDescriptor d = new NotifyDescriptor.Confirmation( 
                                NbBundle.getMessage(UIStubGenerator.class, "MSG_FILE_EXISTS_MSG", stubFile.getAbsoluteFile()),  //NOI18N
                                NbBundle.getMessage(UIStubGenerator.class, "MSG_FILE_EXISTS_TITLE"), //NOI18N 
                                NotifyDescriptor.YES_NO_OPTION,   
                                NotifyDescriptor.WARNING_MESSAGE);
                            if ( DialogDisplayer.getDefault().notify(d) != NotifyDescriptor.YES_OPTION) {
                                return;
                            }
                        }
                    }

                    implGenerate( fo.getNameExt(), stubLoc, packageName);                    
                }
            } catch( Exception e) {
                e.printStackTrace();
                //SceneManager.error("Animation export failed", e);
            }
        } else {
            throw new IllegalArgumentException( );
        }        
    }

    private void implGenerate( final String archiveName, final String stubLocation, final String packagePath) {
        Thread th = new Thread() {
            @Override
            public void run() {
                try {
                    // force the model update if changed lately
                    FXDComposerModel dataModel = m_dObj.getDataModel();
                    FXDFileModel model = dataModel.getFXDContainer().getFileModel( dataModel.getSelectedEntry());
                    model.updateModel();

                    model.visitElements( new FXDFileModel.ElementVisitor() {
                        public boolean visitElement(String elemType, String elemName, AttributeSet attrs) throws AttributeConflictException {
                            String id = (String) attrs.getAttribute("id"); //NOI18N 
                            int    len;
                            if ( id != null && (len=id.length()) > 0) {
                                if ( len > 2 && id.charAt(0) == '"' && id.charAt(len-1) == '"') { // NOI18N
                                    m_attrs.add( new AttributeDescription(elemName, id.substring(1, len-1)));
                                } else {
                                    System.err.println("Invalid node id: " + id);  //NOI18N 
                                }
                            }
                            return true;
                        }
                    });
                    m_attrs.processNames();

                    final File uiStubFile = new File(stubLocation);
                    writeUIStub( archiveName, uiStubFile, packagePath);

                    SwingUtilities.invokeLater( new Runnable() {
                        public void run() {
                            String msgFormat = NbBundle.getMessage(UIStubGenerator.class, "MSG_STUB_CREATED"); //NOI18N
                            
                            NotifyDescriptor d = new NotifyDescriptor.Confirmation( 
                                    String.format(msgFormat, stubLocation),
                                    NbBundle.getMessage(UIStubGenerator.class, "TITLE_STUB_CREATED"),  //NOI18N 
                                    NotifyDescriptor.YES_NO_OPTION,
                                    NotifyDescriptor.INFORMATION_MESSAGE);
                            if ( DialogDisplayer.getDefault().notify(d) == DialogDescriptor.YES_OPTION) {
                                FileObject uiStubFO = FileUtil.toFileObject(uiStubFile);
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
    
    private void writeUIStub( final String archiveName, final File stubFile, String packagePath) throws FileNotFoundException, IOException, AttributeConflictException {
        FXWriter writer = new FXWriter(stubFile);

        String className = PackagerUtils.getFileName(stubFile);

        writer.write( "/*"); //NOI18N 
        writer.write( " * Generated by JavaFX Production Suite NetBeans plugin.");  //NOI18N 
        writer.write( " * " + stubFile.getName());  //NOI18N 
        writer.write( " *");  //NOI18N 
        writer.write( " * Created on " + new Date());  //NOI18N 
        writer.write( " */");  //NOI18N 
        writer.write( "package " + packagePath + ";");  //NOI18N 
        writer.write( "");  //NOI18N 
        writer.write( "import java.lang.Object;");  //NOI18N 
        writer.write( "import java.lang.System;");  //NOI18N 
        writer.write( "import java.lang.RuntimeException;");  //NOI18N 
        writer.write( "import javafx.scene.Node;");  //NOI18N 
        writer.write( "import javafx.fxd.UiStub;");  //NOI18N 
        writer.write( "");  //NOI18N 

        writer.write( "public class " + className + " extends UiStub {");  //NOI18N 

        writer.increaseIndent();
        writer.write("");  //NOI18N 
        writer.write( "override public var url = \"{__DIR__}" + archiveName + "\";");  //NOI18N 
        writer.write("");  //NOI18N 
        
        m_attrs.serializeDeclarations(writer, true);

        writer.write("");  //NOI18N 

        writer.write("override protected function update() {"); //NOI18N 
        writer.increaseIndent();
        writer.write( "lastNodeId = null;");  //NOI18N 
        writer.write(" try {");  //NOI18N 
        writer.increaseIndent();

        m_attrs.serializeUpdates(writer, true);

        writer.decreaseIndent();
        writer.write( "} catch( e:java.lang.Exception) {");  //NOI18N 
        writer.increaseIndent();
        writer.write( "System.err.println(\"Update of the  attribute '{lastNodeId}' failed with: {e}\");");  //NOI18N 
        writer.write( "throw e;");  //NOI18N 
        writer.decreaseIndent();
        writer.write("}");  //NOI18N 

        writer.decreaseIndent();
        writer.write("}");   //NOI18N 
        writer.decreaseIndent();
        writer.write( "}");  //NOI18N 
        writer.write("");  //NOI18N 

        writer.close();
    }
                
    static String getPackagePath(final File file) {
        StringBuilder pName = new StringBuilder();

        File fp=file;
        while ((fp=fp.getParentFile()) != null) {
            String dirName = fp.getName();
            if (dirName.equals("src")) {  //NOI18N
                return pName.toString();
            } else {
                if ( pName.length() > 0) {
                    pName.insert(0, '.');
                }
                pName.insert( 0, dirName);
            }
        }
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
