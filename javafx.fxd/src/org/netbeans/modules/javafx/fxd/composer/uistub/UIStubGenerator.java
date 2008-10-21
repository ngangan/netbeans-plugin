/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.uistub;

import com.sun.javafx.tools.fxd.ai.AttributeConflictException;
import com.sun.javafx.tools.fxd.ai.AttributeHolder;
import com.sun.javafx.tools.fxd.ai.FXWriter;
import com.sun.javafx.tools.fxd.ai.Utils;
import com.sun.javafx.tools.fxd.ai.shape.NamedElement;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Date;
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

/**
 *
 * @author Pavel Benes
 */
public final class UIStubGenerator {
    private static final String UI_STUB_EXTENSION = "UI.fx";  //NOI18N

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
            return String.format("AttributeDescription( type='%s', name='%s')", m_type, m_name);
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
                    stubFile = new File( panel.getStubLocation());
                    if ( stubFile.isDirectory()) {
                        assert false;
                    } else {
                        if ( stubFile.exists()) {
                            String msgFormat = NbBundle.getMessage(UIStubGenerator.class, "MSG_FILE_EXISTS_MSG");   //NOI18N 
                            NotifyDescriptor d = new NotifyDescriptor.Confirmation( String.format(msgFormat, stubFile.getAbsoluteFile()),
                                NbBundle.getMessage(UIStubGenerator.class, "MSG_FILE_EXISTS_TITLE"), //NOI18N 
                                NotifyDescriptor.YES_NO_OPTION,   
                                NotifyDescriptor.WARNING_MESSAGE);
                            if ( DialogDisplayer.getDefault().notify(d) != NotifyDescriptor.YES_OPTION) {
                                return;
                            }
                        }
                    }
                    implGenerate( panel.getStubLocation(), panel.getPackagePath());                    
                }
            } catch( Exception e) {
                //SceneManager.error("Animation export failed", e);
            }
        } else {
            throw new IllegalArgumentException( );
        }        
    }
     
    
    private void implGenerate( final String stubLocation, final String packagePath) {
        Thread th = new Thread() {
            @Override
            public void run() {
                try {
                    // force the model update if changed lately
                    FXDFileModel model = m_dObj.getDataModel().getFXDContainer().getFileModel(true);
                    model.updateModel();

                    model.visitElements( new FXDFileModel.ElementVisitor() {
                        public boolean visitElement(String elemType, String elemName, AttributeSet attrs) throws AttributeConflictException {
                            String id = (String) attrs.getAttribute("id"); //NOI18N 
                            int    len;
                            if ( id != null && (len=id.length()) > 0) {
                                if ( len > 2 && id.charAt(0) == '"' && id.charAt(len-1) == '"') {
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
                    writeUIStub( uiStubFile, packagePath);

                    SwingUtilities.invokeLater( new Runnable() {
                        public void run() {
                            String msgFormat = NbBundle.getMessage(UIStubGenerator.class, "MSG_STUB_CREATED");   //NOI18N 
                            
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
                                    OpenCookie cookie = (OpenCookie) uiStubDO.getCookie( OpenCookie.class);
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
    
    private void writeUIStub( final File stubFile, String packagePath) throws FileNotFoundException, IOException, AttributeConflictException {
        FXWriter writer = new FXWriter(stubFile);

        String className = Utils.getFileName(stubFile);

        writer.write( "/*"); //NOI18N 
        writer.write( " * Generated by JavaFX Production Suide NetBeans plugin.");  //NOI18N 
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

        m_attrs.serializeDeclarations(writer, true);

        writer.write("");  //NOI18N 

        writer.write("init {");  //NOI18N 
        writer.increaseIndent();
        writer.write("if (url == null) {");  //NOI18N 
        writer.increaseIndent(); 
        writer.write("url = getURL();");  //NOI18N 
        writer.decreaseIndent();
        writer.write("}");  //NOI18N 
        writer.decreaseIndent();
        writer.write("}");   //NOI18N 
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

        writer.write("public function getURL() : String {");  //NOI18N 
        writer.increaseIndent();
        writer.write("return \"{__DIR__}" + stubFile.getParentFile().getName() + "\";");  //NOI18N 
        writer.decreaseIndent();
        writer.write("}");  //NOI18N 

        writer.close();
    }
            
    static final String PACKAGE_NOT_SET = "<enter-your-package-here>";   //NOI18N
    
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
}
