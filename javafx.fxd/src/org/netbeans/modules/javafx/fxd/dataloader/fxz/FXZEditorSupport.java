/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.dataloader.fxz;

import java.io.IOException;
import java.io.InputStream;
import javax.swing.text.BadLocationException;
import javax.swing.text.StyledDocument;
import org.netbeans.core.spi.multiview.MultiViewDescription;
import org.netbeans.core.spi.multiview.MultiViewFactory;
import org.netbeans.modules.javafx.fxd.composer.archive.ArchiveViewDescription;
import org.netbeans.modules.javafx.fxd.composer.preview.PreviewViewDescription;
import org.netbeans.modules.javafx.fxd.composer.source.SourceViewDescription;
import org.openide.cookies.EditCookie;
import org.openide.cookies.EditorCookie;
import org.openide.cookies.OpenCookie;
import org.openide.cookies.SaveCookie;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.text.CloneableEditorSupport;
import org.openide.text.DataEditorSupport;
import org.openide.windows.CloneableTopComponent;
import com.sun.javafx.tools.fxd.container.FXDContainer;
import java.io.OutputStream;
import java.io.Serializable;
import javax.swing.SwingUtilities;
import javax.swing.text.EditorKit;
import org.netbeans.core.spi.multiview.CloseOperationHandler;
import org.netbeans.core.spi.multiview.CloseOperationState;
import org.netbeans.editor.BaseDocument;
import org.openide.util.Task;
import org.openide.windows.TopComponent;

/**
 *
 * @author Pavel Benes
 */
public final class FXZEditorSupport extends DataEditorSupport implements Serializable, OpenCookie, EditorCookie, EditorCookie.Observable, EditCookie {
    private static final long serialVersionUID = 1L;    
        
    //TODO Hack - I should not be holding this reference
    protected CloneableTopComponent m_mvtc  = null;
    
    public FXZEditorSupport( FXZDataObject dObj) {
        super(dObj, new FXDEnv(dObj));
    }
            
    public void updateDisplayName() {
        final TopComponent tc = m_mvtc;
        if (tc == null) {
            return;
        }
        
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                //ProjectTypeInfo projectTypeInfo = ProjectTypeInfo.getProjectTypeInfoFor (IOSupport.getDataObjectContext (dataObject).getProjectType ());
                //tc.setIcon (projectTypeInfo != null ? ImageUtilities.loadImage (projectTypeInfo.getIconResource ()) : null);

                String displayName = messageName();
                if (! displayName.equals(tc.getDisplayName()))
                    tc.setDisplayName(displayName);
                tc.setToolTipText(getDataObject().getPrimaryFile().getPath());
            }
        });
    }

    @Override
    protected Task reloadDocument() {
        final Task reloadTask = super.reloadDocument();
        Thread th = new Thread() {
            @Override
            public void run() {
                reloadTask.waitFinished();
                SwingUtilities.invokeLater( new Runnable() {
                    public void run() {
                        FXZDataObject dObj = (FXZDataObject) getDataObject();
                        dObj.getDataModel().getFXDContainer().incrementChangeTicker(false);
                        dObj.getController().reload();
                    }                    
                });
            }
        };
        th.setName("ReloadDocument-Thread"); //NOI18N
        th.setPriority( Thread.MIN_PRIORITY);
        th.start();
        return reloadTask;
    }
    
    @Override
    protected boolean notifyModified() {
        if ( super.notifyModified()) {
            addSaveCookie();
            updateDisplayName();
            return true;
        } else {
            return false; //still unmodified
        }
    }
    
    private void addSaveCookie() {
        FXZDataObject dObj = (FXZDataObject) getDataObject();
        dObj.addSaveCookie(new SaveCookie() {
            public void save() throws IOException {
                saveDocument();
                ((FXZDataObject) getDataObject()).getDataModel().getFXDContainer().setIsSaved();
            }
        });
    }

    private void removeSaveCookie() {
        FXZDataObject dObj = (FXZDataObject) getDataObject();
        dObj.removeSaveCookie();
    }
    
    @Override
    protected void notifyUnmodified() {
        super.notifyUnmodified();
        removeSaveCookie();

        //disabled since the hack seems to cause some problems with state un/modified handling
//        FXZDataObject dObj = (FXZDataObject) getDataObject();
//        if(dObj.getDataModel().getFXDContainer().areEntriesChanged()) {
//            SwingUtilities.invokeLater(new Runnable() {
//                public void run() {
//                    notifyModified();
//                }
//            });
//        }
        updateDisplayName();
    }
        
    @Override
    protected CloneableEditorSupport.Pane createPane() {
        MultiViewDescription [] views   = getViewDescriptions();
        int                     defView = ((FXZDataObject) getDataObject()).getDefaultView();
        m_mvtc = MultiViewFactory.createCloneableMultiView(views, views[ defView],
                new CloseHandler(this));
        return (CloneableEditorSupport.Pane)m_mvtc;
    }
    
    CloneableTopComponent getMVTC() {
        return m_mvtc;
    }
    
    void resetMVTC() {
        m_mvtc = null;
    }
        
    @Override
    public String messageHtmlName() {
        return super.messageHtmlName();
    }

    @Override
    public String messageName() {
        return super.messageName();        
    }
    
    protected MultiViewDescription [] getViewDescriptions() {
        return new MultiViewDescription[] {
            new PreviewViewDescription(this),        
            new SourceViewDescription(this),
            new ArchiveViewDescription(this)
        };
    }
        
    @Override
    protected void saveFromKitToStream(StyledDocument doc, EditorKit kit, OutputStream stream)
        throws IOException, BadLocationException {
        ((FXZDataObject)getDataObject()).getDataModel().getFXDContainer().save( (BaseDocument) doc, kit, stream);
    }
    
    static final class FXDEnv extends DataEditorSupport.Env {
        private static final long  serialVersionUID = 1L;
        
        public FXDEnv( FXZDataObject obj) {
            super(obj);
        }
        
        @Override
        public boolean isModified() {
            return super.isModified();
        }
        
        @Override
        public InputStream inputStream() throws IOException {
            FXDContainer container = ((FXZDataObject)getDataObject()).getDataModel().getFXDContainer();
            return container.open();
        }        
                
        @Override
        protected FileObject getFile() {
            return getDataObject().getPrimaryFile();
        }

        @Override
        protected FileLock takeLock() throws IOException {
            return ((FXZDataObject)getDataObject()).getPrimaryEntry().takeLock();
        }
    }
    
    private static class CloseHandler implements CloseOperationHandler, Serializable {
        private static final long serialVersionUID =1L;

        private FXZEditorSupport sup;

        private CloseHandler() {
        }

        public CloseHandler(FXZEditorSupport formDO) {
            sup = formDO;
        }

        public boolean resolveCloseOperation(CloseOperationState[] elements) {
            boolean status = sup.canClose();
            //hack or the only possible fix?
            //we need to call support.canClose() here to get the status of the
            //fxd editor. Once user chooses discard option the editor is not saved,
            //and the whole multiview gets closed. During that FXDSourceEditor.componentClosed()
            //is called. Then it runs to CloneableEditor.closeLast() which triggers
            //support.canClose() again and the dialog is here once more.
            //There doesn't seem to be a clean fix for that so setting the editor
            //state to unmodified if the canClose() returns true (save or discard)
            //so the next call to canClose() doesn't do anything (it tests the
            //editor for modified state first).
            if(status) {
                sup.notifyUnmodified();
            }   
            return status;        
        }
    }   
}
