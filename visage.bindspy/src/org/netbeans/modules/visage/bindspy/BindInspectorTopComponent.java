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


package org.netbeans.modules.visage.bindspy;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.Serializable;
import java.util.logging.Logger;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.text.StyledDocument;
import org.netbeans.api.visage.source.ClassIndex;
import org.netbeans.api.visage.source.CompilationController;
import org.netbeans.api.visage.source.VisageSource;
import org.netbeans.api.visage.source.Task;
import org.netbeans.api.progress.ProgressHandle;
import org.netbeans.api.progress.ProgressHandleFactory;
import org.netbeans.api.visual.graph.layout.GridGraphLayout;
import org.netbeans.modules.visage.bindspy.BindsModel.BindConnection;
import org.netbeans.modules.visage.bindspy.BindsModel.BindVariable;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.nodes.Node;
import org.openide.util.ImageUtilities;
import org.openide.util.NbBundle;
import org.openide.util.RequestProcessor;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;

/**
 *
 * @author Michal Skvor <michal.skvor at sun.com>
 */
public class BindInspectorTopComponent extends TopComponent implements PropertyChangeListener {

    private static BindInspectorTopComponent instance;
    private static final String PREFERRED_ID = "BindInspectorTopComponent"; //NOI18N
    private static final Logger log = Logger.getLogger( "org.netbeans.visage.bindspy" ); //NOI18N

    private DataObject oldDataObject;
    private Process process;
    private int timer;

    private JToolBar toolBar;
    private JScrollPane scrollPane;

    private BindGraphScene bindGraphScene;
    private double zoomFactor = 1.0;

    public BindInspectorTopComponent() {
        setLayout( new BorderLayout());
        setBackground( Color.WHITE );
        setDisplayName( NbBundle.getMessage( BindInspectorTopComponent.class, "CTL_Inspector_name"));

        toolBar = new JToolBar();
        toolBar.setLayout( new BoxLayout( toolBar, BoxLayout.X_AXIS ));

        JButton buttonZoomIn = new JButton( new ImageIcon(
                ImageUtilities.loadImage( "org/netbeans/modules/visage/bindspy/resources/zoom_in.png" )));
        buttonZoomIn.addActionListener( new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if( bindGraphScene != null ) {
                    zoomFactor *= 1.1;
                    bindGraphScene.setZoomFactor( zoomFactor );
                    repaint();
                }
            }
        });
        toolBar.add( buttonZoomIn );

        JButton buttonZoomOut = new JButton( new ImageIcon(
                ImageUtilities.loadImage( "org/netbeans/modules/visage/bindspy/resources/zoom_out.png" )));
        buttonZoomOut.addActionListener( new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if( bindGraphScene != null ) {
                    zoomFactor *= 0.9;
                    bindGraphScene.setZoomFactor( zoomFactor );
                    repaint();
                }
            }
        });
        toolBar.add( buttonZoomOut );
        add( toolBar, BorderLayout.NORTH );

        scrollPane = new JScrollPane();
        scrollPane.setHorizontalScrollBarPolicy( JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED );
        scrollPane.setVerticalScrollBarPolicy( JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED );
        add( scrollPane, BorderLayout.CENTER );
    }

    private final RequestProcessor.Task task = RequestProcessor.getDefault().create( new Runnable() {

        private ProgressHandle wHandle = ProgressHandleFactory.createHandle( NbBundle.getMessage( BindInspectorTopComponent.class, "MSG_Wait" )); // NOI18N
        private JComponent graphComponent = null;

        public void run() {
            synchronized( BindInspectorTopComponent.this ) {
                if( process != null ) {
                    process.destroy();
                    timer = 0;
                    task.schedule( 150 );
                    return;
                }
            }
            if( oldDataObject != null ) {
                oldDataObject.removePropertyChangeListener( BindInspectorTopComponent.this );
                oldDataObject = null;
            }
            
            Node[] sel = TopComponent.getRegistry().getActivatedNodes();
            if( sel.length == 1 ) {
                final DataObject d = sel[0].getLookup().lookup( DataObject.class );
                if( d != null ) {
                    final FileObject f = d.getPrimaryFile();
//                    if( f.isData()) bi = null;
                    if( "fx".equals( f.getExt())) { //NOI18N
                        EditorCookie ec = sel[0].getLookup().lookup( EditorCookie.class );

                        StyledDocument document = ec.getDocument();
                        if( document == null ) { 
                            task.schedule( 500 );
                            return;
                        }
//                        wHandle.start();
                        final VisageSource s = VisageSource.forDocument( ec.getDocument());
                        try {
                            s.runWhenScanFinished( new Task<CompilationController>() {
                                public void run( CompilationController cc ) throws Exception {
                                    final FileObject source = f;

                                    ClassIndex index = cc.getClasspathInfo().getClassIndex();
                                    BindsWalker iw = new BindsWalker( cc, index, d );
                                    final BindsModel model = new BindsModel();
                                    iw.scan( cc.getCompilationUnit(), model );

                                    final BindGraphScene bgs = new BindGraphScene();
                                    SwingUtilities.invokeLater( new Runnable() {
                                        public void run() {
//                                            System.out.println(" - - - - Nodes - - - - ");
                                            for( BindVariable node : model.getNodes()) {
//                                                System.out.println( "n:" + node.getVariableName());
                                                if( model.hasReference( node ))
                                                    bgs.addNode( node );
                                            }

//                                            System.out.println(" - - - - Edges - - - - ");
                                            for( BindVariable node : model.getNodes()) {
                                                if( !model.hasReference( node )) continue;
                                                for( BindConnection connection : node.getConnections()) {
                                                    bgs.addEdge( connection );
                                                    bgs.setEdgeSource( connection, model.getVariable( node.getVariableName()));
                                                    bgs.setEdgeTarget( connection, model.getVariable( connection.getVariableName()));
                                                }
                                            }

                                            graphComponent = bgs.createView();
                                            if( graphComponent != null ) {
                                                remove( graphComponent );
                                            }
                                            scrollPane.setViewportView( graphComponent );
                                            
                                            bgs.setZoomFactor( zoomFactor );
                                            bgs.validate();
                                            final GridGraphLayout gridGraphLayout = new GridGraphLayout();
                                            gridGraphLayout.setAnimated( false );
                                            gridGraphLayout.layoutGraph( bgs );
                                            bgs.validate();

                                            bindGraphScene = bgs;
                                            
                                        }
                                    });
                                }
                            }, true );
                        } catch( IOException e ) {

                        } finally {
                            if( wHandle != null ) {
//                                wHandle.finish();
                            }
                        }

                    }
                }
            }
            repaint();
        }
    });

    public static synchronized BindInspectorTopComponent getDefault() {
        if (instance == null) {
            instance = new BindInspectorTopComponent();
        }
        return instance;
    }

//    @Override
//    public void paintComponent( Graphics g ) {
//        g.clearRect( 0, 0, getWidth(), getHeight());
//        String noPreview = NbBundle.getMessage( BindInspectorTopComponent.class, "MSG_NoInspection" ); //NOI18N
//        Rectangle2D r = g.getFontMetrics().getStringBounds( noPreview, g );
//        g.drawString( noPreview, (getWidth()-(int)r.getWidth())/2, (getHeight()-(int)r.getHeight())/2 );
//    }

    /**
     * Obtain the VisagePreviewTopComponent instance. Never call {@link #getDefault} directly!
     */
    public static synchronized BindInspectorTopComponent findInstance() {
        TopComponent win = WindowManager.getDefault().findTopComponent( PREFERRED_ID );
        if (win == null) {
            Logger.getLogger( BindInspectorTopComponent.class.getName()).
                    warning( "Cannot find " + PREFERRED_ID + " component. It will not be located properly in the window system." ); //NOI18N
            return getDefault();
        }
        if( win instanceof BindInspectorTopComponent ) {
            return (BindInspectorTopComponent)win;
        }
        Logger.getLogger( BindInspectorTopComponent.class.getName()).
                warning("There seem to be multiple components with the '" + PREFERRED_ID + "' ID. That is a potential source of errors and unexpected behavior."); //NOI18N
        return getDefault();
    }

    @Override
    public int getPersistenceType() {
        return TopComponent.PERSISTENCE_ALWAYS;
    }

    @Override
    public void componentOpened() {
        TopComponent.getRegistry().addPropertyChangeListener( this );
        task.schedule( 150 );
    }

    @Override
    public void componentClosed() {
        TopComponent.getRegistry().removePropertyChangeListener( this );
    }

    public void propertyChange( PropertyChangeEvent ev ) {
        if( TopComponent.Registry.PROP_ACTIVATED_NODES.equals( ev.getPropertyName()) || DataObject.PROP_MODIFIED.equals( ev.getPropertyName())) {
            task.schedule( 150 );
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
            return BindInspectorTopComponent.getDefault();
        }
    }
}
