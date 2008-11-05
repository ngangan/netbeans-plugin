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

package org.netbeans.modules.javafx.sdksamples;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Logger;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.MultiFileSystem;
import org.openide.filesystems.Repository;
import org.openide.filesystems.XMLFileSystem;
import org.openide.modules.InstalledFileLocator;
import org.openide.util.NbBundle;
import org.openide.windows.WindowManager;
import org.xml.sax.SAXException;

/**
 *
 * @author Michal Skvor
 */
public class SDKSamplesFileSystem extends MultiFileSystem {

    public SDKSamplesFileSystem() {
        WindowManager.getDefault().invokeWhenUIReady( new Runnable() {
            public void run() {
                try {
                    URL layerURL = createLayer();
                    if( layerURL != null ) {
                        setDelegates( new XMLFileSystem( layerURL ));
                    }
                } catch( SAXException e ) {
                    Logger.getLogger( SDKSamplesFileSystem.class.getName()).warning(
                            NbBundle.getMessage( SDKSamplesFileSystem.class, "WARN_Cannot_find_demo_layer" ));
                } catch( MalformedURLException e ) {
                    Logger.getLogger( SDKSamplesFileSystem.class.getName()).warning(
                            NbBundle.getMessage( SDKSamplesFileSystem.class, "WARN_Cannot_find_demo_layer" ));
                } catch( IOException e ) {
                    Logger.getLogger( SDKSamplesFileSystem.class.getName()).warning(
                            NbBundle.getMessage( SDKSamplesFileSystem.class, "WARN_Cannot_find_demo_layer" ));
                }
            }
        });
    }

    private URL createLayer() throws IOException {
        FileObject root = Repository.getDefault().getDefaultFileSystem().getRoot();
        FileObject folder = FileUtil.createFolder(root, "javafx/samples" );
        FileObject xml = FileUtil.createData( folder, "sdksamples.xml" );

        // Get all files in samples
        File fxPath = InstalledFileLocator.getDefault().locate( "javafx-sdk1.0/samples",
                "org.netbeans.modules.javafx", false );
        if( fxPath == null ) return null;
        String descriptionsXml = "", samplesXml = "";
        FileObject sdkRoot = FileUtil.toFileObject( fxPath );
        if( sdkRoot == null ) return null;
        for( FileObject sample : sdkRoot.getChildren()) {
            // Skip non folder
            if( !sample.isFolder()) continue;
            String sampleName = sample.getName();
            FileObject zip = sample.getFileObject( sampleName + ".zip" );
            // Skip directories without zip
            if( zip == null ) continue;
            // Descriptions
            FileObject descriptionFO = sample.getFileObject( "description.html" );

            if( descriptionFO != null ) {
                descriptionsXml += "<folder name=\"" + sampleName + "\">";
                descriptionsXml += "<file name=\"description.html\" url=\"" + descriptionFO.getURL().toString() + "\"/>";
                descriptionsXml += "</folder>";
            }

            // file definition
            samplesXml += "<file name=\"" + spaceName( sampleName ) + "\" url=\"" + zip.getURL().toString() + "\">";
                samplesXml += "<attr name=\"SystemFileSystem.icon\" urlvalue=\"nbresloc:/org/netbeans/modules/javafx/dataloader/FX-filetype.png\"/>";
                samplesXml += "<attr name=\"SystemFileSystem.localizingBundle\" stringvalue=\"org.netbeans.modules.javafx.sdksamples.Bundle\"/>";
                samplesXml += "<attr name=\"instantiatingIterator\" methodvalue=\"org.netbeans.modules.javafx.sdksamples.SDKSamplesWizardIterator.createIterator\"/>";
                if( descriptionFO != null ) {
                    samplesXml += "<attr name=\"instantiatingWizardURL\" urlvalue=\"nbfs:/SystemFileSystem/SDKSamples/" + sampleName + "/description.html\"/>";
                }
                samplesXml += "<attr name=\"template\" boolvalue=\"true\"/>";
            samplesXml += "</file>";
        }

        FileLock lock = xml.lock();
        PrintStream os = new PrintStream( xml.getOutputStream( lock ));
        os.println( "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" );
        os.println( "<filesystem>" );
            os.println( "<folder name=\"SDKSamples\">" );
                os.println( descriptionsXml );
            os.println( "</folder>" );
            os.println( "<folder name=\"Templates\">" );
                os.println( "<folder name=\"Project\">" );
                    os.println( "<folder name=\"Samples\">" );
                        os.println( "<folder name=\"JavaFX\">" );
                            os.println( samplesXml );
                        os.println( "</folder>" );
                    os.println( "</folder>" );
                os.println( "</folder>" );
            os.println( "</folder>" );
        os.println( "</filesystem>" );
        os.close();

        return xml.getURL();
    }

    private String spaceName( String name ) {
        String newName = "";
        for( char c : name.toCharArray()) {
            if( Character.isUpperCase( c )) newName += " ";
            newName += c;
        }
        return newName.trim();
    }
}
