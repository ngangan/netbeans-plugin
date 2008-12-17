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
                            NbBundle.getMessage( SDKSamplesFileSystem.class, "WARN_Cannot_find_demo_layer" )); // NOI18N
                } catch( MalformedURLException e ) {
                    Logger.getLogger( SDKSamplesFileSystem.class.getName()).warning(
                            NbBundle.getMessage( SDKSamplesFileSystem.class, "WARN_Cannot_find_demo_layer" )); // NOI18N
                } catch( IOException e ) {
                    Logger.getLogger( SDKSamplesFileSystem.class.getName()).warning(
                            NbBundle.getMessage( SDKSamplesFileSystem.class, "WARN_Cannot_find_demo_layer" )); // NOI18N
                }
            }
        });
    }

    private URL createLayer() throws IOException {
        FileObject root = Repository.getDefault().getDefaultFileSystem().getRoot();
        FileObject folder = FileUtil.createFolder(root, "javafx/samples" ); // NOI18N
        FileObject xml = FileUtil.createData( folder, "sdksamples.xml" ); // NOI18N

        // Get all files in samples
        File fxPath = InstalledFileLocator.getDefault().locate( "javafx-sdk1.0/samples", // NOI18N
                "org.netbeans.modules.javafx", false ); // NOI18N
        if( fxPath == null ) return null;
        String descriptionsXml = "", samplesXml = ""; // NOI18N
        FileObject sdkRoot = FileUtil.toFileObject( fxPath );
        if( sdkRoot == null ) return null;
        for( FileObject sample : sdkRoot.getChildren()) {
            // Skip non folder
            if( !sample.isFolder()) continue;
            String sampleName = sample.getName();
            FileObject zip = sample.getFileObject( sampleName + ".zip" ); // NOI18N
            // Skip directories without zip
            if( zip == null ) continue;
            // Descriptions
            FileObject descriptionFO = sample.getFileObject( "description.html" ); // NOI18N

            if( descriptionFO != null ) {
                descriptionsXml += "<folder name=\"" + sampleName + "\">"; // NOI18N
                descriptionsXml += "<file name=\"description.html\" url=\"" + descriptionFO.getURL().toString() + "\"/>"; // NOI18N
                descriptionsXml += "</folder>"; // NOI18N
            }

            // file definition
            samplesXml += "<file name=\"" + spaceName( sampleName ) + "\" url=\"" + zip.getURL().toString() + "\">"; // NOI18N
                samplesXml += "<attr name=\"SystemFileSystem.icon\" urlvalue=\"nbresloc:/org/netbeans/modules/javafx/dataloader/FX-filetype.png\"/>"; // NOI18N
                samplesXml += "<attr name=\"SystemFileSystem.localizingBundle\" stringvalue=\"org.netbeans.modules.javafx.sdksamples.Bundle\"/>"; // NOI18N
                samplesXml += "<attr name=\"instantiatingIterator\" methodvalue=\"org.netbeans.modules.javafx.sdksamples.SDKSamplesWizardIterator.createIterator\"/>"; // NOI18N
                if( descriptionFO != null ) {
                    samplesXml += "<attr name=\"instantiatingWizardURL\" urlvalue=\"nbfs:/SystemFileSystem/SDKSamples/" + sampleName + "/description.html\"/>"; // NOI18N
                }
                samplesXml += "<attr name=\"template\" boolvalue=\"true\"/>"; // NOI18N
            samplesXml += "</file>"; // NOI18N
        }

        FileLock lock = xml.lock();
        PrintStream os = new PrintStream( xml.getOutputStream( lock ));
        os.println( "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" ); // NOI18N
        os.println( "<filesystem>" ); // NOI18N
            os.println( "<folder name=\"SDKSamples\">" ); // NOI18N
                os.println( descriptionsXml );
            os.println( "</folder>" ); // NOI18N
            os.println( "<folder name=\"Templates\">" ); // NOI18N
                os.println( "<folder name=\"Project\">" ); // NOI18N
                    os.println( "<folder name=\"Samples\">" ); // NOI18N
                        os.println( "<folder name=\"JavaFX\">" ); // NOI18N
                            os.println( samplesXml );
                        os.println( "</folder>" ); // NOI18N
                    os.println( "</folder>" ); // NOI18N
                os.println( "</folder>" ); // NOI18N
            os.println( "</folder>" ); // NOI18N
        os.println( "</filesystem>" ); // NOI18N
        os.close();

        return xml.getURL();
    }

    private String spaceName( String name ) {
        String newName = ""; // NOI18N
        for( char c : name.toCharArray()) {
            if( Character.isUpperCase( c )) newName += " "; // NOI18N
            newName += c;
        }
        return newName.trim();
    }
}
