/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2009 Sun Microsystems, Inc. All rights reserved.
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
 * Portions Copyrighted 2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.sdksamples;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Set;
import javax.annotation.processing.Processor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedSourceVersion;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.tools.StandardLocation;
import org.netbeans.modules.javafx.sdksamples.api.JavaFXSamples;
import org.openide.filesystems.annotations.LayerBuilder;
import org.openide.filesystems.annotations.LayerGeneratingProcessor;
import org.openide.filesystems.annotations.LayerGenerationException;
import org.openide.util.lookup.ServiceProvider;

/**
 * A processor that gathers all the samples folder annotations, scans the
 * folders and generates layer file containing all the samples found.
 */
@ServiceProvider(service=Processor.class)
@SupportedAnnotationTypes("org.netbeans.modules.javafx.sdksamples.api.JavaFXSamples")
@SupportedSourceVersion(SourceVersion.RELEASE_5)
public class SDKSamplesProcessor extends LayerGeneratingProcessor {

    @Override
    protected boolean handleProcess(Set<? extends TypeElement> set, RoundEnvironment re) throws LayerGenerationException {
        if (re.processingOver()) return false;

        for (Element e : re.getElementsAnnotatedWith(JavaFXSamples.class)) {
            JavaFXSamples samples = e.getAnnotation(JavaFXSamples.class);

            LayerBuilder.File wizzard = layer(e).folder("Templates/Project/Samples/JavaFX");
            File sdkRoot = null;
            try {
                // the annotation is attached to a package
                String pkg = e.asType().toString();

                // path to the annotated source file
                URI uri = processingEnv.getFiler().getResource(StandardLocation.SOURCE_PATH,
                    pkg, "package-info.java").toUri(); // NOI18N

                // use relative path to find the samples folder
                URI sdk = uri.resolve(samples.pathToSamples());
                sdkRoot = new File(sdk.toString());
            } catch (IOException ioe) {
                LayerGenerationException lge = new LayerGenerationException(ioe.getMessage());
                lge.initCause(ioe);
                throw lge;
            }
            if (!sdkRoot.isDirectory()) continue;

            for( File sample : sdkRoot.listFiles()) {
                // Skip non folder
                if( !sample.isDirectory()) continue;
                String sampleName = sample.getName();
                File zip = new File(sample, sampleName + ".zip" ); // NOI18N
                // Skip directories without zip
                if( !zip.isFile()) continue;

                // Descriptions
                File descriptionFile = new File(sample, "comment.html" ); // NOI18N

                if( descriptionFile.canRead()) {
                    LayerBuilder.File desc = layer(e).file("SDKSamples/" + sampleName + "/description.html"); // NOI18N
                    desc.url("nbinst://javafx2/javafx-sdk/samples/" + sampleName + "/comment.html");
                    desc.write();
                }

                LayerBuilder.File l = layer(e).file("Templates/Project/Samples/JavaFX/" + spaceName(sampleName));
                l.url("nbinst://javafx2/javafx-sdk/samples/" + sampleName + "/" + zip.getName());
                l.urlvalue("SystemFileSystem.icon", "nbresloc:/org/netbeans/modules/javafx/dataloader/FX-filetype.png"); // NOI18N
                l.stringvalue("SystemFileSystem.localizingBundle", "org.netbeans.modules.javafx.sdksamples.Bundle"); // NOI18N
                l.methodvalue("instantiatingIterator", "org.netbeans.modules.javafx.sdksamples.SDKSamplesWizardIterator", "createIterator"); // NOI18N
                if (descriptionFile.isFile()) {
                    l.urlvalue("instantiatingWizardURL", "nbfs:/SystemFileSystem/SDKSamples/" + sampleName + "/description.html"); // NOI18N
                }
                l.boolvalue("template", true); // NOI18N
                l.write();
            }



            wizzard.write();
        }
        return true;
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
