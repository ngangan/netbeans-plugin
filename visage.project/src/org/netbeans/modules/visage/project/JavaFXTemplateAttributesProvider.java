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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
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

package org.netbeans.modules.visage.project;

import java.util.Collections;
import java.util.Map;
import java.util.logging.Logger;
import org.netbeans.api.java.classpath.ClassPath;
import org.openide.filesystems.FileObject;
import org.openide.loaders.CreateFromTemplateAttributesProvider;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;

/**
 * Provides attributes that can be used inside scripting templates.
 * <dl><dt><code>project.license</code></dt>
 * <dd>attribute containing license name.
 * The provider reads <code>project.license</code> property from build.properties
 * and returns it as the template attribute. In case the property is not available
 * the attribute is filled with <code>"default"</code> value.</dd>
 * </dl>
 *
 * @author Jan Pokorsky
 */
public final class VisageTemplateAttributesProvider implements CreateFromTemplateAttributesProvider {
    
    public Map<String,? extends Object> attributesFor(DataObject template, DataFolder target, String name) {
        FileObject templateFO = template.getPrimaryFile();
        if (!"fx".equals(templateFO.getExt()) || templateFO.isFolder()) { // NOI18N
            return null;
        }
        
        FileObject targetFO = target.getPrimaryFile();
        ClassPath cp = ClassPath.getClassPath(targetFO, ClassPath.SOURCE);
        if (cp == null) {
            Logger.getLogger(VisageTemplateAttributesProvider.class.getName()).warning("No classpath was found for folder: " + target.getPrimaryFile()); // NOI18N
            return Collections.emptyMap();
        }
        return Collections.<String, Object>singletonMap("package", cp.getResourceName(targetFO, '.', false)); // NOI18N
        
/*        
        
        EditableProperties props = helper.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
        String license = props.getProperty("project.license"); // NOI18N
        if (license == null) {
            return null;
        } else {
            return Collections.singletonMap("project", Collections.singletonMap("license", license)); // NOI18N
        }
 */ 
    }

}
