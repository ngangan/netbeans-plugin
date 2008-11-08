/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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

package org.netbeans.modules.javafx.project.queries;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.nio.charset.UnsupportedCharsetException;
import org.netbeans.api.queries.FileEncodingQuery;
import org.netbeans.modules.javafx.project.ui.customizer.JavaFXProjectProperties;
import org.netbeans.spi.queries.FileEncodingQueryImplementation;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.openide.filesystems.FileObject;

/**
 *
 * @author Tomas Zezula
 */
public class JavaFXProjectEncodingQueryImpl extends FileEncodingQueryImplementation implements PropertyChangeListener {
    
    
    private final PropertyEvaluator eval;
    private Charset cache;
    
    /** Creates a new instance of JavaFXProjectEncodingQueryImpl */
    public JavaFXProjectEncodingQueryImpl(final PropertyEvaluator eval) {
        assert eval != null;
        this.eval = eval;
        this.eval.addPropertyChangeListener(this);
    }
    
    public Charset getEncoding(FileObject file) {
        assert file != null;
        synchronized (this) {
            if (cache != null) {
                return cache;
            }
        }
        String enc = eval.getProperty(JavaFXProjectProperties.SOURCE_ENCODING);
        synchronized (this) {
            if (cache == null) {
                try {
                    cache = enc == null ? FileEncodingQuery.getDefaultEncoding() : Charset.forName(enc);
                } catch (IllegalCharsetNameException exception) {
                    return null;
                } catch (UnsupportedCharsetException exception) {
                    return null;
                }
            }
            return cache;
        }
    }
   
    public void propertyChange(PropertyChangeEvent event) {        
        String propName = event.getPropertyName();
        if (propName == null || propName.equals(JavaFXProjectProperties.SOURCE_ENCODING)) {
            synchronized (this) {
                cache = null;
            }
        }
    }  
}
