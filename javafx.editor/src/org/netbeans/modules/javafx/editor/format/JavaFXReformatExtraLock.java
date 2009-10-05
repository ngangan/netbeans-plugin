/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2008 Sun
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
package org.netbeans.modules.javafx.editor.format;

import java.lang.reflect.Method;
import java.util.logging.Logger;
import org.netbeans.modules.editor.indent.spi.ExtraLock;

/**
 * Extra lock for formatting task.
 *
 * @author Anton Chechel
 */
public final class JavaFXReformatExtraLock implements ExtraLock {

    private static final Logger LOGGER = Logger.getLogger(JavaFXReformatExtraLock.class.getName());
    private static JavaFXReformatExtraLock instance;

    private Method aplMethod;
    private Method rplMethod;

    // Using reflecion is temporarily solution while waiting friendly module to
    // provide Utilities functionality from Parsing API.
    private JavaFXReformatExtraLock() {
        try {
            final ClassLoader cl = Thread.currentThread().getContextClassLoader();
            final Class<?> clazz = Class.forName("org.netbeans.modules.parsing.impl.Utilities", true, cl); // NOI18N
            aplMethod = clazz.getDeclaredMethod("acquireParserLock"); // NOI18N
            rplMethod = clazz.getDeclaredMethod("releaseParserLock"); // NOI18N
            aplMethod.setAccessible(true);
            rplMethod.setAccessible(true);
        } catch (Exception ex) {
            LOGGER.severe(ex.getMessage());
        }
    }

    public static synchronized JavaFXReformatExtraLock getInstance() {
        if (instance == null) {
            instance = new JavaFXReformatExtraLock();
        }
        return instance;
    }

    public void lock() {
        if (aplMethod != null) {
            try {
                aplMethod.invoke(this, new Object[]{});
            } catch (Exception ex) {
                LOGGER.severe(ex.getMessage());
            }
        }
    }

    public void unlock() {
        if (rplMethod != null) {
            try {
                rplMethod.invoke(this, new Object[]{});
            } catch (Exception ex) {
                LOGGER.severe(ex.getMessage());
            }
        }
    }
}
