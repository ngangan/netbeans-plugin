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
package org.netbeans.modules.visage.navigation.actions;

import javax.swing.*;
import java.awt.Toolkit;
import java.awt.event.*;
import javax.lang.model.element.Element;
import org.netbeans.api.visage.editor.ElementOpen;
import org.netbeans.api.visage.source.CompilationInfo;
import org.netbeans.api.visage.source.ElementHandle;
import org.netbeans.modules.visage.navigation.SpaceMagicUtils;
import org.openide.awt.StatusDisplayer;
import org.openide.filesystems.FileObject;
import org.openide.util.*;

/**
 * An action that opens editor and jumps to the element given in constructor.
 * Similar to editor's go to declaration action.
 *
 * @author tim, Dafe Simonek
 * @author Anton Chechel - visage modifications
 */
public final class OpenAction extends AbstractAction {

    private CompilationInfo compilationInfo;
    private ElementHandle<? extends Element> elementHandle;
    private FileObject fileObject;
    private String displayName;
    private long offset;

//    public OpenAction(Element element, FileObject fileObject) {
//        this(element, fileObject, null, -1);
//    }

    public OpenAction(CompilationInfo compilationInfo, ElementHandle<? extends Element> elementHandle, FileObject fileObject, String displayName, long offset) {
        this.compilationInfo = compilationInfo;
        this.elementHandle = elementHandle;
        this.fileObject = fileObject;
        this.displayName = displayName;
        this.offset = offset;
        putValue(Action.NAME, NbBundle.getMessage(OpenAction.class, "LBL_Goto")); //NOI18N
    }

    public void actionPerformed(ActionEvent ev) {
        if (null == fileObject) {
            Toolkit.getDefaultToolkit().beep();
            if (null != displayName) {
                StatusDisplayer.getDefault().setStatusText(NbBundle.getMessage(OpenAction.class, "MSG_NoSource", displayName));  //NOI18N
            }
        } else {
            try {
                if (SpaceMagicUtils.hasSpiritualInvocation(elementHandle, compilationInfo)) {
                    // space magic here, can't be opened via element
                    ElementOpen.open(fileObject, (int) offset);
                } else {
                    ElementOpen.open(fileObject, elementHandle);
                }
            } catch (Exception ex) {
                Exceptions.printStackTrace(ex);
            }
        }
    }

    @Override
    public boolean isEnabled() {
        return true;
    }
    
}
