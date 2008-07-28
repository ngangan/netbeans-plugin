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

package qa.javafx.functional.library.project;

import javax.swing.JDialog;
import org.netbeans.jellytools.modules.debugger.actions.FinishDebuggerAction;
import org.netbeans.jemmy.operators.JDialogOperator;
import org.netbeans.junit.ide.ProjectSupport;
import qa.javafx.functional.library.Constant;
import qa.javafx.functional.library.Util;

/**
 *
 * @author andromeda
 */
public class DebuggerOperator {
    
    JavaProject project;
    
    public DebuggerOperator(JavaProject project){
        this.project = project;
    }
    
    public void debug(){
        //new DebugAction().performMenu(project.getProjectNode());
        //ProjectSupport.waitScanFinished();
        //System.out.println("[debugger] Wait scan finished");
        Util.waitScanFinished();

        //System.out.println("[debugger] start");

        Util.sleep(1000);
        project.getProjectNode().performPopupActionNoBlock(Constant.POPUP_MENU_ITEM_DEBUG);
        //ProjectSupport.waitScanFinished();

//        System.out.println("[debugger] looking for the dialog");
//        
//        JDialog dialog = JDialogOperator.waitJDialog(Constant.DIALOG_TITLE_DEBUG_PROJECT, false, true);
//        System.out.println("[debugger] dialog: " + dialog);
//
//        if (dialog != null ) {
//            new JDialogOperator(dialog).waitClosed();
//        }
        

    }

    public void finish(){
        new FinishDebuggerAction().perform();
    }
}
