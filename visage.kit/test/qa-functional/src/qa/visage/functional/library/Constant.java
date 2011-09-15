/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Oracle and/or its affiliates. All rights reserved.
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

package qa.visage.functional.library;

/**
 *
 * @author andromeda
 */
public interface Constant {

    
    // ================   SDK Version  ========================
    
    String JAVAFX_SDK_COMPILER = "compiler";
    String JAVAFX_SDK_REPRISE = "reprise";
    
    String JAVAFX_SDK_VERSION_PROPERTY = "visage.sdk.version";
    
    
    // ================   User Data  ========================
    
    String USER_DATA_REPRISE = "gui";
    String USER_DATA_COMPILER = "ui";
    
    String USER_DATA_PATH_DEFAULT = USER_DATA_REPRISE;
    

    // ================   Project  ========================
    
    String PROJECT_JAVA_APPLICATION = "Java Application";
    String PROJECT_JAVAFX_APPLICATION = "Visage Script Application";
    
    
    String PROJECT_CATEGORY_JAVA = "Java";
    String PROJECT_CATEGORY_JAVAFX = "Visage";
    
    // ================   Buttons  ========================
    String BUTTON_OK = "OK";
    String BUTTON_CANCEL = "Cancel";
    String BUTTON_RUN = "Run";

    // ================   Tabs  ========================
    String TAB_PROFILER = "Profiler";

    // ================   Menu items  ========================

    String POPUP_MENU_ITEM_RUN = "Run Project";
    String POPUP_MENU_ITEM_BUILD = "Build Project";
    String POPUP_MENU_ITEM_PROFILE = "Profile";
    String POPUP_MENU_ITEM_DEBUG = "Debug Project";
    String POPUP_MENU_ITEM_PROPERTIES = "Properties";


    // ================   Dialogs  ========================

    String DIALOG_TITLE_ENABLE_PROFILING = "Enable Profiling";
    String DIALOG_TITLE_DEBUG_PROJECT = "Debug Project";
    String DIALOG_TITLE_PROPERTIES = "Project Properties";
    
    // ============    Deployment  ===========================
    String DEPLOYMENT_STANDARD = "Standard Execution";
    String DEPLOYMENT_WEB_START = "Web Start Execution";
    String DEPLOYMENT_BROWSER = "Run in Browser";
    String DEPLOYMENT_MOBILE = "Run in Mobile Emulator";
    String DEPLOYMENT_TV = "Run in TV Emulator";


    String DEPLOYMENT_MOBILE_NOT_INCLUDED = "Current platform does not include mobile device emulator necessary for the execution.";
    // ================   Tests  ========================
    // ============    Smoke  ===========================
    
    String SMOKE_PROFILER_FILE_PATH = "profiler/smoke/Profiler.fx";
    String SMOKE_DEBUGGER_FILE_PATH = "debugger/smoke/Debugger.fx";


    String PALETTE_FILE_PATH = "palette";
    
}
