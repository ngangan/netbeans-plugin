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


package org.netbeans.modules.javafx.debugger.utils;

import com.sun.javafx.jdi.FXClassType;
import com.sun.javafx.jdi.FXObjectReference;
import com.sun.jdi.Field;
import com.sun.jdi.Value;
import java.net.MalformedURLException;
import java.net.URL;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.modules.debugger.jpda.JPDADebuggerImpl;
import org.netbeans.modules.debugger.jpda.models.JPDAThreadImpl;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;

/**
 *
 * @author Michal Skvor
 */
public class Utils {

    public static String getContextPath( String url ) {
        FileObject fo = getFileObjectFromUrl(url);

        return fo.getNameExt();
    }

    public static FileObject getFileObjectFromUrl( String url ) {
        FileObject fo = null;

        try {
            fo = URLMapper.findFileObject( new URL( url ));
        } catch( MalformedURLException e ) {
            //noop
        }

        return fo;
    }

    private static DataObject getDataObject (String url) {
        FileObject file;
        try {
            file = URLMapper.findFileObject( new URL( url ));
        } catch( MalformedURLException e ) {
            return null;
        }

        if( file == null ) return null;
        try {
            return DataObject.find( file );
        } catch( DataObjectNotFoundException ex ) {
            return null;
        }
    }


    public static String getFXName( String url ) {
        FileObject fo = getFileObjectFromUrl( url );
        if( fo != null ) {
            return fo.getNameExt();
        }
        return ( url == null ) ? null : url.toString();
    }

    public static String getFXPath( String url ) {
        FileObject fo = getFileObjectFromUrl( url );
        String fxRelativePath = null;
        if( fo == null ) return null;
        Project project = FileOwnerQuery.getOwner( fo );

        Sources s = ProjectUtils.getSources( project );
        SourceGroup[] sg = s.getSourceGroups( JavaProjectConstants.SOURCES_TYPE_JAVA );
        for( int i = 0; i < sg.length; i++ ) {
            fxRelativePath = FileUtil.getRelativePath( sg[i].getRootFolder(), fo );
            if( fxRelativePath != null ) break;
        }
        
        return fxRelativePath;
    }

    public static String getFXClassName( String url, int lineNumber ) {
        String clsName = getFXPath(url);
        if( clsName == null || !clsName.endsWith( ".fx" )) return null;


        clsName = clsName.substring( 0, clsName.length() - 3 ).replace( '/', '.' );
        return clsName;
    }
    
    public static Value getClassValue( JPDADebuggerImpl debug, FXClassType classType, Field field ) {
        JPDAThreadImpl thread = (JPDAThreadImpl)debug.getCurrentThread();  
        if( thread == null ) {
            return null;
        }
        thread.accessLock.writeLock().lock();

        Value value = null;
        try {            
            if( !thread.isSuspended()) {
                return null;
            }        
            value = classType.getValue( field );
        } finally {
            thread.accessLock.writeLock().unlock();
        }          
        return value;
}
    
    public static Value getObjectValue( JPDADebuggerImpl debug, FXObjectReference classType, Field field ) {
        JPDAThreadImpl thread = (JPDAThreadImpl)debug.getCurrentThread();  
        if( thread == null ) {
            return null;
        }
        thread.accessLock.writeLock().lock();

        Value value = null;
        try {            
            if( !thread.isSuspended()) {
                return null;
            }        
           value = classType.getValue( field );
        } finally {
            thread.accessLock.writeLock().unlock();
        }          
        return value;
    }
}
