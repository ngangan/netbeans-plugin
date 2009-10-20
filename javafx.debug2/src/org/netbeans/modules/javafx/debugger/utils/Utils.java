/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger.utils;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree.JavaFXKind;
import com.sun.tools.javafx.api.JavafxcScope;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import javax.lang.model.element.TypeElement;
import javax.swing.text.StyledDocument;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementUtilities;
import org.netbeans.api.javafx.source.JavaFXSource;
import org.netbeans.api.javafx.source.JavaFXSource.Phase;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.modules.javafx.debugger.Context;
import org.openide.ErrorManager;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;
import org.openide.text.NbDocument;

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
}
