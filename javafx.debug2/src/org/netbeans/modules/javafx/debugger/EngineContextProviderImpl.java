/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.debugger;

import java.beans.PropertyChangeListener;
import java.util.logging.Logger;
import org.netbeans.spi.debugger.jpda.SourcePathProvider;

/**
 *
 * @author Michal Skvor
 */
@SourcePathProvider.Registration(path="")
public class EngineContextProviderImpl extends SourcePathProvider {

    private static final Logger LOGGER = Logger.getLogger( EngineContextProviderImpl.class.getName());

    @Override
    public String getRelativePath( String url, char directorySeparator, boolean includeExtension ) {
        return null;
    }

    @Override
    public String getURL( String relativePath, boolean global ) {
        return null;
    }

    @Override
    public String[] getSourceRoots() {
        return new String[0];
    }

    @Override
    public void setSourceRoots( String[] sourceRoots ) {
    }

    @Override
    public String[] getOriginalSourceRoots() {
        return new String[0];
    }

    @Override
    public void addPropertyChangeListener( PropertyChangeListener l ) {
    }

    @Override
    public void removePropertyChangeListener( PropertyChangeListener l ) {
    }

}
