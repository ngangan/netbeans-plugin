/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.misc;

import java.io.File;

/**
 *
 * @author Pavel Benes
 */
public class FXDComposerUtils {
    public static boolean safeEquals( Object o1, Object o2) {
        return o1 == o2 || (o1 != null && o1.equals(o2));
    }

    public static String removeEntryExtension( String entryName) {
        int i = entryName.lastIndexOf('.');
        if ( i != -1) {
            entryName = entryName.substring(0, i);
        }
        return entryName;
    }
    
    /**
     * Get the file name without extension.
     * @param file
     * @return
     */
    public static String getFileName( File file) {
        String fileName = file.getName();
        int p = fileName.lastIndexOf('.');
        if ( p != -1) {
            fileName = fileName.substring(0, p);
        }
        return fileName;
    }
}
