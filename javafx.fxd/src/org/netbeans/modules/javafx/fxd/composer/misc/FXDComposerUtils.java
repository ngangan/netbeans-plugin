/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.misc;

/**
 *
 * @author Pavel Benes
 */
public class FXDComposerUtils {
    public static boolean safeEquals( Object o1, Object o2) {
        return o1 == o2 || (o1 != null && o1.equals(o2));
    }
}
