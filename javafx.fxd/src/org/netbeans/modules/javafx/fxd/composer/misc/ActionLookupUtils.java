/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.misc;

import javax.swing.Action;

/**
 *
 * @author Pavel Benes
 */
public abstract class ActionLookupUtils {
    private ActionLookupUtils() {        
    }
    
    public static Action get( final Action [] actions, Class clazz) {
        for ( Action action : actions) {
            if ( action.getClass().equals(clazz)) {
                return action;
            }            
        }
        return null;
    }
    
    public static ActionLookup merge( final ActionLookup[] lookups) {
        return new ActionLookup() {
            public Action get(Class clazz) {
                for ( ActionLookup lookup: lookups) {
                    Action a = lookup.get(clazz);
                    if (a != null) {
                        return a;
                    }
                }
                return null;
            }
        };
    }    
}
