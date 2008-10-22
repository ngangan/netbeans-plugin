/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.uistub;

import org.netbeans.modules.javafx.fxd.dataloader.fxz.FXZDataObject;
import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.NbBundle;
import org.openide.util.actions.CookieAction;

/**
 *
 * @author Pavel Benes
 */
public class GenerateUIStubAction extends CookieAction {

    public GenerateUIStubAction() {
    }

    protected int mode() {
        return CookieAction.MODE_EXACTLY_ONE;
    }

    protected Class<?>[] cookieClasses() {
        return new Class[] {
            FXZDataObject.class
        };
    }

    protected void performAction(Node[] activatedNodes) {
        FXZDataObject doj = (FXZDataObject) activatedNodes[0].getLookup().lookup(FXZDataObject.class);
        if (doj != null) {
            UIStubGenerator generator = new UIStubGenerator(doj);
            generator.generate();
        }
    }

    public String getName() {
         return NbBundle.getMessage(GenerateUIStubAction.class, "CTL_GenerateUIStubAction");  //NOI18N
    }

    public HelpCtx getHelpCtx() {
        return HelpCtx.DEFAULT_HELP;
    }
    
    @Override
    public boolean asynchronous() {
        return false;
    }
}
