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
 * Portions Copyrighted 2007 Sun Microsystems, Inc.
 */

package org.netbeans.visage.preview;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Window;
import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;
import javax.swing.SwingUtilities;

/**
 *
 * @author Adam
 */
public class ScreenShot {

    public static void screenShot(final Window w) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                if (w.getComponents().length > 0) try {
                    w.addNotify();
                    w.validate();
                    Component c = w.getComponents()[0];
                    Dimension bounds = c.getSize();
                    BufferedImage bi = new BufferedImage(bounds.width, bounds.height, BufferedImage.TYPE_INT_RGB);
                    Graphics g = bi.getGraphics();
                    c.print(g);
                    ImageIO.write(bi, "PNG", SilentPremain.out); //NOI18N
                } catch (Exception ex) {
                    ex.printStackTrace();
                    SilentPremain.out.close();
                    System.exit(1);
                }
                System.exit(0);
            }
        });
    }

}
