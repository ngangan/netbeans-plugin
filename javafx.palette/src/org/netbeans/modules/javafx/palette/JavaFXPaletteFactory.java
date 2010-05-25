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
package org.netbeans.modules.javafx.palette;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.io.IOException;
import org.netbeans.spi.palette.DragAndDropHandler;
import org.netbeans.spi.palette.PaletteController;
import org.netbeans.spi.palette.PaletteFactory;
import org.netbeans.spi.palette.PaletteFilter;
import org.openide.util.Lookup;
import org.openide.util.datatransfer.ExTransferable;

/**
 *
 * @author Michal Skvor
 */
public class JavaFXPaletteFactory {

    public static final String JAVAFX_PALETTE_FOLDER = "JavaFXPalette"; // NOI18N
    static private PaletteController palette = null;

    public static synchronized PaletteController getPalette() throws IOException {

        if (palette == null) {
            //palette = PaletteFactory.createPalette(JAVAFX_PALETTE_FOLDER, new JavaFXPaletteActions());
            PaletteFilter pf = new PaletteFilter() {

                @Override
                public boolean isValidCategory(Lookup lkp) {
                    return true;
                }

                @Override
                public boolean isValidItem(Lookup lkp) {
                    return true;
                }
            };

            DragAndDropHandler dndh = new DragAndDropHandler() {

                @Override
                public void customize(ExTransferable et, Lookup lkp) {
                }

                @Override
                public boolean canDrop(Lookup lkp, DataFlavor[] dfs, int i) {
                    return false;
                }

                @Override
                public boolean doDrop(Lookup lkp, Transferable t, int i, int i1) {
                    return false;
                }

                @Override
                public boolean canReorderCategories(Lookup lkp) {
                    return false;
                }

                @Override
                public boolean moveCategory(Lookup lkp, int i) {
                    return false;
                }
            };

            palette = PaletteFactory.createPalette(JAVAFX_PALETTE_FOLDER, new JavaFXPaletteActions(), pf, dndh);
        }
        return palette;
    }
}
