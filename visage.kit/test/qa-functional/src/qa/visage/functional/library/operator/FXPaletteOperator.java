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

package qa.javafx.functional.library.operator;

import java.awt.Point;
import javax.swing.JComponent;
import org.netbeans.jellytools.PaletteOperator;
import org.netbeans.jemmy.operators.JCheckBoxOperator;
import org.netbeans.jemmy.operators.JComponentOperator;
import org.netbeans.jemmy.operators.JListOperator;
import qa.javafx.functional.library.MouseRobot;

/**
 *
 * @author andromeda
 */
public class FXPaletteOperator extends PaletteOperator{

    public void expand(String category){
        expand( new JCheckBoxOperator(this, category), true);
    }

    public JListOperator getList(String category){
        expand(category);
        //Util.showComponents(this);
        JComponentOperator comp = new JComponentOperator(this, new ClassNameComponentChooser("CategoryButton", category));
        comp = new JComponentOperator((JComponent) comp.getParent());
        //return new JListOperator(comp, new ClassNameComponentChooser("", category));
        return new JListOperator(comp);
        
    }

    public void dragNDrop(JComponentOperator comp){
        JListOperator list = lstComponents();
        Point point = list.getClickPoint(0);

        int x1 = point.x;
        int y1 = point.y;

        int x2 = comp.getCenterXForClick();
        int y2 = comp.getCenterYForClick();

        System.out.println("[palette] Drag and Drop");

        MouseRobot.dragNDrop(list, x1, y1, comp, x2, y2);

        
    }

     public void dragNDrop(JListOperator list, int index, JComponentOperator comp, int x, int y){
        Point point = list.getClickPoint(index);
        
        int x1 = point.x;
        int y1 = point.y;

        System.out.println("[palette] Drag and Drop");

        MouseRobot.dragNDrop(list, x1, y1, comp, x, y);

        
    }

    public void dragNDrop(JListOperator list, int index, JComponentOperator comp){
        //JListOperator list = lstComponents();
        Point point = list.getClickPoint(index);

        int x1 = point.x;
        int y1 = point.y;

        int x2 = comp.getCenterXForClick();
        int y2 = comp.getCenterYForClick();

        System.out.println("[palette] Drag and Drop");

        MouseRobot.dragNDrop(list, x1, y1, comp, x2, y2);


    }




}
