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

package mathematics.graph.simple;

import visage.scene.shape.Line;
import mathematics.graph.GEdge;
import visage.scene.Group;
import visage.scene.text.Text;
import visage.scene.text.Font;
import visage.scene.paint.Color;

/**
 * @author andromeda
 */
public class GSimpleEdge extends GEdge {


//    public var view:GView = GView.VIEW;
//    public var shape:function():Node;

    //override var view = GView{};
    public var view = GView{};
    public var text: String;

    //override var shape = function() {
    public var shape = function() {
        //println("[simple edge] {vertex1}, {vertex2}");
        //println(this);

        var v1 = vertex1 as GSimpleVertex;
        var v2 = vertex2 as GSimpleVertex;

        Group{

        content: [
             Line{
                startX: bind v1.centerX
                startY: bind v1.centerY
                endX: bind v2.centerX
                endY: bind v2.centerY
                //stroke: Color.ORANGE
                stroke: bind view.stroke
            }
            Text{
                x: bind (v1.centerX + v2.centerX) / 2
                y: bind (v1.centerY + v2.centerY) / 2
                font: Font{ size: 26 }
                content: bind text
                fill: Color.BLUE
            }

            ]
        }

    }

    override function toString() {
        "Edge {vertex1}, {vertex2}"
    }

}
