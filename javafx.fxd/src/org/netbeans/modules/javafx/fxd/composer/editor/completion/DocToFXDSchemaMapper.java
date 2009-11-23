/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2008 Sun
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
package org.netbeans.modules.javafx.fxd.composer.editor.completion;

import java.util.HashMap;
import java.util.Map;

/**
 * Pavel Beneš:
 * currently it is defined (in code) like this:
 * private static final String[] DEFAULT_IMPORTS = {
 * "javafx.fxd.",
 * "javafx.geometry.",
 * "javafx.scene.",
 * "javafx.scene.effect.",
 * "javafx.scene.effect.light.",
 * "javafx.scene.image.",
 * "javafx.scene.paint.",
 * "javafx.scene.shape.",
 * "javafx.scene.text.",
 * "javafx.scene.transform."
 * };
 * so correct way how to get fully qualified name
 * is to try all default prefixes
 * and try to find element with the constructed id in schema
 * currently it is not implemented
 *
 * @author Andrey Korostelev
 */
public class DocToFXDSchemaMapper {

    public static final Map<String, String> DOCUMENT_ID_TO_FX_ID =
            new HashMap<String, String>() {
                {
                    //Enum
                    put("BlendMode", "javafx-scene-effect-BlendMode");
                    put("CycleMethod", "javafx-scene-paint-CycleMethod");
                    put("ArcType", "javafx-scene-shape-ArcType");
                    put("FillRule", "javafx-scene-shape-FillRule");
                    put("StrokeLineJoin", "javafx-scene-shape-StrokeLineJoin");
                    put("StrokeLineCap", "javafx-scene-shape-StrokeLineCap");
                    put("FontPosition", "javafx-scene-text-FontPosition");
                    put("FontWeight", "javafx-scene-text-FontWeight");
                    put("FontPosture", "javafx-scene-text-FontPosture");
                    put("TextOrigin", "javafx-scene-text-TextOrigin");
                    put("TextAlignment", "javafx-scene-text-TextAlignment");
                    put("BlurType", "javafx-scene-effect-BlurType");
                    //Element
                    put("FXDNode", "javafx-fxd-FXDNode");
                    put("Rectangle2D", "javafx-geometry-Rectangle2D");
                    put("Group", "javafx-scene-Group");
                    put("Node", "javafx-scene-Node");
                    put("Parent", "javafx-scene-Parent");
                    put("Blend", "javafx-scene-effect-Blend");
                    put("Bloom", "javafx-scene-effect-Bloom");
                    put("ColorAdjust", "javafx-scene-effect-ColorAdjust");
                    put("DisplacementMap", "javafx-scene-effect-DisplacementMap");
                    put("DropShadow", "javafx-scene-effect-DropShadow");
                    put("Effect", "javafx-scene-effect-Effect");
                    put("FloatMap", "javafx-scene-effect-FloatMap");
                    put("Flood", "javafx-scene-effect-Flood");
                    put("GaussianBlur", "javafx-scene-effect-GaussianBlur");
                    put("Glow", "javafx-scene-effect-Glow");
                    put("Identity", "javafx-scene-effect-Identity");
                    put("InnerShadow", "javafx-scene-effect-InnerShadow");
                    put("InvertMask", "javafx-scene-effect-InvertMask");
                    put("Lighting", "javafx-scene-effect-Lighting");
                    put("MotionBlur", "javafx-scene-effect-MotionBlur");
                    put("PerspectiveTransform", "javafx-scene-effect-PerspectiveTransform");
                    put("Reflection", "javafx-scene-effect-Reflection");
                    put("SepiaTone", "javafx-scene-effect-SepiaTone");
                    put("Shadow", "javafx-scene-effect-Shadow");
                    put("BoxBlur", "javafx-scene-effect-BoxBlur");
                    put("DistantLight", "javafx-scene-effect-light-DistantLight");
                    put("Light", "javafx-scene-effect-light-Light");
                    put("PointLight", "javafx-scene-effect-light-PointLight");
                    put("SpotLight", "javafx-scene-effect-light-SpotLight");
                    put("Image", "javafx-scene-image-Image");
                    put("ImageView", "javafx-scene-image-ImageView");
                    put("Color", "javafx-scene-paint-Color");
                    put("LinearGradient", "javafx-scene-paint-LinearGradient");
                    put("Paint", "javafx-scene-paint-Paint");
                    put("RadialGradient", "javafx-scene-paint-RadialGradient");
                    put("Stop", "javafx-scene-paint-Stop");
                    put("Arc", "javafx-scene-shape-Arc");
                    put("ArcTo", "javafx-scene-shape-ArcTo");
                    put("Circle", "javafx-scene-shape-Circle");
                    put("ClosePath", "javafx-scene-shape-ClosePath");
                    put("CubicCurve", "javafx-scene-shape-CubicCurve");
                    put("CubicCurveTo", "javafx-scene-shape-CubicCurveTo");
                    put("DelegateShape", "javafx-scene-shape-DelegateShape");
                    put("Ellipse", "javafx-scene-shape-Ellipse");
                    put("HLineTo", "javafx-scene-shape-HLineTo");
                    put("Line", "javafx-scene-shape-Line");
                    put("LineTo", "javafx-scene-shape-LineTo");
                    put("MoveTo", "javafx-scene-shape-MoveTo");
                    put("Path", "javafx-scene-shape-Path");
                    put("PathElement", "javafx-scene-shape-PathElement");
                    put("Polygon", "javafx-scene-shape-Polygon");
                    put("Polyline", "javafx-scene-shape-Polyline");
                    put("QuadCurve", "javafx-scene-shape-QuadCurve");
                    put("QuadCurveTo", "javafx-scene-shape-QuadCurveTo");
                    put("Rectangle", "javafx-scene-shape-Rectangle");
                    put("SVGPath", "javafx-scene-shape-SVGPath");
                    put("Shape", "javafx-scene-shape-Shape");
                    put("ShapeIntersect", "javafx-scene-shape-ShapeIntersect");
                    put("ShapeSubtract", "javafx-scene-shape-ShapeSubtract");
                    put("VLineTo", "javafx-scene-shape-VLineTo");
                    put("Font", "javafx-scene-text-Font");
                    put("Text", "javafx-scene-text-Text");
                    put("Affine", "javafx-scene-transform-Affine");
                    put("Rotate", "javafx-scene-transform-Rotate");
                    put("Scale", "javafx-scene-transform-Scale");
                    put("Shear", "javafx-scene-transform-Shear");
                    put("Transform", "javafx-scene-transform-Transform");
                    put("Translate", "javafx-scene-transform-Translate");
                    put("Element", "javafx-fxd-Element");
                }
            };

    /**
     * returns fxd element or enumeration id from fxd schema associated with
     * given node id from document model.
     * @param docNodeId document model node id
     * @return fxd schema element or enumeration id
     */
    public static String getFXDSchemaId(String docNodeId) {
        return DOCUMENT_ID_TO_FX_ID.get(docNodeId);
    }
}
