/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 2008 Sun Microsystems, Inc. All rights reserved.
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

package org.netbeans.modules.javafx.preview;

import java.awt.Graphics;
import java.io.*;
import java.awt.image.BufferedImage;

public class SerializableImage implements java.io.Serializable {
    private transient BufferedImage image = null;
    
    public SerializableImage(BufferedImage image) {
        this.image = image;
    }
    
    public Graphics getGraphics() {
        return image.getGraphics();
    }
    
    public BufferedImage getBufferedImage() {
        return image;
    }

    private void writeObject(ObjectOutputStream s) throws IOException {
        s.defaultWriteObject();
        int arr[] = new int[image.getWidth()* image.getHeight()];
        image.getRGB(0, 0, image.getWidth(), image.getHeight(), arr, 0, image.getWidth());
        s.writeInt(image.getWidth());
        s.writeInt(image.getHeight());
        s.writeInt(image.getType());
        s.writeObject(arr);
    }

    private void readObject(ObjectInputStream s) throws IOException {
        try {
            s.defaultReadObject();
            int width = s.readInt();
            int height = s.readInt();
            int type =  s.readInt();
            image = new BufferedImage(width, height, type);
            int arr[] = new int[width * height];
            arr = (int[]) s.readObject();
            image.setRGB(0, 0, width, height, arr, 0, width);
        } catch (ClassNotFoundException ex) {
            ex.printStackTrace();
        }
            
    }
}