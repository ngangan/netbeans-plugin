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


package org.netbeans.installer.javafx;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.PrintStream;
import java.security.MessageDigest;
import java.util.Enumeration;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.zip.ZipEntry;
import org.apache.tools.zip.ZipFile;
import org.apache.tools.zip.ZipOutputStream;

/**
 *
 * @author Adam
 */
public class FilesListTask extends Task {

    private File src, tar;
    private static char[] HEX = new char[] {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};

    public void setFile(File zip) {
        this.src = zip;
    }

    public void setToFile(File zip) {
        this.tar = zip;
    }

    @Override
    public void execute() throws BuildException {
        try {
            ZipFile f = new ZipFile(src);
            ZipOutputStream out = new ZipOutputStream(tar);
            ByteArrayOutputStream lArray = new ByteArrayOutputStream();
            PrintStream list = new PrintStream(lArray, false, "UTF-8");
            list.print("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<files-list>\n");
            out.setEncoding(f.getEncoding());
            Enumeration en = f.getEntries();
            MessageDigest md5 = MessageDigest.getInstance("MD5");
            byte buf[] = new byte[16384];
            while (en.hasMoreElements()) {
                ZipEntry ze = (ZipEntry)en.nextElement();
                out.putNextEntry(ze);
                if (ze.isDirectory()) {
                    list.print("    <entry type=\"directory\" empty=\"false\" modified=\"" + ze.getTime() + "\" permissions=\"" + Integer.toOctalString(ze.getUnixMode() & 0x1ff) + "\">" + ze.getName() + "</entry>\n");
                    getProject().log(ze.getName() + ' ' + Integer.toOctalString(ze.getUnixMode() & 0x1ff) + ' ' + ze.getTime(), Project.MSG_VERBOSE);
                } else {
                    md5.reset();
                    InputStream in = f.getInputStream(ze);
                    int i;
                    while ((i = in.read(buf)) >= 0) {
                        out.write(buf, 0, i);
                        md5.update(buf, 0, i);
                    }
                    in.close();
                    byte dig[] = md5.digest();
                    list.print("    <entry type=\"file\" size=\"" + ze.getSize() + "\" md5=\"");
                    for (i=0; i<dig.length; i++) list.print("" + HEX[((int)dig[i] & 0xf0) >> 4] + HEX[dig[i] & 0xf]);
                    list.print("\" jar=\"" + ze.getName().endsWith(".jar") + "\" packed=\"false\" signed=\"false\" modified=\"" + ze.getTime() + "\" permissions=\"" + Integer.toOctalString(ze.getUnixMode() & 0x1ff) + "\">" + ze.getName() + "</entry>\n");
                    getProject().log(ze.getName() + ' ' + Integer.toOctalString(ze.getUnixMode() & 0x1ff) + ' ' + ze.getTime() + ' ' + + ze.getSize(), Project.MSG_VERBOSE);
                }
                out.closeEntry();
            }
            list.println("</files-list>\n");
            list.close();
            out.putNextEntry(new ZipEntry("META-INF/files.list"));
            out.write(lArray.toByteArray());
            out.closeEntry();
            out.close();
        } catch (Exception e) {
            throw new BuildException(e);
        }
    }

}
