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

package org.netbeans.modules.javafx.project;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class JnlpDownloadServlet extends HttpServlet {
    
    private static final long serialVersionUID = 5518842704648404245L;
   
    public void doHead(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        handleRequest(request, response, true);
    }

    public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        handleRequest(request, response, false);
    }
    
    private void handleRequest(HttpServletRequest request, HttpServletResponse response, boolean isHead) throws IOException {
        try {
            File f = new File(URLDecoder.decode(request.getRequestURI().substring(request.getServletPath().length()).trim(), "UTF-8")); // NOI18N
            if (f.exists()) {
                response.setContentLength((int)f.length());
                if (f.lastModified() != 0) response.setDateHeader("Last-Modified", f.lastModified()); // NOI18N
                String name = f.getName();
                if (name.endsWith(".pack.gz")) { // NOI18N
                    response.setHeader("content-encoding", "pack200-gzip"); // NOI18N
                } else if (name.endsWith(".gz")) { // NOI18N
                    response.setHeader("content-encoding", "gzip"); // NOI18N
                } else if (name.endsWith(".jar")) { // NOI18N
                    response.setContentType("application/x-java-archive"); // NOI18N
                } else if (name.endsWith(".jnlp")) { // NOI18N
                    response.setContentType("application/x-java-jnlp-file"); // NOI18N
                }
                if (isHead) {
                    response.sendError(HttpServletResponse.SC_OK);
                } else {
                    FileInputStream in = new FileInputStream(f);
                    try {
                        OutputStream out = response.getOutputStream();
                        try {
                            byte[] bytes = new byte[32 * 1024];
                            int read;
                            while ((read = in.read(bytes)) != -1) {
                                out.write(bytes, 0, read);
                            }
                        } finally {
                            if (out != null) out.close();
                        }            
                    } finally {
                        if (in != null) in.close();
                    }            
                }
            } else {
                response.sendError(HttpServletResponse.SC_NOT_FOUND, f.getAbsolutePath());
            }
        } catch (UnsupportedEncodingException e) {
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
        }
    }
     
}
