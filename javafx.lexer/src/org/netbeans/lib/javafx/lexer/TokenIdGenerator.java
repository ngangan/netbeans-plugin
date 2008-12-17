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
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.lib.javafx.lexer;

import org.netbeans.api.javafx.lexer.JFXTokenId;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @todo documentation
 */
public class TokenIdGenerator {
    public static final String FORMAT = "\t{0}(\"{1}\", {2}),"; // NOI18N


    public static void main(String[] args) {
        if (args == null || args.length == 0) {
            System.exit(-2);
        }

        String tokens = get("-tokens", args); // NOI18N
        File tokenF = new File(tokens);
        if (tokens == null || !tokenF.exists()) {
            System.out.println("Use -tokens to specify token input file."); // NOI18N
            System.exit(-1);
        }

        BufferedReader r = null;
        JFXTokenId[] tokenIds = JFXTokenId.values();
        Map<String, String> nameToCat = new HashMap<String, String>(tokenIds.length);
        for (JFXTokenId tokenId : tokenIds) {
            nameToCat.put(tokenId.name(), tokenId.primaryCategory());
        }
        try {
            r = new BufferedReader(new FileReader(tokenF));
            MessageFormat f = new MessageFormat(FORMAT);
            String line;            
            while ((line = r.readLine()) != null) {
                if (line.startsWith("\'")) continue;
                String[] elements = line.split("=");
                String name = elements[0];
                String category;
                if (nameToCat.containsKey(name)) {
                    category = nameToCat.get(name);
                } else {
                    category = JFXTokenId.UNKNOWN.primaryCategory();
                }
                System.out.println(f.format(new Object[]{name, category, elements[1]}));
            }
            System.exit(0);
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(-2);
        } finally {
            if (r != null) try {
                r.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }


    private static String get(String param, String[] args) {
        int index = Arrays.binarySearch(args, param);
        if (index < 0) return null;
        if (index == args.length - 1) return null;
        return args[index + 1];
    }

}
