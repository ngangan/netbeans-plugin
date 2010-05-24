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


package org.netbeans.modules.javafx.source.indexing;

import java.util.StringTokenizer;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeKind;
import org.netbeans.api.javafx.source.ElementHandle;

/**
 *
 * @author Jaroslav Bachorik
 */
final public class IndexingUtilities {
    final public static String INDEX_SEPARATOR = "#"; // NOI18N
    final public static String DEFAULT_PACKAGE = "<default>"; // NOI18N
    @SuppressWarnings(value="unchecked")
    public static String getIndexValue(ElementHandle<? extends Element> eeh) {
        switch (eeh.getKind()) {
            case PACKAGE:
            case CLASS:
            case INTERFACE:
            case ENUM: {
                return eeh.getSignatures()[0].replace("$", ".");
            }
            case FIELD:
            case LOCAL_VARIABLE:
            case PARAMETER:
            case METHOD: {
                return eeh.getSignatures()[1] + INDEX_SEPARATOR + convertInternalSignature(eeh.getSignatures()[2].replace("$", ".")) + INDEX_SEPARATOR + eeh.getSignatures()[0].replace("$", ".");
            }
            default: {
                return eeh.toString();
            }
        }

    }

    public static ElementHandle<TypeElement> getLocationHandle(String index) {
        String[] tokens = tokenize(index);
        assert tokens.length == 4;

        return new ElementHandle<TypeElement>(ElementKind.CLASS, new String[]{tokens[3]});
    }

    public static ElementHandle<ExecutableElement> getMethodHandle(String index) {
        String[] tokens = tokenize(index);
        assert tokens.length == 3;

        return new ElementHandle(ElementKind.METHOD, new String[]{tokens[2], tokens[0], encodeType(tokens[1])});
    }

    public static ElementHandle<VariableElement> getFieldHandle(String index) {
        String[] tokens = tokenize(index);
        assert tokens.length == 4;

        return new ElementHandle(ElementKind.FIELD, new String[]{tokens[2], tokens[0], encodeType(tokens[1])});
    }

    public static ElementHandle<TypeElement> getTypeHandle(String index) {
        String[] tokens = tokenize(index);
        assert tokens.length == 1;

        return new ElementHandle(ElementKind.CLASS, new String[]{tokens[0]});
    }

    private static String[] tokenize(String index) {
        StringTokenizer st = new StringTokenizer(index, INDEX_SEPARATOR);
        String[] tokens = new String[st.countTokens()];
        int tokenIndex = 0;
        while(st.hasMoreTokens()) {
            tokens[tokenIndex++] = st.nextToken();
        }
        return tokens;
    }

    private static String convertInternalSignature(String signature) {
        StringBuilder sb = new StringBuilder();
        int startParen = signature.indexOf("(");
        int stopParen = signature.lastIndexOf(")");
        if (startParen > -1 && stopParen > -1) {
            sb.append("(");
            String toProcess = signature.substring(startParen + 1, stopParen);
            StringTokenizer st = new StringTokenizer(toProcess, ";");
            while(st.hasMoreTokens()) {
                String type = convertInternalType(st.nextToken());
                if (type != null) {
                    if (sb.length() > 1) {
                        sb.append(";");
                    }
                    sb.append(type);
                }
            }
            sb.append(";)");
            sb.append(convertInternalType(signature.substring(stopParen + 1)));
        } else {
            sb.append(convertInternalType(signature)); // field type
        }
        return sb.toString();
    }

    static private String convertInternalType(String type) {
        char firstLetter = type.charAt(0);
        switch(firstLetter) {
            case 'L': {
                return type.substring(1).replace("/", ".");
            }
            case 'V': {
                return TypeKind.VOID.toString().toLowerCase() + (type.length() > 0 ? type.substring(1) : "");
            }
            case 'Z': {
                return TypeKind.BOOLEAN.toString().toLowerCase() + (type.length() > 0 ? type.substring(1) : "");
            }
            case 'B': {
                return TypeKind.BYTE.toString().toLowerCase() + (type.length() > 0 ? type.substring(1) : "");
            }
            case 'C': {
                return TypeKind.CHAR.toString().toLowerCase() + (type.length() > 0 ? type.substring(1) : "");
            }
            case 'I': {
                return TypeKind.INT.toString().toLowerCase() + (type.length() > 0 ? type.substring(1) : "");
            }
            case 'J': {
                return TypeKind.LONG.toString().toLowerCase() + (type.length() > 0 ? type.substring(1) : "");
            }
            case 'S': {
                return TypeKind.SHORT.toString().toLowerCase() + (type.length() > 0 ? type.substring(1) : "");
            }
            case 'D': {
                return TypeKind.DOUBLE.toString().toLowerCase() + (type.length() > 0 ? type.substring(1) : "");
            }
            case 'F': {
                return TypeKind.FLOAT.toString().toLowerCase() + (type.length() > 0 ? type.substring(1) : "");
            }
            default: {
                return null;
            }
        }
    }

    private static String encodeType(String type) {
        StringBuilder sb = new StringBuilder();
        int arrayStart = type.indexOf("[");
        String arrayDef = "";
        if (arrayStart > -1) {
            arrayDef = type.substring(arrayStart);
            type = type.substring(0, arrayStart - 1);
        }
        if (type.equals("void")) {
            sb.append('V');	    // NOI18N
        } else if (type.equals("boolean")) {
            sb.append('Z');	    // NOI18N
        } else if (type.equals("byte")) {
            sb.append('B');	    // NOI18N
        } else if (type.equals("short")) {
            sb.append('S');	    // NOI18N
        } else if (type.equals("int")) {
            sb.append('I');	    // NOI18N
        } else if (type.equals("long")) {
            sb.append('J');	    // NOI18N
        } else if (type.equals("char")) {
            sb.append('C');	    // NOI18N
        } else if (type.equals("float")) {
            sb.append('F');	    // NOI18N
        } else if (type.equals("double")) {
            sb.append('D');	    // NOI18N
        } else {
            sb.append('L');	    // NOI18N
            sb.append(type.replace(".", "/"));
        }
        sb.append(arrayDef);
        return sb.toString();
    }
}
