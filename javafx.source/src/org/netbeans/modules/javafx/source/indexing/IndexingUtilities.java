/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
    final public static String INDEX_SEPARATOR = "#";
    @SuppressWarnings(value="unchecked")
    public static String getIndexValue(ElementHandle<? extends Element> eeh) {
        switch (eeh.getKind()) {
            case CLASS: {
                return eeh.getSignatures()[0];
            }
            case FIELD:
            case METHOD: {
                return eeh.getSignatures()[1] + INDEX_SEPARATOR + convertInternalSignature(eeh.getSignatures()[2]) + INDEX_SEPARATOR + eeh.getSignatures()[0];
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
