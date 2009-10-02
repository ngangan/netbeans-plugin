/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

import java.util.regex.Pattern;

/**
 *
 * @author karol harezlak
 */

final class HintsUtils {

    private HintsUtils() {
    }

     static String getMethodName(String fullMethodName) {
        String methodName;
        if (fullMethodName.contains(".")) { //NOI18N
            int start = fullMethodName.lastIndexOf("."); //NOI18N
            int end = fullMethodName.length();
            methodName = fullMethodName.substring(start + 1, end).replace("()", "").trim(); //NOI18N
        } else {
            methodName = fullMethodName;
        }

        return methodName.trim();
    }

    static String getClassSimpleName(String fqName) {
        int start = fqName.lastIndexOf(".") + 1; //NOI18N
        if (start > 0) {
            fqName = fqName.substring(start); //NOI18N
        }
        return fqName;
    }

    static boolean checkString(String name) {
        return Pattern.compile("[!@#$%^&*(){}\\|:'?/><~`]").matcher(name).find();
    }

     

}
