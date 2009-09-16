/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

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

        return methodName;
    }

    static String getObjectName(String fullMethodName) {
        int end = fullMethodName.indexOf("."); //NOI18N
        String className = fullMethodName.substring(0, end).replace("{}","").replace("()", "").trim(); //NOI18N
        return className;
    }

}
