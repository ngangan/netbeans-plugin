/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

import com.sun.tools.javac.code.Symbol.MethodSymbol;
import com.sun.tools.javac.code.Symbol.VarSymbol;
import com.sun.tools.javafx.code.JavafxClassSymbol;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Pattern;
import javax.lang.model.element.Element;

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

    static boolean isClassUsed(Element element, Collection<JavafxClassSymbol> imports, Element currentClass) {
        for (JavafxClassSymbol importElement : imports) {
            if (element instanceof JavafxClassSymbol && currentClass instanceof JavafxClassSymbol) {
                JavafxClassSymbol classTypeElement = (JavafxClassSymbol) element;
                JavafxClassSymbol currentClassSymbol = (JavafxClassSymbol) currentClass;
                if (currentClassSymbol.location().equals(importElement.location())) {
                    return true;
                } else if (classTypeElement == importElement) {
                    return true;
                }
            }
        }
        return false;
    }
    private static final Comparator<List<VarSymbol>> COMPARATOR = new ParamsComparator();

    static MethodSymbol isOverriden(Collection<MethodSymbol> overridenMethodList, MethodSymbol method) {

        if (overridenMethodList != null && overridenMethodList.size() != 0) {
            for (MethodSymbol overridenMethod : overridenMethodList) {
                //TODO Work around to avoid NPE at com.sun.tools.javac.code.Symbol$MethodSymbol.params(Symbol.java:1201)!
                try {
                    if (method.getQualifiedName().equals(overridenMethod.getQualifiedName()) &&
                            method.getParameters().size() == overridenMethod.getParameters().size() &&
                            COMPARATOR.compare(method.getParameters(), overridenMethod.getParameters()) == 0) {
                        return overridenMethod;
                    }
                } catch (Exception ex) {
                    ex.printStackTrace();
                    continue;
                }
            }
        }

        return null;
    }

    private static class ParamsComparator implements Comparator<List<VarSymbol>> {

        public int compare(List<VarSymbol> methodList, List<VarSymbol> overridenMethod) {
            for (VarSymbol var : methodList) {
                VarSymbol overridenVar = overridenMethod.get(methodList.indexOf(var));
                if (!var.asType().toString().equals(overridenVar.asType().toString())) {
                    return -1;
                }
            }
            return 0;
        }
    }
}
