/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.tools.javac.code.Symbol.MethodSymbol;
import com.sun.tools.javac.code.Symbol.VarSymbol;
import com.sun.tools.javafx.code.JavafxClassSymbol;
import com.sun.tools.javafx.tree.JFXImport;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementUtilities;

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
            fqName = fqName.substring(start);
        }
        fqName = fqName.replace("{", "").replace("}", ""); //NOI18N
        return fqName.trim();
    }

    static boolean checkString(String name) {
        return Pattern.compile("[!@#%^&*(){}\\|:'?/><~`]").matcher(name).find(); //NOI18N
    }

    static boolean isClassUsed(Element foundElement,
            Collection<JFXImport> imports,
            CompilationInfo compilationInfo,
            Collection<Element> allClasses,
            Element superElement) {

        //Check if there are in the same package
        if (foundElement instanceof JavafxClassSymbol && superElement instanceof JavafxClassSymbol) {
            JavafxClassSymbol foundElementClassSymbol = (JavafxClassSymbol) foundElement;
            JavafxClassSymbol superElementClassSymbol = (JavafxClassSymbol) superElement;
            if (superElementClassSymbol.getQualifiedName().equals(foundElementClassSymbol.getQualifiedName())) {
                return true;
            }
            //Check is classes are int the same script
            for (Element elementClass : allClasses) {
                if (elementClass instanceof JavafxClassSymbol) {
                    JavafxClassSymbol elementClassSymbol = (JavafxClassSymbol) elementClass;
                    if (elementClassSymbol.location().equals(foundElementClassSymbol.location())) {
                        return true;
                    }
                }

            }
            //Check imports
            for (JFXImport importTree : imports) {
                if (importTree.toString().contains(".*")) { //NOI18N
                    String importLocation = importTree.toString().substring(0, importTree.toString().lastIndexOf(".")).replace("import ", ""); //NOI18N
                    if (foundElementClassSymbol.location().equals(importLocation)) {
                        return true;
                    }
                } else {
                    JavaFXTreePath path = compilationInfo.getTrees().getPath(compilationInfo.getCompilationUnit(), importTree.getQualifiedIdentifier());
                    Element importElement = compilationInfo.getTrees().getElement(path);
                    if (importElement instanceof JavafxClassSymbol || importElement != null) {
                        JavafxClassSymbol importElementSymbol = (JavafxClassSymbol) importElement;
                        if (foundElementClassSymbol.getQualifiedName().toString().equals(importElementSymbol.getQualifiedName().toString())) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    static MethodSymbol isOverridden(Collection<MethodSymbol> overriddenMethodList, MethodSymbol method, CompilationInfo compilationInfo) {
        if (overriddenMethodList != null && overriddenMethodList.size() != 0) {
            for (MethodSymbol overriddenMethod : overriddenMethodList) {
                String overrriddenName = overriddenMethod.getSimpleName().toString();
                if (!method.getSimpleName().toString().equals(overrriddenName)) {
                    continue;
                }
                if (method.getEnclosingElement() != overriddenMethod.getEnclosingElement()) {
                    continue;
                }
                TypeElement typeOverridden = ElementUtilities.enclosingTypeElement(overriddenMethod);
                if (compilationInfo.getElements().overrides(overriddenMethod, method, typeOverridden)) {
                    return overriddenMethod;
                }
            }
        }

        return null;
    }

     static MethodSymbol isOverridden(TypeElement typeOverridden, Collection<MethodSymbol> overriddenMethodList, MethodSymbol method, CompilationInfo compilationInfo) {
        if (overriddenMethodList != null && overriddenMethodList.size() != 0) {
            for (MethodSymbol overriddenMethod : overriddenMethodList) {
                String overrriddenName = overriddenMethod.getSimpleName().toString();
                if (!method.getSimpleName().toString().equals(overrriddenName)) {
                    continue;
                }
                if (method.getEnclosingElement() != overriddenMethod.getEnclosingElement()) {
                    continue;
                }
                if (compilationInfo.getElements().overrides(overriddenMethod, method, typeOverridden)) {
                    return overriddenMethod;
                }
            }
        }

        return null;
    }


    static MethodSymbol isAlreadyDefined(Collection<MethodSymbol> overriddenMethodList, MethodSymbol method, CompilationInfo compilationInfo) {
        if (overriddenMethodList != null && overriddenMethodList.size() != 0) {
            for (MethodSymbol overriddenMethod : overriddenMethodList) {
                String overrriddenName = overriddenMethod.getSimpleName().toString();
                if (!method.getSimpleName().toString().equals(overrriddenName)) {
                    continue;
                }
                TypeElement typeOverridden = ElementUtilities.enclosingTypeElement(overriddenMethod);
                try {
                    if (compilationInfo.getElementUtilities().alreadyDefinedIn(overrriddenName, method, typeOverridden)
                            || compilationInfo.getElements().overrides(overriddenMethod, method, typeOverridden)) {
                        return overriddenMethod;
                    }
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        }

        return null;
    }
    //TODO Should be replaced with proper formating ASAP

    static String calculateSpace(int startPosition, Document document) {
        String text = null;
        try {
            text = document.getText(document.getStartPosition().getOffset(), startPosition);
        } catch (BadLocationException ex) {
            ex.printStackTrace();
            return "";
        }
        int lastIndex = -1;
        if (text != null && text.length() > 1) {
            lastIndex = text.lastIndexOf("\n"); //NOI18N
        }
        int charNumber = -1;

        if (lastIndex > 0) {
            int varIndex = 0;
            String line = text.substring(lastIndex, startPosition);
            Pattern pattern = Pattern.compile("[a-z]"); //NOI18N
            Matcher matcher = pattern.matcher(line);
            if (matcher.find()) {
                varIndex = matcher.start();
                charNumber = varIndex - 1;
            } else {
                charNumber = line.length();
            }

        }
        if (charNumber < 0) {
            return ""; //NOI18N
        }
        StringBuilder space = new StringBuilder(charNumber);
        for (int i = 0; i < charNumber - 1; i++) {
            space.append(" "); //NOI18M
        }
        return space.toString();
    }

    private static class ParamsComparator implements Comparator<List<VarSymbol>> {

        public int compare(List<VarSymbol> methodList, List<VarSymbol> overriddenMethod) {
            for (VarSymbol var : methodList) {
                VarSymbol overriddenVar = overriddenMethod.get(methodList.indexOf(var));
                if (!var.asType().toString().equals(overriddenVar.asType().toString())) {
                    return -1;
                }
            }
            return 0;
        }
    }
}
