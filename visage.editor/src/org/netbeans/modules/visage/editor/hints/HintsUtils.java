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

package org.netbeans.modules.visage.editor.hints;

import com.sun.tools.mjavac.code.Symbol.MethodSymbol;
import com.sun.tools.visage.code.JavafxClassSymbol;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.netbeans.api.visage.source.CompilationInfo;
import org.netbeans.api.visage.source.ElementUtilities;
import org.netbeans.modules.visage.editor.VisageDocument;
import org.openide.util.Exceptions;

/**
 *
 * @author karol harezlak
 */
final class HintsUtils {

    static final String TAB = "    "; //NOI18N
    static final String EXCEPTION_UOE = "java.lang.UnsupportedOperationException"; //NOI18N

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

    /**
     * Add imports for provided document. Based on Imports.addImport from module Visage Source
     *
     * @param document
     * @param im - import string
     */
    public static void addImport(final Document document, final String im) {
        class Import {

            public boolean pkg = false;
            public long start;
            public long end;
            public String value;
        }
        if (im.indexOf('.') < 0) {
            return;
        }

        String doc = null;
        try {
            doc = document.getText(0, document.getEndPosition().getOffset());
        } catch (BadLocationException ex) {
            Exceptions.printStackTrace(ex);
        }
        if (doc == null) {
            return;
        }

        Pattern packagePattern = Pattern.compile("package [a-zA-Z0-9_.]+;"); // NOI18N
        Pattern importPattern = Pattern.compile("import [a-zA-Z0-9_.*]+;"); // NOI18N

        List<Import> imports = new ArrayList<Import>();
        int position = 0;
        try {
            BufferedReader in = new BufferedReader(new StringReader(doc));
            String line = "";
            while (line != null) {
                char c;
                line = null;
                for (;;) {
                    c = (char) in.read();
                    position++;
                    if (c == 65535) {
                        line = null;
                        break;
                    }
                    if (c == '\n' || c == '\r') {
                        line = line == null ? "" : line;
                        break;
                    } // NOI18N
                    if (line == null) {
                        line = "" + c; // NOI18N
                    } else {
                        line += c;
                    }
                }
                if (line == null) {
                    continue;
                }

                if (packagePattern.matcher(line).matches()) {
                    // Package
                    Import i = new Import();
                    i.pkg = true;
                    i.end = position;
                    i.value = line.trim();
                    imports.add(i);
                }
                if (importPattern.matcher(line).matches()) {
                    // Import
                    Import i = new Import();
                    i.end = position;
                    i.value = line.trim();
                    imports.add(i);
                }
            }
            in.close();
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }

        final String pkgImport = im.substring(0, im.lastIndexOf('.')) + ".*"; // NOI18N
        // Check whether the import is in there
        boolean found = false;
        Import lastImport = null;
        for (Import i : imports) {
            if (lastImport == null) {
                lastImport = i;
            } else if (lastImport.end < i.end) {
                lastImport = i;
            }

            if (("import " + im + ";").equals(i.value) || ("import " + pkgImport + ";").equals(i.value)) { // NOI18N
                found = true;
                break;
            }
        }
        if (!found) {
            if (lastImport == null) {
                try {
                    document.insertString(0, "import " + im + ";\r\n", null); // NOI18N
                } catch (BadLocationException e) {
                    throw new IllegalStateException(e);
                }
            } else {
                if (lastImport.pkg) {
                    try {
                        document.insertString((int) lastImport.end, "\r\nimport " + im + ";\r\n", null); // NOI18N
                    } catch (BadLocationException e) {
                        throw new IllegalStateException(e);
                    }
                } else {
                    try {
                        document.insertString((int) lastImport.end, "import " + im + ";\r\n", null); // NOI18N
                    } catch (BadLocationException e) {
                        throw new IllegalStateException(e);
                    }
                }
            }
        }
    }

    static void runInAWT(Runnable runnable) {
        if (SwingUtilities.isEventDispatchThread()) {
            runnable.run();
        } else {
            SwingUtilities.invokeLater(runnable);
        }
    }

    static void runInAWTandWait(Runnable runnable) throws Exception {
        if (SwingUtilities.isEventDispatchThread()) {
            runnable.run();
        } else {
            SwingUtilities.invokeAndWait(runnable);
        }
    }

    static String getPackageName(String fqn) {
        String methodName;
        if (fqn.contains(".")) { //NOI18N
            int end = fqn.lastIndexOf("."); //NOI18N
            methodName = fqn.substring(0, end);
        } else {
            methodName = ""; //NOI!8N
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

    static MethodSymbol isAlreadyDefined(Collection<MethodSymbol> overriddenMethodList, MethodSymbol method, CompilationInfo compilationInfo) {
        if (overriddenMethodList != null && !overriddenMethodList.isEmpty()) {
            for (MethodSymbol overriddenMethod : overriddenMethodList) {
                String overrriddenName = overriddenMethod.getSimpleName().toString();
                if (!method.getSimpleName().toString().equals(overrriddenName)) {
                    continue;
                }
                TypeElement typeOverridden = ElementUtilities.enclosingTypeElement(overriddenMethod);
                try {
                    if (ElementUtilities.alreadyDefinedIn(overrriddenName, method, typeOverridden)) {
                        return overriddenMethod;
                    }
                } catch (NullPointerException ex) {
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

    static boolean isAnnon(Element element) {
        if (!(element instanceof JavafxClassSymbol)) {
            return false;
        }
        JavafxClassSymbol classSymbol = ((JavafxClassSymbol) element);
        if (!classSymbol.isLocal()) {
            return false;
        }
        String name = element.toString();
        int lastIndex = name.lastIndexOf("$"); //NOI18N
        if (lastIndex < 0) {
            return false;
        }
        if (!name.substring(lastIndex).contains("anon")) { //NOI18N
            return false;
        }

        return true;
    }

    static boolean isInGuardedBlock(Document document, int position) {
        VisageDocument fxdocument = null;
        if (document instanceof VisageDocument) {
            fxdocument = (VisageDocument) document;
        }
        if (fxdocument != null && fxdocument.isPosGuarded(position)) {
            return true;
        }

        return false;
    }
}
