/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.InstantiateTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.tools.javac.code.Symbol.MethodSymbol;
import com.sun.tools.javafx.code.JavafxClassSymbol;
import com.sun.tools.javafx.tree.JFXImport;
import java.util.Collection;
import javax.lang.model.element.Element;
import org.netbeans.api.javafx.source.CompilationInfo;

/**
 *
 * @author karol harezlak
 */
final class OverrideAllVisitor extends JavaFXTreePathScanner<Void, Void> {

    private CompilationInfo compilationInfo;
    private Collection<MethodSymbol> overriddenMethods;
    private Collection<JFXImport> imports;

    public OverrideAllVisitor(CompilationInfo compilationInfo,
            Collection<MethodSymbol> overriddenMethods,
            Collection<JFXImport> imports) {

        this.compilationInfo = compilationInfo;
        this.overriddenMethods = overriddenMethods;
        this.imports = imports;
    }

    @Override
    public Void visitImport(ImportTree node, Void p) {
        if (node instanceof JFXImport) {
            imports.add((JFXImport) node);
        }

        return null;
    }

    @Override
    public Void visitFunctionDefinition(FunctionDefinitionTree node, Void v) {
        if (node.toString().contains("override")) { //NOI18N
            Element element = compilationInfo.getTrees().getElement(getCurrentPath());
            if (element != null && element.getEnclosingElement() instanceof JavafxClassSymbol) {
                if (element instanceof MethodSymbol) {
                    MethodSymbol methodSymbol = (MethodSymbol) element;
                    overriddenMethods.add(methodSymbol);
                }
            }
        }
        return null;
    }
}
