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
final class OverrideAnonVisitor extends JavaFXTreePathScanner<Void, Void> {

    private CompilationInfo compilationInfo;
    private Collection<MethodSymbol> overridenMethods;
    private Collection<JFXImport> imports;

    public OverrideAnonVisitor(CompilationInfo compilationInfo,
            Collection<MethodSymbol> overridenMethods,
            Collection<JFXImport> imports) {

        this.compilationInfo = compilationInfo;
        this.overridenMethods = overridenMethods;
        this.imports = imports;
    }

    @Override
    public Void visitClassDeclaration(ClassDeclarationTree node, Void p) {
        return null;
    }

    @Override
    public Void visitInstantiate(InstantiateTree node, Void p) {
        return null;
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
        Element element = compilationInfo.getTrees().getElement(getCurrentPath());
        if (element != null && element.getEnclosingElement() instanceof JavafxClassSymbol) {
            if (element instanceof MethodSymbol) {
                MethodSymbol methodSymbol = (MethodSymbol) element;
                overridenMethods.add(methodSymbol);
            }
        }

        return null;
    }
}
