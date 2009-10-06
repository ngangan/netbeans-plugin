/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javac.code.Symbol.MethodSymbol;
import com.sun.tools.javafx.code.JavafxClassSymbol;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import javax.lang.model.element.Element;
import org.netbeans.api.javafx.source.CompilationInfo;

/**
 *
 * @author karol harezlak
 */
final class OverrideVisitor extends JavaFXTreePathScanner<Void, Void> {

    private CompilationInfo compilationInfo;
    private Map<Element, Collection<Tree>> classTrees;
    private Map<Element, List<MethodSymbol>> overridenMethods;
    private Collection<JavafxClassSymbol> imports;

    public OverrideVisitor(CompilationInfo compilationInfo,
                            Map<Element, Collection<Tree>> classTrees,
                            Map<Element, List<MethodSymbol>> overridenMethods,
                            Collection<JavafxClassSymbol> imports) {
        this.compilationInfo = compilationInfo;
        this.classTrees = classTrees;
        this.overridenMethods = overridenMethods;
        this.imports = imports;
    }

    @Override
    public Void visitClassDeclaration(ClassDeclarationTree node, Void v) {
        Element currentClass = compilationInfo.getTrees().getElement(getCurrentPath());
        Collection<Tree> extendsList = classTrees.get(currentClass);
        if (extendsList == null) {
            extendsList = new HashSet<Tree>();
        }
        extendsList.addAll(node.getSupertypeList());
        classTrees.put(currentClass, extendsList);

        return super.visitClassDeclaration(node, v);
    }

    @Override
    public Void visitVariable(VariableTree node, Void p) {
        //addOverriden(node);
        return super.visitVariable(node, p);
    }

    @Override
    public Void visitImport(ImportTree node, Void p) {
        JavaFXTreePath path = compilationInfo.getTrees().getPath(compilationInfo.getCompilationUnit(), node.getQualifiedIdentifier());
        Element element = compilationInfo.getTrees().getElement(path);
        if (element instanceof JavafxClassSymbol) {
            imports.add((JavafxClassSymbol) element);
        }

        return super.visitImport(node, p);
    }

    @Override
    public Void visitFunctionDefinition(FunctionDefinitionTree node, Void v) {
        addOverriden(node);
        return super.visitFunctionDefinition(node, v);
    }

    private void addOverriden(Tree node) {
        if (node.toString().contains(" overridefunction ") || node.toString().contains(" override ")) { //NOI18N
            Element element = compilationInfo.getTrees().getElement(getCurrentPath());
            if (element != null) {
                Element currentClass = element.getEnclosingElement();
                if (element instanceof MethodSymbol) {
                    if (overridenMethods.get(currentClass) == null) {
                        overridenMethods.put(currentClass, new ArrayList<MethodSymbol>());
                    }
                    List<MethodSymbol> methods = overridenMethods.get(currentClass);
                    methods.add((MethodSymbol) element);
                    overridenMethods.put(currentClass, methods);
                }
            }
        }
    }
}
