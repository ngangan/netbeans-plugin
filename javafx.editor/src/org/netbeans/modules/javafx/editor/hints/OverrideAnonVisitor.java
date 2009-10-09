/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.BinaryTree;
import com.sun.javafx.api.tree.BlockExpressionTree;
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.InstantiateTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
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
final class OverrideAnonVisitor extends JavaFXTreePathScanner<Void, Void> {

    private CompilationInfo compilationInfo;
    private Map<Element, Collection<Tree>> classTrees;
    private Map<Element, List<MethodSymbol>> overridenMethods;
    private Collection<JavafxClassSymbol> imports;
    Map<Element, Tree> position;

    public OverrideAnonVisitor(CompilationInfo compilationInfo,
            Map<Element, Collection<Tree>> classTrees,
            Map<Element, List<MethodSymbol>> overridenMethods,
            Collection<JavafxClassSymbol> imports,
            Map<Element, Tree> position) {

        this.compilationInfo = compilationInfo;
        this.classTrees = classTrees;
        this.overridenMethods = overridenMethods;
        this.imports = imports;
        this.position = position;
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
    @SuppressWarnings("cast") //NOI18N
    public Void visitInstantiate(InstantiateTree node, Void p) {
        JavaFXTreePath path = compilationInfo.getTrees().getPath(compilationInfo.getCompilationUnit(), node.getIdentifier());
        
        if (compilationInfo.getTrees().getElement(path) instanceof JavafxClassSymbol) {
            JavafxClassSymbol currentClass =  (JavafxClassSymbol) compilationInfo.getTrees().getElement(path);
            Collection<Tree> extendsList = classTrees.get(currentClass);
            if (extendsList == null) {
                extendsList = new HashSet<Tree>();
            }
            extendsList.add(node.getIdentifier());
            classTrees.put((JavafxClassSymbol) currentClass, extendsList);
            position.put(currentClass, node);
        }
        return super.visitInstantiate(node, p);
    }

    @Override
    public Void visitBinary(BinaryTree node, Void p) {
        return super.visitBinary(node, p);
    }

    @Override
    public Void visitBlockExpression(BlockExpressionTree node, Void p) {
        return super.visitBlockExpression(node, p);
    }

    @Override
    public Void visitFunctionDefinition(FunctionDefinitionTree node, Void v) {
        if (node.toString().contains(" overridefunction ") || node.toString().contains(" override ")) { //NOI18N
            Element element = compilationInfo.getTrees().getElement(getCurrentPath());
            if (element != null && element.getEnclosingElement() instanceof JavafxClassSymbol) {
                JavafxClassSymbol currentClass = (JavafxClassSymbol) element.getEnclosingElement();
                Tree currentClassTree = compilationInfo.getTrees().getTree(currentClass);
                SourcePositions sourcePositions = compilationInfo.getTrees().getSourcePositions();
                //Hack to recognize anon class for method
                int currentClassPosition  = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), currentClassTree);
                for (Element classElement : classTrees.keySet()) {
                    if (!(classElement instanceof JavafxClassSymbol)) {
                        continue;
                    }
                    Tree classElementTree = position.get(classElement);
                    int  classPosition  = (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), classElementTree);
                    if (currentClassPosition == classPosition) {
                        currentClass = (JavafxClassSymbol) classElement;
                        break;
                    }
                }
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

        return super.visitFunctionDefinition(node, v);
    }
}
