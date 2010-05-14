/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.editor.hints;

import com.sun.javafx.api.tree.ClassDeclarationTree;
import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.JavaFXTreePathScanner;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.tools.mjavac.code.Symbol.MethodSymbol;
import java.util.ArrayList;
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
    private Map<Element, List<MethodSymbol>> overriddenMethods;
    private Map<MethodSymbol, Integer> positions;
    private SourcePositions sourcePositions;

    OverrideVisitor(CompilationInfo compilationInfo,
            Map<Element, List<MethodSymbol>> overriddenMethods,
            Map<MethodSymbol, Integer> positions) {

        this.compilationInfo = compilationInfo;
        this.overriddenMethods = overriddenMethods;
        this.positions = positions;
        this.sourcePositions = compilationInfo.getTrees().getSourcePositions();
    }

    @Override
    public Void visitClassDeclaration(ClassDeclarationTree node, Void v) {
        Element currentClass = compilationInfo.getTrees().getElement(getCurrentPath());
        if (currentClass != null) {
            overriddenMethods.put(currentClass, new ArrayList<MethodSymbol>());
        }

        return super.visitClassDeclaration(node, v);
    }

    @Override
    @SuppressWarnings("element-type-mismatch")
    public Void visitFunctionDefinition(FunctionDefinitionTree node, Void v) {
//        if (node.toString().contains(" overridefunction ") || node.toString().contains(" override ")) { //NOI18N
        Element element = compilationInfo.getTrees().getElement(getCurrentPath());
        if (element != null) {
            Element currentClass = element.getEnclosingElement();
            if (element instanceof MethodSymbol) {
                List<MethodSymbol> methods = overriddenMethods.get(currentClass);
                if (!methods.contains(element)) {
                    methods.add((MethodSymbol) element);
                    positions.put((MethodSymbol) element, (int) sourcePositions.getStartPosition(compilationInfo.getCompilationUnit(), node));
                }
                overriddenMethods.put(currentClass, methods);
            }
        }
        //}
        return super.visitFunctionDefinition(node, v);
    }
}
