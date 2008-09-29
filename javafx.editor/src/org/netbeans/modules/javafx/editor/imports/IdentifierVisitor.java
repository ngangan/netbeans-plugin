/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.modules.javafx.editor.imports;

import com.sun.javafx.api.tree.*;
import org.netbeans.api.javafx.source.CompilationInfo;

import javax.lang.model.element.Element;
import java.util.Collection;

/**
 * @author Rastislav Komara (<a href="mailto:moonko@netbeans.orgm">RKo</a>)
 * @todo documentation
 */
class IdentifierVisitor extends JavaFXTreeScanner<Collection<Element>, Collection<Element>> {
    private final CompilationInfo info;
    protected UnitTree cu;

    IdentifierVisitor(CompilationInfo info) {
        this.info = info;
        cu = this.info.getCompilationUnit();
    }

    @Override
    public Collection<Element> visitIdentifier(IdentifierTree node, Collection<Element> elements) {
        Element element = toElement(node);
        elements.add(element);
        return elements;
    }

    private Element toElement(Tree node) {
        return info.getTrees().getElement(JavaFXTreePath.getPath(cu, node));
    }
}
