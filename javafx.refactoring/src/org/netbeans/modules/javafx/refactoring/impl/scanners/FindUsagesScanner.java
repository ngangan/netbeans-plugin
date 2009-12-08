/*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 *  Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 * 
 *  The contents of this file are subject to the terms of either the GNU
 *  General Public License Version 2 only ("GPL") or the Common
 *  Development and Distribution License("CDDL") (collectively, the
 *  "License"). You may not use this file except in compliance with the
 *  License. You can obtain a copy of the License at
 *  http://www.netbeans.org/cddl-gplv2.html
 *  or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 *  specific language governing permissions and limitations under the
 *  License.  When distributing the software, include this License Header
 *  Notice in each file and include the License file at
 *  nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Sun in the GPL Version 2 section of the License file that
 *  agetCompilationController()ompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 * 
 *  Contributor(s):
 * 
 *  Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.refactoring.impl.scanners;

import com.sun.javafx.api.tree.ImportTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.Tree.JavaFXKind;
import com.sun.tools.mjavac.code.Symbol;
import java.util.EnumSet;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import org.netbeans.api.javafx.source.ClassIndex.SearchKind;
import org.netbeans.api.javafx.source.ClassIndex.SearchScope;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.modules.javafx.refactoring.impl.ElementLocation;
import org.openide.filesystems.FileObject;

/**
 *
 * @author Jaroslav Bachorik
 */
public class FindUsagesScanner extends BaseRefactoringScanner {
    private FileObject definingFO = null;
    private boolean isImport = false;

    public FindUsagesScanner(ElementLocation location, CompilationController cc) {
        super(location, cc);
        Element owner = ((Symbol)getElement()).owner;
        if (owner != null && owner.getKind() == ElementKind.CLASS) {
            Set<FileObject> fo = cc.getClasspathInfo().getClassIndex().getResources(ElementHandle.create(owner), EnumSet.of(SearchKind.TYPE_DEFS), EnumSet.allOf(SearchScope.class));
            if (fo.size() > 0) {
                definingFO = fo.iterator().next();
            }
        }
    }

    @Override
    protected boolean isChecked(JavaFXTreePath path, Element element) {
        if (isImport(path)) return false;
        
        ElementKind kind = getElementKind();
        JavaFXKind tKind = path.getLeaf().getJavaFXKind();
        if (kind == ElementKind.CLASS || kind == ElementKind.INTERFACE) {
            return tKind != JavaFXKind.CLASS_DECLARATION;
        } else if (kind == ElementKind.METHOD) {
            return tKind == JavaFXKind.METHOD_INVOCATION;
        } else {
            if (getCC().getFileObject().equals(definingFO)) {
                return !path.getLeaf().equals(getCC().getTrees().getPath(element).getLeaf());
            }
        }
        return true;
    }

    @Override
    public Void visitImport(ImportTree node, Set<ElementLocation> p) {
        try {
            isImport = true;
            return super.visitImport(node, p);
        } finally {
            isImport = false;
        }
    }

    private boolean isImport(JavaFXTreePath path) {
        return isImport;
//        Tree node = path.getLeaf();
//        while (node != null) {
//            path = path.getParentPath();
//            if (path == null) break;
//            node = path.getLeaf();
//            if (node != null && node.getJavaFXKind() == JavaFXKind.IMPORT) return true;
//        }
//        return false;
    }

//    public FindUsagesScanner(ElementLocation location, CompilationController cc) {
//        this(location, ElementHandle.create(location.getElement(cc)), cc);
//    }
//
//    public FindUsagesScanner(ElementLocation location, CompilationController cc) {
//        super(location, cc);
//        this.targetSimpleName = location.getElement(cc).getSimpleName().toString();
//        this.targetQualName = (elementHandle.getKind() == ElementKind.CLASS || elementHandle.getKind() == ElementKind.INTERFACE || elementHandle.getKind() == ElementKind.OTHER) ? elementHandle.getQualifiedName() : "";
//    }
//
//    @Override
//    public Void visitMethodInvocation(FunctionInvocationTree node, Set<ElementLocation> handles) {
//        if (getElementKind() != ElementKind.METHOD) return super.visitMethodInvocation(node, handles);
//
//        if (isSameElement()) {
//            handles.add(ElementLocation.locationFor(getCurrentPath(), getCC()));
//        }
//
//        return super.visitMethodInvocation(node, handles);
//    }
//
//
//    @Override
//    public Void visitMemberSelect(MemberSelectTree node, Set<ElementLocation> handles) {
//        if (!node.getIdentifier().contentEquals(targetSimpleName)) return super.visitMemberSelect(node, handles);
//        if (getElementKind() == ElementKind.CLASS || getElementKind() == ElementKind.INTERFACE) {
//            Element e = getCC().getTrees().getElement(getCurrentPath());
//            if (e.getKind() == getElementKind()) {
//                if (((TypeElement)e).getQualifiedName().contentEquals(targetQualName)) {
//                    handles.add(ElementLocation.locationFor(getCurrentPath(), getCC()));
//                }
//            }
//        } else {
//            ExpressionTree expression = node.getExpression();
//            if (expression instanceof JFXIdent) {
//                Type type = ((JFXIdent)expression).type;
//                if (type == null) return super.visitMemberSelect(node, handles);
//                TypeSymbol ts = type.asElement();
//                if (ts.getKind() != ElementKind.CLASS) return super.visitMemberSelect(node, handles);
//                for(Symbol sy : ts.getEnclosedElements()) {
//                    if (sy.getKind() == ElementKind.FIELD) {
//                        if (getElementHandle() != null && getElementHandle().equals(ElementHandle.create(sy))) {
//                            handles.add(ElementLocation.locationFor(getCurrentPath(), getCC()));
//                            return null;
//                        }
//                    }
//                }
//            }
//        }
//        return super.visitMemberSelect(node, handles);
//    }
//
//    @Override
//    public Void visitIdentifier(IdentifierTree node, Set<ElementLocation> handles) {
//        switch (getElementKind()) {
//            case FIELD:
//            case PARAMETER:
//            case LOCAL_VARIABLE:
//            case CLASS:
//            case INTERFACE: {
//                if (isSameElement()) {
//                    handles.add(ElementLocation.locationFor(getCurrentPath(), getCC()));
//                }
//                break;
//            }
//        }
//        return super.visitIdentifier(node, handles);
//    }
//
//    @Override
//    public Void visitObjectLiteralPart(ObjectLiteralPartTree node, Set<ElementLocation> handles) {
//        switch (getElementKind()) {
//            case FIELD:
//            case PARAMETER:
//            case LOCAL_VARIABLE: {
//                if (isSameElement()) {
//                    handles.add(ElementLocation.locationFor(getCurrentPath(), getCC()));
//                }
//            }
//        }
//        return super.visitObjectLiteralPart(node, handles);
//    }
//
//    @Override
//    public Void visitInstantiate(InstantiateTree node, Set<ElementLocation> handles) {
//        switch (getElementKind()) {
//            case CLASS: {
//                JavaFXTreePath path = JavafxcTrees.getPath(getCurrentPath(), node.getIdentifier());
//                if (isSameElement(path)) {
//                    handles.add(ElementLocation.locationFor(path, getCC()));
//                }
//            }
//        }
//        return super.visitInstantiate(node, handles);
//    }
//

}
