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
 *  accompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 * 
 *  Contributor(s):
 * 
 *  Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.refactoring.impl.javafxc;

import com.sun.javafx.api.tree.FunctionDefinitionTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.VariableTree;
import com.sun.tools.javafx.tree.JFXClassDeclaration;
import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import javax.lang.model.element.Element;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

/**
 *
 * @author Jaroslav Bachorik
 */
final public class TreePathHandle {
    private static class KindPath {
        private ArrayList<Tree.JavaFXKind> kindPath = new ArrayList();

        KindPath(JavaFXTreePath treePath) {
            while (treePath != null) {
                kindPath.add(treePath.getLeaf().getJavaFXKind());
                treePath = treePath.getParentPath();
            }
        }

        public int hashCode() {
            return kindPath.hashCode();
        }

        public boolean equals(Object object) {
            if (object instanceof KindPath) {
                return kindPath.equals(((KindPath) object).kindPath);
            }
            return false;
        }

        public ArrayList<Tree.JavaFXKind> getList() {
            return kindPath;
        }
    }

    private KindPath kindPath;
    private long position = -1;
    private String displayName;
    private FileObject fileObject;
    private Tree.JavaFXKind kind;

    private TreePathHandle(long pos, JavaFXTreePath path, CompilationInfo cc) {
        kindPath = new KindPath(path);
        kind = path.getLeaf().getJavaFXKind();
        
        position = pos;

        displayName = cc.getTrees().getElement(path).getSimpleName().toString();

        URI srcUri = cc.getCompilationUnit().getSourceFile().toUri();
        fileObject = FileUtil.toFileObject(new File(srcUri));
    }

    static public TreePathHandle create(JavaFXTreePath path, CompilationInfo cc) {
        long[] pos = findPos(path.getLeaf(), cc);
        return create(pos[1], path, cc);
    }

    static public TreePathHandle create(long srcPos, JavaFXTreePath path, CompilationInfo cc) {
        return new TreePathHandle(srcPos, path, cc);
    }

    public static TreePathHandle create(long srcPos, Element element, CompilationInfo cc) {
        return new TreePathHandle(srcPos, cc.getTrees().getPath(element), cc);
    }

    public JavaFXTreePath resolve(CompilationInfo cc) {
        JavaFXTreePath result = cc.getTreeUtilities().pathFor(position);
        JavaFXTreePath intermediary = result;
        while (intermediary != null && intermediary.getLeaf().getJavaFXKind() != kind) {
            intermediary = intermediary.getParentPath();
        }
        if (intermediary != null) {
            result = intermediary;
        }
        if (!kindPath.equals(new KindPath(result))) {
            assert false;
        }

        return result;
    }

    public Element resolveElement(CompilationInfo cc) {
        JavaFXTreePath path = resolve(cc);
        return cc.getTrees().getElement(path);
    }

    public long getSrcPos() {
        return position;
    }

    public String getSimpleName() {
        return displayName;
    }

    public FileObject getFileObject() {
        return fileObject;
    }

    public Tree.JavaFXKind getKind() {
        return kind;
    }

    static private long[] findPos(Tree t, CompilationInfo cc) {
        long[] result = null;
        switch(t.getJavaFXKind()) {
            case VARIABLE: {
                result = cc.getTreeUtilities().findNameSpan((VariableTree)t);
                break;
            }
            case FUNCTION_DEFINITION: {
                result = cc.getTreeUtilities().findNameSpan((FunctionDefinitionTree)t);
                break;
            }
            case CLASS_DECLARATION: {
                result = cc.getTreeUtilities().findNameSpan((JFXClassDeclaration)t);
                break;
            }
        }
        if (result == null) {
            SourcePositions sp = cc.getTrees().getSourcePositions();
            result = new long[] {sp.getStartPosition(cc.getCompilationUnit(), t), sp.getEndPosition(cc.getCompilationUnit(), t)};
        }
        return result;
    }

    @Override
    public String toString() {
        return displayName + "[" + kind + "]@" + position;
    }
}
