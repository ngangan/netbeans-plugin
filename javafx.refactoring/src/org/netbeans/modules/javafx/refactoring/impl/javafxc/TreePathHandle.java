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
import com.sun.tools.javafx.tree.JFXFunctionDefinition;
import com.sun.tools.javafx.tree.JFXInstanciate;
import com.sun.tools.javafx.tree.JFXTypeClass;
import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import javax.lang.model.element.Name;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

/**
 *
 * @author Jaroslav Bachorik
 */
final public class TreePathHandle {
    final private static Logger LOG = Logger.getLogger(TreePathHandle.class.getName());
    final private static boolean DEBUG = LOG.isLoggable(Level.FINE);

    private static class KindPath {
        private ArrayList<Tree.JavaFXKind> kindPath = new ArrayList();

        KindPath(JavaFXTreePath treePath) {
            while (treePath != null) {
                kindPath.add(treePath.getLeaf().getJavaFXKind());
                treePath = treePath.getParentPath();
            }
        }

        @Override
        public int hashCode() {
            return kindPath.hashCode();
        }

        @Override
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

    private TreePathHandle(long pos, JavaFXTreePath path, CompilationInfo cc) throws InstantiationException {
        position = pos;
        path = findSupportedPath(path, cc);
        if (path == null) throw new InstantiationException();
        
        kindPath = new KindPath(path);
        kind = path.getLeaf().getJavaFXKind();

        if (path.getLeaf().getJavaFXKind() == Tree.JavaFXKind.INSTANTIATE_NEW) {
            displayName = ((JFXInstanciate)path.getLeaf()).getIdentifierSym().name.toString();
        } else {
            Name n = cc.getTrees().getElement(path).getSimpleName();
            if (n == null) throw new InstantiationException();
            displayName = n.toString();
        }

        URI srcUri = cc.getCompilationUnit().getSourceFile().toUri();
        fileObject = FileUtil.toFileObject(new File(srcUri));
    }

    static public TreePathHandle create(JavaFXTreePath path, CompilationInfo cc) {
        long[] pos = findPos(path.getLeaf(), cc);
        if (path.getLeaf().getJavaFXKind() == Tree.JavaFXKind.OBJECT_LITERAL_PART) {
            return create(pos[0], path, cc);
        }
        return create(pos[1], path, cc);
    }

    static public TreePathHandle create(long srcPos, JavaFXTreePath path, CompilationInfo cc) {
        try {
            return new TreePathHandle(srcPos, path, cc);
        } catch (InstantiationException e) {
            return null;
        }
    }

    public static TreePathHandle create(long srcPos, Element element, CompilationInfo cc) {
        try {
            return new TreePathHandle(srcPos, cc.getTrees().getPath(element), cc);
        } catch (InstantiationException e) {
            return null;
        }
    }

    public JavaFXTreePath resolve(CompilationInfo cc) {
        JavaFXTreePath result = findSupportedPath(cc.getTreeUtilities().pathFor(position), cc);

        KindPath newKindPath = new KindPath(result);
        if (!kindPath.equals(new KindPath(result))) {
            if (DEBUG) {
                StringBuilder sb = new StringBuilder();
                sb.append("Error resolving tree path at position ").append(position).append(" (src=").append(fileObject.getPath()).append(")\n"); // NOI18N
                sb.append("Expected kind path: "); // NOI18N
                dumpKindPath(kindPath, sb);
                sb.append("\n"); // NOI18N
                sb.append("Obtained kind path: "); // NOI18N
                dumpKindPath(newKindPath, sb);
                sb.append("\n"); // NOI18N
            }
            return null;
        }

        return result;
    }

    public Element resolveElement(CompilationInfo cc) {
        JavaFXTreePath path = resolve(cc);
        if (path == null) return null;
        
        if ((path.getLeaf().getJavaFXKind() == Tree.JavaFXKind.IDENTIFIER || path.getLeaf().getJavaFXKind() == Tree.JavaFXKind.MEMBER_SELECT) && path.getParentPath().getLeaf().getJavaFXKind() == Tree.JavaFXKind.COMPILATION_UNIT) { // package identifier
           Element e = cc.getTrees().getElement(path.getParentPath());
           return e;
        }
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
        return displayName + "[" + kind + "]@" + position; // NOI18N
    }
    
    private void dumpKindPath(KindPath kindPath, StringBuilder sb) {
        boolean firstEntry = true;
        for (Tree.JavaFXKind aKind : kindPath.getList()) {
            if (!firstEntry) {
                sb.append("->"); // NOI18N
            }
            sb.append(aKind.toString());
        }
    }

    private boolean isSupported(JavaFXTreePath path) {

        switch (path.getLeaf().getJavaFXKind()) {
            case COMPILATION_UNIT:
            case CLASS_DECLARATION:
            case TYPE_CLASS:
            case METHOD_INVOCATION:
            case FUNCTION_DEFINITION:
            case INIT_DEFINITION:
            case INSTANTIATE_NEW:
            case VARIABLE: {
                return true;
            }
            default: {
                return false;
            }
        }
    }

    private JavaFXTreePath findSupportedPath(JavaFXTreePath initPath, CompilationInfo cc) {
        while (initPath != null) {
            Element e = cc.getTrees().getElement(initPath);
            if (e != null && e.getSimpleName() != null) break;
            initPath = initPath.getParentPath();
        }
        if (initPath.getLeaf().getJavaFXKind() == Tree.JavaFXKind.FUNCTION_DEFINITION) {
            // this stinks; synthetic method resolves to an element, nevertheless
            if (((JFXFunctionDefinition)initPath.getLeaf()).getName().contentEquals("javafx$run$")) {  // NOI18N
                initPath = initPath.getParentPath();
            }
        }
        return initPath;
    }
}
