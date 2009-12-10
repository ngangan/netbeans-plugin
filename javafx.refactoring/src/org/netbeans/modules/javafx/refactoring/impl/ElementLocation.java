/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.refactoring.impl;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree.JavaFXKind;
import com.sun.tools.javafx.tree.JFXOverrideClassVar;
import java.lang.ref.WeakReference;
import java.util.EnumSet;
import javax.lang.model.element.Element;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.openide.filesystems.FileObject;

/**
 *
 * @author Jaroslav Bachorik <jaroslav.bachorik@sun.com>
 */
final public class ElementLocation {
    private Element element;
    private FileObject sourceFile;
    private int startPosition, endPosition;
    private WeakReference<CompilationInfo> ciRef = null;
    private ElementHandle handle;
    private String simpleName;

//    private final KindPath kindPath;

    public static ElementLocation forPath(JavaFXTreePath tp, CompilationInfo ci) {
        long pos = ci.getTrees().getSourcePositions().getStartPosition(ci.getCompilationUnit(), tp.getLeaf());
        Element e = ci.getTrees().getElement(tp);
        if (e == null) {
            if ((tp.getLeaf().getJavaFXKind() == JavaFXKind.MEMBER_SELECT || tp.getLeaf().getJavaFXKind() == JavaFXKind.IDENTIFIER) && tp.getParentPath().getLeaf().getJavaFXKind() == JavaFXKind.COMPILATION_UNIT) {
                e = ci.getElementUtilities().getPackageElement(tp.getLeaf().toString());
            } else if (tp.getLeaf().getJavaFXKind() == JavaFXKind.VARIABLE) {
                e = ((JFXOverrideClassVar)tp.getLeaf()).sym;
            }
        }
        return new ElementLocation(e, (int)pos, ci);
    }

    public ElementLocation(Element element, int position, CompilationInfo ci) {
        this.element = element;
        this.sourceFile = ci.getFileObject();
        this.ciRef = new WeakReference<CompilationInfo>(ci);
        this.handle = ElementHandle.create(element);
        this.simpleName = element.getSimpleName().toString();
        setPositions(position, ci);
    }

    public Element getElement(CompilationInfo info) {
        CompilationInfo oldCi = ciRef != null ? ciRef.get() : null;
        if (oldCi != null && oldCi == info) return element;
        else {
            if (info.getFileObject().equals(sourceFile)) {
                ciRef = new WeakReference<CompilationInfo>(info);
                element = info.getElementUtilities().elementFor(startPosition);
                return element;
            } else {
                return handle != null ? handle.resolve(info) : null;
            }
        }
    }

    public Element getElement() {
        return element;
    }

    public int getStartPosition() {
        return startPosition;
    }

    public int getEndPosition() {
        return endPosition;
    }

    public FileObject getSourceFile() {
        return sourceFile;
    }

    public String getSimpleName() {
        return simpleName;
    }

    /**
     * Returns the currently assigned {@linkplain CompilationInfo} instance
     * @return May return NULL
     *
     */
    public CompilationInfo getCompilationInfo() {
        return ciRef.get();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final ElementLocation other = (ElementLocation) obj;
        if (this.element != other.element && (this.element == null || !this.element.equals(other.element))) {
            return false;
        }
        if (this.sourceFile != other.sourceFile && (this.sourceFile == null || !this.sourceFile.equals(other.sourceFile))) {
            return false;
        }
        if (this.startPosition != other.startPosition) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 5;
        hash = 17 * hash + (this.element != null ? this.element.hashCode() : 0);
        hash = 17 * hash + (this.sourceFile != null ? this.sourceFile.hashCode() : 0);
        hash = 17 * hash + this.startPosition;
        return hash;
    }

    @Override
    public String toString() {
        return element.toString() + " @ " + startPosition + " in " + sourceFile.getPath();
    }

    final private static EnumSet<JFXTokenId> closingTokens = EnumSet.of(JFXTokenId.WS, JFXTokenId.LPAREN, JFXTokenId.DOT, JFXTokenId.COMMA, JFXTokenId.SEMI, JFXTokenId.COLON, JFXTokenId.LBRACE, JFXTokenId.LBRACKET);
    private void setPositions(int pos, CompilationInfo ci) {
        String simpleText = element.getSimpleName().toString();
        String elementText = element.toString();
        int textDiff = elementText.length() - simpleText.length();

        TokenSequence<JFXTokenId> tokens = ci.getTokenHierarchy().tokenSequence();
        tokens.moveStart();
        tokens.move(pos);
        boolean start = true;
        while (tokens.moveNext()) {
            Token<JFXTokenId> token = tokens.token();
            if (start) {
                if (closingTokens.contains(token.id())){
                    while (closingTokens.contains(token.id())) {
                        tokens.movePrevious();
                        token = tokens.token();
                    }
                    if (token.id() == JFXTokenId.IDENTIFIER) {
                        startPosition = token.offset(ci.getTokenHierarchy());
                        endPosition = startPosition + token.length();
                        break;
                    }
                }
                start = false;
            }
            if (token != null && token.id() == JFXTokenId.IDENTIFIER) {
                if (token.text().toString().equals(simpleText)) {
                    startPosition = token.offset(ci.getTokenHierarchy());
                    endPosition = startPosition + token.length();
                    break;
                }
            }
        }
    }

//    private JavaFXTreePath pathFor(final int pos, final CompilationInfo ci) {
//        final JavaFXTreePath[] path = new JavaFXTreePath[1];
//        final SourcePositions positions = ci.getTrees().getSourcePositions();
//
//        JavaFXTreePathScanner<Void, Void> scanner = new JavaFXTreePathScanner<Void, Void>() {
//            private long lastValidSpan = Long.MAX_VALUE;
//            @Override
//            public Void scan(Tree tree, Void p) {
//                JavaFXTreePath oldPath = getCurrentPath();
//                super.scan(tree, p);
//                JavaFXTreePath newPath = (tree != null && oldPath != null) ? JavafxcTrees.getPath(oldPath, tree) : null;
//                if (tree != null) {
//                    long start = positions.getStartPosition(ci.getCompilationUnit(), tree);
//                    long end = positions.getEndPosition(ci.getCompilationUnit(), tree);
//
//                    if (tree.getJavaFXKind() != Tree.JavaFXKind.STRING_LITERAL || !(tree.toString().equals("\"\"") || tree.toString().equals(""))) {
//                        if (tree.getJavaFXKind() != Tree.JavaFXKind.MODIFIERS && start != -1 && start != end && start <= pos && end >=pos) {
//                            // check for javafx$run$ magic
//                            if (!(tree.getJavaFXKind() == Tree.JavaFXKind.FUNCTION_DEFINITION && ((JFXFunctionDefinition)tree).getName().contentEquals("javafx$run$"))) {
//                                long span = end - start + 1;
//                                if (span < lastValidSpan) {
//                                    if (kindPath.equals(new KindPath(newPath))) {
//                                        path[0] = newPath;
//                                        lastValidSpan = span;
//                                    }
//                                }
//                            }
//                        }
//                    }
//                }
//                return null;
//            }
//        };
//        scanner.scan(ci.getCompilationUnit(), null);
//        return path[0];
//    }
//
//    private static class KindPath {
//        private ArrayList<Tree.JavaFXKind> kindPath = new ArrayList();
//
//        KindPath(JavaFXTreePath treePath) {
//            while (treePath != null) {
//                kindPath.add(treePath.getLeaf().getJavaFXKind());
//                treePath = treePath.getParentPath();
//            }
//        }
//
//        @Override
//        public int hashCode() {
//            return kindPath.hashCode();
//        }
//
//        @Override
//        public boolean equals(Object object) {
//            if (object instanceof KindPath) {
//                return kindPath.equals(((KindPath) object).kindPath);
//            }
//            return false;
//        }
//
//        public ArrayList<Tree.JavaFXKind> getList() {
//            return kindPath;
//        }
//    }
}
