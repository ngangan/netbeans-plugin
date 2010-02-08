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

    private void setPositions(int pos, CompilationInfo ci) {
        TokenSequence<JFXTokenId> tokens = ci.getTokenHierarchy().tokenSequence();
        tokens.moveStart();
        tokens.move(pos);
        tokens.moveNext();
        Token<JFXTokenId> token = tokens.token();
        pos += token.length();
        
        boolean found = false;
        String simpleText = element.getSimpleName().toString();

        if (token.id() != JFXTokenId.IDENTIFIER || !simpleText.equals(token.text().toString())) {
            if (tokens.movePrevious()) {
                token = tokens.token();
                pos -= token.length();
                if (token.id() == JFXTokenId.IDENTIFIER && simpleText.equals(token.text().toString())) {
                    startPosition = pos;
                    endPosition = pos + token.length();
                    found = true;
                } else {
                    pos += token.length();
                    tokens.moveNext();
                }
            }
            if (!found) {
                while (tokens.moveNext()) {
                    token = tokens.token();
                    pos += token.length();
                    if (token.id() == JFXTokenId.IDENTIFIER && simpleText.equals(token.text().toString())) {
                        startPosition = pos - token.length();
                        endPosition = pos;
                        break;
                    }
                }
            }
        } else {
            startPosition = pos - token.length();
            endPosition = pos;
        }
    }
}
