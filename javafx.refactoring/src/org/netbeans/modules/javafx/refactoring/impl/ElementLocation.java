/*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 *  Copyright 1997-2010 Sun Microsystems, Inc. All rights reserved.
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

package org.netbeans.modules.javafx.refactoring.impl;

import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.Tree.JavaFXKind;
import com.sun.tools.javafx.tree.JFXOverrideClassVar;
import java.lang.ref.WeakReference;
import javax.lang.model.element.Element;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.ElementHandle;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.javafx.refactoring.repository.ElementDef;
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

    private ElementDef elDef;

    public ElementDef getElementDef() {
        return elDef;
    }

    public ElementLocation(ElementDef elDef, FileObject fo) {
        this.elDef = elDef;
        this.sourceFile = fo;
        this.startPosition = elDef.getStartPos();
        this.endPosition = elDef.getEndPos();
    }

    public Element getElement(CompilationInfo info) {
        CompilationInfo oldCi = ciRef != null ? ciRef.get() : null;
        if (oldCi != null && oldCi == info) return element;
        else {
            if (info.getFileObject().equals(sourceFile)) {
                ciRef = new WeakReference<CompilationInfo>(info);
                if (startPosition > 0) {
                    element = info.getElementUtilities().elementFor(startPosition);
                } else {
                    element = handle != null ? handle.resolve(info) : null;
                }
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
        return element.toString() + " @ " + startPosition + " in " + sourceFile.getPath(); // NOI18N
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
