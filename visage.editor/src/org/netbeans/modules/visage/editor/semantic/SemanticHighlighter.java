/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 * 
 * Contributor(s):
 * 
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */
package org.netbeans.modules.visage.editor.semantic;

import com.sun.visage.api.tree.*;

import org.netbeans.api.editor.settings.AttributesUtilities;
import org.netbeans.api.visage.editor.FXSourceUtils;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.api.visage.lexer.VSGTokenId;
import org.netbeans.api.visage.source.CancellableTask;
import org.netbeans.api.visage.source.CompilationInfo;
import org.netbeans.api.visage.source.TreeUtilities;
import org.netbeans.api.visage.source.support.CancellableTreePathScanner;
import org.netbeans.api.lexer.Token;
import org.netbeans.spi.editor.highlighting.support.OffsetsBag;
import org.openide.filesystems.FileObject;

import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.Name;
import javax.swing.text.AttributeSet;
import javax.swing.text.Document;
import javax.swing.text.StyleConstants;
import java.awt.*;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Anton Chechel
 */
public class SemanticHighlighter implements CancellableTask<CompilationInfo> {
    
    private static final Logger LOGGER = Logger.getLogger(SemanticHighlighter.class.getName());
    private static final boolean LOGGABLE = LOGGER.isLoggable(Level.FINE);

    private final FileObject file;
    private final AtomicBoolean cancel = new AtomicBoolean();
    private final List<Result> identifiers = new ArrayList<Result>();

    SemanticHighlighter(FileObject file) {
        this.file = file;
    }

    public void cancel() {
        cancel.set(true);
    }

    public void run(CompilationInfo info) {
        cancel.set(false);
        process(info);
    }

    private boolean process(CompilationInfo info) {
        Document doc = FXSourceUtils.getDocument(file);
        if (doc == null) {
            return false;
        }
        
        identifiers.clear(); // clear cache
        List<Result> result = new ArrayList<Result>();
        UnitTree compilationUnit = info.getCompilationUnit();
        VisageThreeVisitor javaFXThreeVisitor = new VisageThreeVisitor(info, doc, cancel);
        javaFXThreeVisitor.scan(compilationUnit, result);
        if (cancel.get()) {
            return true;
        }

        setHighlights(doc, result, identifiers);
        return false;
    }

    static void setHighlights(Document doc, List<Result> results, List<Result> identifiers) {
        if (results.isEmpty()) {
            return;
        }

        OffsetsBag bag = new OffsetsBag(doc, true);
        for (Result result : results) {
            int start = (int) result.start;
            int end = (int) result.end;

            if (start >= 0 && end >= 0) {
                bag.addHighlight(start, end, getAttributeSet(result));
            } else {
                if (LOGGABLE) {
                    log("* Incorrect positions for highlighting: " + start + ", " + end); // NOI18N
                }
            }

            // highlighting for variables from cache
            if (result.type == ResultTypes.FIELD) {
                for (Result id : identifiers) {
                    final String idText = id.token.text() == null ? "" : id.token.text().toString(); // NOI18N
                    final String resText = result.token.text() == null ? "" : result.token.text().toString(); // NOI18N
                    if (idText.equals(resText)) {
                        bag.addHighlight((int) id.start, (int) id.end, getAttributeSet(result));
                    }
                }
            }
        }

        getBag(doc).setHighlights(bag);
    }

    // TODO move coloring attributes to xml
    private static AttributeSet getAttributeSet(Result result) {
        switch (result.type) {
            case METHOD:
                return AttributesUtilities.createImmutable(StyleConstants.Foreground, Color.BLACK, StyleConstants.Bold, Boolean.TRUE,
                        StyleConstants.Italic, result.attributes.contains(ColoringAttributes.STATIC) ? Boolean.TRUE : Boolean.FALSE,
                        StyleConstants.StrikeThrough, result.attributes.contains(ColoringAttributes.DEPRECATED) ? Boolean.TRUE : Boolean.FALSE);
            case METHOD_INVOCATION:
                return AttributesUtilities.createImmutable(StyleConstants.Foreground, Color.BLACK,
                        StyleConstants.Italic, result.attributes.contains(ColoringAttributes.STATIC) ? Boolean.TRUE : Boolean.FALSE,
                        StyleConstants.StrikeThrough, result.attributes.contains(ColoringAttributes.DEPRECATED) ? Boolean.TRUE : Boolean.FALSE);
            case FIELD:
                return AttributesUtilities.createImmutable(StyleConstants.Foreground, new Color(0, 153, 0),
                        StyleConstants.Italic, result.attributes.contains(ColoringAttributes.STATIC) ? Boolean.TRUE : Boolean.FALSE,
                        StyleConstants.StrikeThrough, result.attributes.contains(ColoringAttributes.DEPRECATED) ? Boolean.TRUE : Boolean.FALSE);
            case VARIABLE:
                return AttributesUtilities.createImmutable(StyleConstants.Background, new Color(255, 127, 127),
                        StyleConstants.Italic, result.attributes.contains(ColoringAttributes.STATIC) ? Boolean.TRUE : Boolean.FALSE,
                        StyleConstants.StrikeThrough, result.attributes.contains(ColoringAttributes.DEPRECATED) ? Boolean.TRUE : Boolean.FALSE);
            case CLASS:
                return AttributesUtilities.createImmutable(StyleConstants.Foreground, Color.BLACK, StyleConstants.Bold, Boolean.TRUE,
                        StyleConstants.Italic, result.attributes.contains(ColoringAttributes.STATIC) ? Boolean.TRUE : Boolean.FALSE,
                        StyleConstants.StrikeThrough, result.attributes.contains(ColoringAttributes.DEPRECATED) ? Boolean.TRUE : Boolean.FALSE);
            default:
                return AttributesUtilities.createImmutable(StyleConstants.Foreground, new Color(0, 153, 0));
        }
    }

    static OffsetsBag getBag(Document doc) {
        OffsetsBag bag = (OffsetsBag) doc.getProperty(SemanticHighlighter.class);

        if (bag == null) {
            doc.putProperty(SemanticHighlighter.class, bag = new OffsetsBag(doc));
        }

        return bag;
    }

    private static void log(String s) {
        if (LOGGABLE) {
            LOGGER.fine(s);
        }
    }

    private class VisageThreeVisitor extends CancellableTreePathScanner<Void, List<Result>> {

        private CompilationInfo info;
        private Document doc;
        private TreeUtilities tu;

        public VisageThreeVisitor(final CompilationInfo info, final Document doc, final AtomicBoolean cancel) {
            super(cancel);
            this.info = info;
            this.doc = doc;
            tu = TreeUtilities.create(info);
        }

        @Override
        public Void visitFunctionDefinition(FunctionDefinitionTree tree, List<Result> list) {
//            String name = ((VSGFunctionDefinition) tree).getName().toString();

            SourcePositions sourcePositions = info.getTrees().getSourcePositions();
            long start = sourcePositions.getStartPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());
            long end = sourcePositions.getEndPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());

            if (start < 0 || end < 0) { // synthetic
                return super.visitFunctionDefinition(tree, list);
            }

            Element element = info.getTrees().getElement(getCurrentPath());
            Set<Modifier> modifiers = FXSourceUtils.getModifiers(element);

            TokenSequence<VSGTokenId> ts = tu.tokensFor(tree);
            while (ts.moveNext()) {
                Token t = ts.token();
                if (VSGTokenId.IDENTIFIER.equals(t.id())) { // first identifier is a name
                    start = ts.offset();
                    end = start + t.length();

                    EnumSet<ColoringAttributes> attr = EnumSet.noneOf(ColoringAttributes.class);
                    boolean isStatic = modifiers != null && modifiers.contains(Modifier.STATIC);
                    if (isStatic) {
                        attr.add(ColoringAttributes.STATIC);
                    }
                    boolean isDeprecated = element != null && info.getElementUtilities().isDeprecated(element);
                    if (isDeprecated) {
                        attr.add(ColoringAttributes.DEPRECATED);
                    }

                    list.add(new Result(start, end, ResultTypes.METHOD, t, attr));
                    break;
                }
            }

            return super.visitFunctionDefinition(tree, list);
        }

        @Override
        public Void visitMethodInvocation(FunctionInvocationTree tree, List<Result> list) {
            SourcePositions sourcePositions = info.getTrees().getSourcePositions();
            long start = sourcePositions.getStartPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());
            long end = sourcePositions.getEndPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());

            if (start < 0 || end < 0) { // synthetic
                return super.visitMethodInvocation(tree, list);
            }

            Element element = info.getTrees().getElement(getCurrentPath());
            Set<Modifier> modifiers = FXSourceUtils.getModifiers(element);

            TokenSequence<VSGTokenId> ts = tu.tokensFor(tree);
            Token name = null;

            ts.moveEnd();
            boolean metLBrace = false;
            while (ts.movePrevious()) {
                Token t = ts.token();
                if (!metLBrace) {
                    metLBrace = VSGTokenId.LPAREN.equals(t.id());
                    if (!metLBrace) {
                        continue;
                    }
                }
                if (VSGTokenId.IDENTIFIER.equals(t.id())) {
                    start = ts.offset();
                    name = t; // last identifier followed left parenthis is a name
                    break;
                }
            }

            if (name != null) {
                end = start + name.length();

                EnumSet<ColoringAttributes> attr = EnumSet.noneOf(ColoringAttributes.class);
                boolean isStatic = modifiers != null && modifiers.contains(Modifier.STATIC);
                if (isStatic) {
                    attr.add(ColoringAttributes.STATIC);
                }
                    boolean isDeprecated = element != null && info.getElementUtilities().isDeprecated(element);
                if (isDeprecated) {
                    attr.add(ColoringAttributes.DEPRECATED);
                }

                list.add(new Result(start, end, ResultTypes.METHOD_INVOCATION, name, attr));
            }

            return super.visitMethodInvocation(tree, list);
        }

        @Override
        public Void visitVariable(VariableTree tree, List<Result> list) {
            SourcePositions sourcePositions = info.getTrees().getSourcePositions();
            long start = sourcePositions.getStartPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());
            long end = sourcePositions.getEndPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());

            if (start < 0 || end < 0) { // synthetic
                return super.visitVariable(tree, list);
            }

            Element element = info.getTrees().getElement(getCurrentPath());
            Set<Modifier> modifiers = FXSourceUtils.getModifiers(element);

            TokenSequence<VSGTokenId> ts = tu.tokensFor(tree);
            while (ts.moveNext()) {
                // do not highlight parameters and local variables
                if (element != null && !element.getKind().isField()) {
                    continue;
                }

                Token t = ts.token();
                if (VSGTokenId.IDENTIFIER.equals(t.id())) { // first identifier is a name
                    start = ts.offset();
                    end = start + t.length();

                    EnumSet<ColoringAttributes> attr = EnumSet.noneOf(ColoringAttributes.class);
                    boolean isStatic = modifiers != null && modifiers.contains(Modifier.STATIC);
                    if (isStatic) {
                        attr.add(ColoringAttributes.STATIC);
                    }
                    boolean isDeprecated = element != null && info.getElementUtilities().isDeprecated(element);
                    if (isDeprecated) {
                        attr.add(ColoringAttributes.DEPRECATED);
                    }

                    list.add(new Result(start, end, ResultTypes.FIELD, t, attr));
                    break;
                }
            }

            return super.visitVariable(tree, list);
        }

        @Override
        public Void visitIdentifier(IdentifierTree tree, List<Result> list) {
            SourcePositions sourcePositions = info.getTrees().getSourcePositions();
            long start = sourcePositions.getStartPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());
            long end = sourcePositions.getEndPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());

            if (start < 0 || end < 0) { // synthetic
                return super.visitIdentifier(tree, list);
            }

            Element element = info.getTrees().getElement(getCurrentPath());
            Set<Modifier> modifiers = FXSourceUtils.getModifiers(element);

            TokenSequence<VSGTokenId> ts = tu.tokensFor(tree);
            while (ts.moveNext()) {
                // do not highlight parameters, local variables and packages
                if (element == null || (element != null && !element.getKind().isField())) {
                    continue;
                }

                Token t = ts.token();
                if (VSGTokenId.IDENTIFIER.equals(t.id())) {
                    start = ts.offset();
                    end = start + t.length();

                    EnumSet<ColoringAttributes> attr = EnumSet.noneOf(ColoringAttributes.class);
                    boolean isStatic = modifiers != null && modifiers.contains(Modifier.STATIC);
                    if (isStatic) {
                        attr.add(ColoringAttributes.STATIC);
                    }
                    boolean isDeprecated = element != null && info.getElementUtilities().isDeprecated(element);
                    if (isDeprecated) {
                        attr.add(ColoringAttributes.DEPRECATED);
                    }
                    
                    identifiers.add(new Result(start, end, ResultTypes.VARIABLE, t, attr)); // identfiers chache
                    break;
                }
            }

            return super.visitIdentifier(tree, list);
        }

        @Override
        public Void visitClassDeclaration(ClassDeclarationTree tree, List<Result> list) {
            SourcePositions sourcePositions = info.getTrees().getSourcePositions();
            long start = sourcePositions.getStartPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());
            long end = sourcePositions.getEndPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());

            if (start < 0 || end < 0) { // synthetic
                return super.visitClassDeclaration(tree, list);
            }

            Element element = info.getTrees().getElement(getCurrentPath());
            Set<Modifier> modifiers = FXSourceUtils.getModifiers(element);

            TokenSequence<VSGTokenId> ts = tu.tokensFor(tree);
            while (ts.moveNext()) {
                Token t = ts.token();
                if (VSGTokenId.IDENTIFIER.equals(t.id())) { // first identifier is a name
                    start = ts.offset();
                    end = start + t.length();

                    EnumSet<ColoringAttributes> attr = EnumSet.noneOf(ColoringAttributes.class);
                    boolean isStatic = modifiers != null && modifiers.contains(Modifier.STATIC);
                    if (isStatic) {
                        attr.add(ColoringAttributes.STATIC);
                    }
                    boolean isDeprecated = element != null && info.getElementUtilities().isDeprecated(element);
                    if (isDeprecated) {
                        attr.add(ColoringAttributes.DEPRECATED);
                    }

                    list.add(new Result(start, end, ResultTypes.CLASS, t, attr));
                    break;
                }
            }

            return super.visitClassDeclaration(tree, list);
        }

        @Override
        public Void visitMemberSelect(MemberSelectTree tree, List<Result> list) {
            SourcePositions sourcePositions = info.getTrees().getSourcePositions();
            long start = sourcePositions.getStartPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());
            long end = sourcePositions.getEndPosition(info.getCompilationUnit(), getCurrentPath().getLeaf());

            if (start < 0 || end < 0) { // synthetic
                return super.visitMemberSelect(tree, list);
            }

            boolean handled = false;
            Element element = info.getTrees().getElement(getCurrentPath());
            if (element != null) {
                TokenSequence<VSGTokenId> ts = tu.tokensFor(tree);

                while (ts.moveNext()) {
                    Token t = ts.token();
                    String tokenStr = t.text().toString();

                    if (VSGTokenId.IDENTIFIER.equals(t.id())) {
                        start = ts.offset();
                        VisageTreePath subPath = tu.pathFor(getCurrentPath(), (int) start);
                        Element subElement = info.getTrees().getElement(subPath);

                        if (subElement != null) {
                            final Name simpleName = subElement.getSimpleName();

                            if (simpleName != null) {
                                String subElementName = simpleName.toString();

                                if (tokenStr.equals(subElementName)) {
                                    handled = true;
                                    Set<Modifier> modifiers = FXSourceUtils.getModifiers(element);
                                    start = ts.offset();
                                    end = start + t.length();

                                    EnumSet<ColoringAttributes> attr = EnumSet.noneOf(ColoringAttributes.class);
                                    boolean isStatic = modifiers != null && modifiers.contains(Modifier.STATIC);
                                    if (isStatic) {
                                        attr.add(ColoringAttributes.STATIC);
                                    }
                                    boolean isDeprecated = element != null && info.getElementUtilities().isDeprecated(element);
                                    if (isDeprecated) {
                                        attr.add(ColoringAttributes.DEPRECATED);
                                    }

                                    if (subElement.getKind().isField()) {
                                        list.add(new Result(start, end, ResultTypes.FIELD, t, attr));
                                    } else if (ElementKind.METHOD.equals(subElement.getKind())) {
                                        list.add(new Result(start, end, ResultTypes.METHOD_INVOCATION, t, attr));
                                    }
                                }
                            }
                        }
                    }

                }
            }

            return handled ? null : super.visitMemberSelect(tree, list);
        }
    }

    private enum ColoringAttributes {
        STATIC, DEPRECATED, UNUSED
    }

    private enum ResultTypes {
        METHOD, METHOD_INVOCATION, FIELD, VARIABLE, CLASS
    }

    private static class Result {
        private long start;
        private long end;
        private ResultTypes type;
        private Token token;
        private EnumSet<ColoringAttributes> attributes;

        public Result(long start, long end, ResultTypes type, Token token) {
            this(start, end, type, token, EnumSet.noneOf(ColoringAttributes.class));
        }

        public Result(long start, long end, ResultTypes type, Token token, EnumSet<ColoringAttributes> attributes) {
            this.start = start;
            this.end = end;
            this.type = type;
            this.token = token;
            this.attributes = attributes;
        }

        @Override
        public String toString() {
            return "[" + start + ", " + end + ", " + type + "]"; // NOI18N
        }
    }
}
