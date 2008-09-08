/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
 * Microsystems, Inc. All Rights Reserved.
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
 */
package org.netbeans.modules.javafx.editor.semantic;

import com.sun.javafx.api.tree.ExpressionTree;
import com.sun.javafx.api.tree.ForExpressionTree;
import com.sun.javafx.api.tree.JavaFXTreePath;
import com.sun.javafx.api.tree.SourcePositions;
import com.sun.javafx.api.tree.Tree;
import com.sun.javafx.api.tree.Tree.JavaFXKind;
import com.sun.javafx.api.tree.UnitTree;
import com.sun.javafx.api.tree.WhileLoopTree;
import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Element;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.AttributeSet;
import javax.swing.text.Document;
import javax.swing.text.StyleConstants;
import org.netbeans.api.editor.settings.AttributesUtilities;
import org.netbeans.api.javafx.lexer.JFXTokenId;
import org.netbeans.api.javafx.source.CancellableTask;
import org.netbeans.api.javafx.source.CompilationController;
import org.netbeans.api.javafx.source.CompilationInfo;
import org.netbeans.api.javafx.source.JavaFXSource.Phase;
import org.netbeans.api.javafx.source.TreeUtilities;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenHierarchy;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.spi.editor.highlighting.support.OffsetsBag;
import org.openide.filesystems.FileObject;
import org.openide.util.NbBundle;

/**
 *
 * @author Jan Lahoda
 */
public class MarkOccurrencesHighlighter implements CancellableTask<CompilationInfo> {

    public static final Color ES_COLOR = new Color(175, 172, 102); // new Color(244, 164, 113);

    private FileObject file;

    /** Creates a new instance of SemanticHighlighter */
    MarkOccurrencesHighlighter(FileObject file) {
        this.file = file;
    }

    public void run(CompilationInfo info) throws IOException {
        // dirty hack by nenik
        if (((CompilationController) info).toPhase(Phase.ANALYZED).lessThan(Phase.ANALYZED)) {
            return; // convert the element to local element
        }

        resume();

//        Document doc = info.getDocument();
        Document doc = info.getJavaFXSource().getDocument();

        if (doc == null) {
            Logger.getLogger(MarkOccurrencesHighlighter.class.getName()).log(Level.FINE, "SemanticHighlighter: Cannot get document!");
            return;
        }

//        Preferences node = MarkOccurencesSettings.getCurrentNode();
//        
//        if (!node.getBoolean(MarkOccurencesSettings.ON_OFF, true)) {
//            getHighlightsBag(doc).clear();
//            OccurrencesMarkProvider.get(doc).setOccurrences(Collections.<Mark>emptySet());
//            return ;
//        }

//        long start = System.currentTimeMillis();

        int caretPosition = MarkOccurrencesHighlighterFactory.getLastPosition(file);
        if (isCancelled()) {
            return;
        }

//        caretPosition = info.getPositionConverter().getJavaSourcePosition(caretPosition);

//        List<int[]> bag = processImpl(info, node, doc, caretPosition);
        List<int[]> bag = processImpl(info, doc, caretPosition);
        if (isCancelled()) {
            return;
        }

//        Logger.getLogger("TIMER").log(Level.FINE, "Occurrences",
//            new Object[] {((DataObject) doc.getProperty(Document.StreamDescriptionProperty)).getPrimaryFile(), (System.currentTimeMillis() - start)});

        if (bag == null) {
//            if (node.getBoolean(MarkOccurencesSettings.KEEP_MARKS, true)) {
//                return ;
//            }

            bag = new ArrayList<int[]>();
        }

        Collections.sort(bag, new Comparator<int[]>() {
            public int compare(int[] o1, int[] o2) {
                return o1[0] - o2[0];
            }
        });

        Iterator<int[]> it = bag.iterator();
        int[] last = it.hasNext() ? it.next() : null;
        List<int[]> result = new ArrayList<int[]>(bag.size());

        while (it.hasNext()) {
            int[] current = it.next();

            if (current[0] < last[1]) {
                //merge the highlights:
                last[1] = Math.max(current[1], last[1]);
            } else {
                result.add(last);
                last = current;
            }
        }

        if (last != null) {
            result.add(last);
        }

        OffsetsBag obag = new OffsetsBag(doc);
        obag.clear();

//        AttributeSet attributes = ColoringManager.getColoringImpl(MO);
        AttributeSet attributes = AttributesUtilities.createImmutable(StyleConstants.Background, new Color(236, 235, 163));

        for (int[] span : result) {
//            int convertedStart = info.getPositionConverter().getOriginalPosition(span[0]);
//            int convertedEnd   = info.getPositionConverter().getOriginalPosition(span[1]);
            int convertedStart = span[0];
            int convertedEnd = span[1];

            if (convertedStart != (-1) && convertedEnd != (-1)) {
                obag.addHighlight(convertedStart, convertedEnd, attributes);
            }
        }

        getHighlightsBag(doc).setHighlights(obag);
        OccurrencesMarkProvider.get(doc).setOccurrences(OccurrencesMarkProvider.createMarks(doc, bag, ES_COLOR, NbBundle.getMessage(MarkOccurrencesHighlighter.class, "LBL_ES_TOOLTIP")));
    }

    private boolean isIn(UnitTree cu, SourcePositions sp, Tree tree, int position) {
        return sp.getStartPosition(cu, tree) <= position && position <= sp.getEndPosition(cu, tree);
    }

    private boolean isIn(int caretPosition, Token<?> span) {
//        System.err.println("caretPosition = " + caretPosition );
//        System.err.println("span[0]= " + span[0]);
//        System.err.println("span[1]= " + span[1]);
        if (span == null) {
            return false;
        }

        return span.offset(null) <= caretPosition && caretPosition <= span.offset(null) + span.length();
    }

//    List<int[]> processImpl(CompilationInfo info, Preferences node, Document doc, int caretPosition) {
    List<int[]> processImpl(CompilationInfo info, Document doc, int caretPosition) {
        UnitTree cu = info.getCompilationUnit();
//        TreePath tp = info.getTreeUtilities().pathFor(caretPosition);
        TreeUtilities tu = new TreeUtilities(info);
        JavaFXTreePath tp = tu.pathFor(caretPosition);
        JavaFXTreePath typePath = findTypePath(tp);

        if (isCancelled()) {
            return null;
        }

        //detect caret inside the return type or throws clause:
        /* XXX[pn]: couldn't work before. Did it?
        if (typePath != null && typePath.getParentPath().getLeaf().getKind() == Kind.METHOD) {
        //hopefully found something, check:
        MethodTree decl = (MethodTree) typePath.getParentPath().getLeaf();
        Tree type = decl.getReturnType();
        
        //            if (   node.getBoolean(MarkOccurencesSettings.EXIT, true)
        //                && isIn(cu, info.getTrees().getSourcePositions(), type, caretPosition)) {
        if (isIn(cu, info.getTrees().getSourcePositions(), type, caretPosition)) {
        MethodExitDetector med = new MethodExitDetector();
        
        setExitDetector(med);
        
        try {
        return med.process(info, doc, decl, null);
        } finally {
        setExitDetector(null);
        }
        }
        
        for (Tree exc : decl.getThrows()) {
        //                if (   node.getBoolean(MarkOccurencesSettings.EXCEPTIONS, true)
        //                    && isIn(cu, info.getTrees().getSourcePositions(), exc, caretPosition)) {
        if (isIn(cu, info.getTrees().getSourcePositions(), exc, caretPosition)) {
        MethodExitDetector med = new MethodExitDetector();
        
        setExitDetector(med);
        
        try {
        return med.process(info, doc, decl, Collections.singletonList(exc));
        } finally {
        setExitDetector(null);
        }
        }
        }
        }*/

        if (isCancelled()) {
            return null;
        }

//        if (node.getBoolean(MarkOccurencesSettings.IMPLEMENTS, true)) {
        if (true) {
            //detect caret inside the extends/implements clause:
            /* XXX[pn]: couldn't work before. Did it? 
            if (typePath != null && typePath.getParentPath().getLeaf().getKind() == Kind.CLASS) {
            ClassTree ctree = (ClassTree) typePath.getParentPath().getLeaf();
            int bodyStart = Utilities.findBodyStart(ctree, cu, info.getTrees().getSourcePositions(), doc);
            
            boolean isExtends = ctree.getExtendsClause() == typePath.getLeaf();
            //                boolean isImplements = false;
            
            //                for (Tree t : ctree.getImplementsClause()) {
            //                    if (t == typePath.getLeaf()) {
            //                        isImplements = true;
            //                        break;
            //                    }
            //                }
            
            //                if (   (isExtends && node.getBoolean(MarkOccurencesSettings.OVERRIDES, true))
            //                    || (isImplements && node.getBoolean(MarkOccurencesSettings.IMPLEMENTS, true))) {
            if (isExtends) {
            Element superType = info.getTrees().getElement(typePath);
            Element thisType  = info.getTrees().getElement(typePath.getParentPath());
            
            if (isClass(superType) && isClass(thisType))
            return detectMethodsForClass(info, doc, typePath.getParentPath(), (TypeElement) superType, (TypeElement) thisType);
            }
            }*/

            if (isCancelled()) {
                return null;
            }

            TokenSequence<JFXTokenId> ts = ((TokenHierarchy<?>) info.getTokenHierarchy()).tokenSequence(JFXTokenId.language());

        /* XXX[pn]: couldn't work before. Did it?
        if (ts != null && tp.getLeaf().getKind() == Kind.CLASS) {
        int bodyStart = Utilities.findBodyStart(tp.getLeaf(), cu, info.getTrees().getSourcePositions(), doc);
        
        if (caretPosition < bodyStart) {
        ts.move(caretPosition);
        
        if (ts.moveNext()) {
        //                        if (node.getBoolean(MarkOccurencesSettings.OVERRIDES, true) && ts.token().id() == JavaTokenId.EXTENDS) {
        if (ts.token().id() == JFXTokenId.EXTENDS) {
        Tree superClass = ((ClassTree) tp.getLeaf()).getExtendsClause();
        
        if (superClass != null) {
        Element superType = info.getTrees().getElement(new TreePath(tp, superClass));
        Element thisType  = info.getTrees().getElement(tp);
        
        if (isClass(superType) && isClass(thisType))
        return detectMethodsForClass(info, doc, tp, (TypeElement) superType, (TypeElement) thisType);
        }
        }
        
        //                        if (node.getBoolean(MarkOccurencesSettings.IMPLEMENTS, true) && ts.token().id() == JavaTokenId.IMPLEMENTS) {
        //                            List<? extends Tree> superClasses = ((ClassTree) tp.getLeaf()).getImplementsClause();
        //                            
        //                            if (superClasses != null) {
        //                                List<TypeElement> superTypes = new ArrayList<TypeElement>();
        //                                
        //                                for (Tree superTypeTree : superClasses) {
        //                                    if (superTypeTree != null) {
        //                                        Element superType = info.getTrees().getElement(new TreePath(tp, superTypeTree));
        //                                        
        //                                        if (isClass(superType))
        //                                            superTypes.add((TypeElement) superType);
        //                                    }
        //                                }
        //                                
        //                                Element thisType  = info.getTrees().getElement(tp);
        //                                
        //                                if (!superTypes.isEmpty() && isClass(thisType))
        //                                    return detectMethodsForClass(info, doc, tp, superTypes, (TypeElement) thisType);
        //                            }
        //                            
        //                        }
        }
        }
        }*/
        }

        if (isCancelled()) {
            return null;
        }

        Tree tree = tp.getLeaf();

//        if (node.getBoolean(MarkOccurencesSettings.BREAK_CONTINUE, true) && (tree.getKind() == Kind.BREAK || tree.getKind() == Kind.CONTINUE)) {
        if ((tree.getJavaFXKind() == JavaFXKind.BREAK || tree.getJavaFXKind() == JavaFXKind.CONTINUE)) {
            return detectBreakOrContinueTarget(info, doc, tp);
        }

        Element el;

//        el = JavadocImports.findReferencedElement(info, caretPosition);
//        boolean insideJavadoc = el != null;

        if (isCancelled()) {
            return null;
        }

        //variable declaration:
//        if (!insideJavadoc) {
        el = info.getTrees().getElement(tp);
//        }

        if (el != null && (!(false/*tree.getKind() == Kind.CLASS*/) || isIn(caretPosition, Utilities.findIdentifierSpan(info, doc, tp))) && !Utilities.isKeyword(tree) && (!(false/*tree.getKind() == Kind.METHOD*/) || isIn(caretPosition, Utilities.findIdentifierSpan(info, doc, tp))) //                && isEnabled(node, el)
                //                || (insideJavadoc && isEnabled(node, el))
                ) {
            FindLocalUsagesQuery fluq = new FindLocalUsagesQuery();

            setLocalUsages(fluq);

            try {
                List<int[]> bag = new ArrayList<int[]>();
                for (Token<?> t : fluq.findUsages(el, info, doc)) {
                    bag.add(new int[]{t.offset(null), t.offset(null) + t.length()});
                }

                return bag;
            } finally {
                setLocalUsages(null);
            }
        }

        return null;
    }
    private static final Set<JavaFXKind> TYPE_PATH_ELEMENT = EnumSet.of(JavaFXKind.IDENTIFIER, /*JavaFXKind.PRIMITIVE_TYPE, JavaFXKind.PARAMETERIZED_TYPE,*/ JavaFXKind.MEMBER_SELECT/*, JavaFXKind.ARRAY_TYPE*/);

    private static JavaFXTreePath findTypePath(JavaFXTreePath tp) {
        if (!TYPE_PATH_ELEMENT.contains(tp.getLeaf().getJavaFXKind())) {
            return null;
        }

        while (TYPE_PATH_ELEMENT.contains(tp.getParentPath().getLeaf().getJavaFXKind())) {
            tp = tp.getParentPath();
        }

        return tp;
    }

    private static boolean isClass(Element el) {
        return el != null && (el.getKind().isClass() || el.getKind().isInterface());
    }
    
//    private static boolean isEnabled(Preferences node, Element el) {
//        switch (el.getKind()) {
//            case ANNOTATION_TYPE:
//            case CLASS:
//            case ENUM:
//            case INTERFACE:
//            case TYPE_PARAMETER: //???
//                return node.getBoolean(MarkOccurencesSettings.TYPES, true);
//            case CONSTRUCTOR:
//            case METHOD:
//                return node.getBoolean(MarkOccurencesSettings.METHODS, true);
//            case ENUM_CONSTANT:
//                return node.getBoolean(MarkOccurencesSettings.CONSTANTS, true);
//            case FIELD:
//                if (el.getModifiers().containsAll(EnumSet.of(Modifier.STATIC, Modifier.FINAL))) {
//                    return node.getBoolean(MarkOccurencesSettings.CONSTANTS, true);
//                } else {
//                    return node.getBoolean(MarkOccurencesSettings.FIELDS, true);
//                }
//            case LOCAL_VARIABLE:
//            case PARAMETER:
//            case EXCEPTION_PARAMETER:
//                return node.getBoolean(MarkOccurencesSettings.LOCAL_VARIABLES, true);
//            case PACKAGE:
//                return false; //never mark occurrence packages
//            default:
//                Logger.getLogger(MarkOccurrencesHighlighter.class.getName()).log(Level.INFO, "Unknow element type: {0}.", el.getKind());
//                return true;
//        }
//    }
    
    private boolean canceled;
    private MethodExitDetector exitDetector;
    private FindLocalUsagesQuery localUsages;

    private final synchronized void setExitDetector(MethodExitDetector detector) {
        this.exitDetector = detector;
    }

    private final synchronized void setLocalUsages(FindLocalUsagesQuery localUsages) {
        this.localUsages = localUsages;
    }

    public final synchronized void cancel() {
        canceled = true;

        if (exitDetector != null) {
            exitDetector.cancel();
        }
        if (localUsages != null) {
            localUsages.cancel();
        }
    }

    protected final synchronized boolean isCancelled() {
        return canceled;
    }

    protected final synchronized void resume() {
        canceled = false;
    }

    /*    private List<int[]> detectMethodsForClass(CompilationInfo info, Document document, JavaFXTreePath clazz, TypeElement superType, TypeElement thisType) {
    return detectMethodsForClass(info, document, clazz, Collections.singletonList(superType), thisType);
    }
    
    //    static Coloring MO = ColoringAttributes.add(ColoringAttributes.empty(), ColoringAttributes.MARK_OCCURRENCES);
    
    private List<int[]> detectMethodsForClass(CompilationInfo info, Document document, JavaFXTreePath clazz, List<TypeElement> superTypes, TypeElement thisType) {
    List<int[]> highlights = new ArrayList<int[]>();
    ClassTree clazzTree = (ClassTree) clazz.getLeaf();
    TypeElement jlObject = info.getElements().getTypeElement("java.lang.Object");
    
    OUTER: for (Tree member: clazzTree.getMembers()) {
    if (isCancelled()) {
    return null;
    }
    
    if (member.getKind() == Kind.METHOD) {
    TreePath path = new TreePath(clazz, member);
    Element el = info.getTrees().getElement(path);
    
    if (el.getKind() == ElementKind.METHOD) {
    for (TypeElement superType : superTypes) {
    for (ExecutableElement ee : ElementFilter.methodsIn(info.getElements().getAllMembers(superType))) {
    if (info.getElements().overrides((ExecutableElement) el, ee, thisType) && (superType.getKind().isClass() || !ee.getEnclosingElement().equals(jlObject))) {
    Token<JFXTokenId> t = Utilities.getToken(info, document, path);
    
    if (t != null) {
    highlights.add(new int[] {t.offset(null), t.offset(null) + t.length()});
    }
    continue OUTER;
    }
    }
    }
    }
    }
    }
    
    return highlights;
    }
     */
    private List<int[]> detectBreakOrContinueTarget(CompilationInfo info, Document document, JavaFXTreePath breakOrContinue) {
        List<int[]> result = new ArrayList<int[]>();
//        StatementTree target = info.getTreeUtilities().getBreakContinueTarget(breakOrContinue);
        ExpressionTree target = new TreeUtilities(info).getBreakContinueTarget(breakOrContinue);

        if (target == null) {
            return null;
        }

        TokenSequence<JFXTokenId> ts = ((TokenHierarchy<?>) info.getTokenHierarchy()).tokenSequence(JFXTokenId.language());
        ts.move((int) info.getTrees().getSourcePositions().getStartPosition(info.getCompilationUnit(), target));
        if (ts.moveNext()) {
            result.add(new int[]{ts.offset(), ts.offset() + ts.token().length()});
        }

//        no labels:        
//        StatementTree statement = target.getKind() == Kind.LABELED_STATEMENT ? ((LabeledStatementTree) target).getStatement() : target;
        ExpressionTree statement = target;
        Tree block = null;

        switch (statement.getJavaFXKind()) {
            case WHILE_LOOP:
                if (((WhileLoopTree) statement).getStatement().getJavaFXKind() == JavaFXKind.BLOCK_EXPRESSION) {
                    block = ((WhileLoopTree) statement).getStatement();
                }
                break;
            case FOR_EXPRESSION_FOR:
                if (((ForExpressionTree) statement).getBodyExpression().getJavaFXKind() == JavaFXKind.BLOCK_EXPRESSION) {
                    block = ((ForExpressionTree) statement).getBodyExpression();
                }
                break;
        }

        if (block != null) {
            ts.move((int) info.getTrees().getSourcePositions().getEndPosition(info.getCompilationUnit(), block));

            if (ts.movePrevious() && ts.token().id() == JFXTokenId.RBRACE) {
                result.add(new int[]{ts.offset(), ts.offset() + ts.token().length()});
            }
        }

        return result;
    }

    static OffsetsBag getHighlightsBag(Document doc) {
        OffsetsBag bag = (OffsetsBag) doc.getProperty(MarkOccurrencesHighlighter.class);

        if (bag == null) {
            doc.putProperty(MarkOccurrencesHighlighter.class, bag = new OffsetsBag(doc, false));

//            Object stream = doc.getProperty(Document.StreamDescriptionProperty);
            final OffsetsBag bagFin = bag;
            DocumentListener l = new DocumentListener() {
                public void insertUpdate(DocumentEvent e) {
                    bagFin.removeHighlights(e.getOffset(), e.getOffset(), false);
                }
                public void removeUpdate(DocumentEvent e) {
                    bagFin.removeHighlights(e.getOffset(), e.getOffset(), false);
                }
                public void changedUpdate(DocumentEvent e) {
                }
            };

            doc.addDocumentListener(l);

//            if (stream instanceof DataObject) {
//                Logger.getLogger("TIMER").log(Level.FINE, "MarkOccurrences Highlights Bag", new Object[] {((DataObject) stream).getPrimaryFile(), bag}); //NOI18N
//                Logger.getLogger("TIMER").log(Level.FINE, "MarkOccurrences Highlights Bag Listener", new Object[] {((DataObject) stream).getPrimaryFile(), l}); //NOI18N
//            }
        }

        return bag;
    }
    
}

