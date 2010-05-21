/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
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
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
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

package org.netbeans.modules.javafx.editor.format;

import java.util.prefs.Preferences;
import javax.swing.text.Document;
import org.netbeans.api.javafx.editor.FXSourceUtils;
import org.netbeans.api.project.Project;
import org.netbeans.modules.javafx.editor.format.ui.FmtOptions;
import static org.netbeans.modules.javafx.editor.format.ui.FmtOptions.*;
import org.netbeans.modules.editor.indent.spi.CodeStylePreferences;
import org.openide.filesystems.FileObject;

/** 
 * Code style settings registry for reformatting
 * 
 * @author Dusan Balek
 * @author Anton Chechel - javafx changes
 */
public final class CodeStyle {
    
    static {
        FmtOptions.codeStyleProducer = new Producer();
    }
    
    private Preferences preferences;
    
    private CodeStyle(Preferences preferences) {
        this.preferences = preferences;
    }

    /**
     * Gets <code>CodeStyle</code> for files in the given project.
     *
     * <p>Please see the other two <code>getDefault</code> methods as they are
     * the preferred way of getting <code>CodeStyle</code>.
     *
     * @param project The project to get the <code>CodeStyle</code> for.
     * @return The current code style that would be used by documents opened
     *   from files belonging to the <code>project</code>.
     *
     * @deprecated Please use {@link #getDefault(javax.swing.text.Document)}
     *   or {@link #getDefault(org.openide.filesystems.FileObject)} respectively.
     */
    public static CodeStyle getDefault(Project project) {
        return getDefault(project.getProjectDirectory());
    }
    
    /**
     * Gets <code>CodeStyle</code> for the given file. If you have a document
     * instance you should use the {@link #getDefault(javax.swing.text.Document)}
     * method.
     * 
     * @param file The file to get the <code>CodeStyle</code> for.
     * @return The current code style that would be used by a document if the
     *   <code>file</code> were opened in the editor.
     *
     * @since 0.39
     */
    public synchronized static CodeStyle getDefault(FileObject file) {
        Preferences prefs = CodeStylePreferences.get(file, FXSourceUtils.MIME_TYPE).getPreferences();
        return FmtOptions.codeStyleProducer.create(prefs);
    }

    /**
     * Gets <code>CodeStyle</code> for the given document. This is the preferred
     * method of getting <code>CodeStyle</code>. If you don't have a document
     * you can use {@link #getDefault(org.openide.filesystems.FileObject)} method instead.
     *
     * @param doc The document to get the <code>CodeStyle</code> for.
     * @return The current code style used by a document. This is the code style that
     *   will be used when formatting the document or generating new code.
     * 
     * @since 0.39
     */
    public synchronized static CodeStyle getDefault(Document doc) {
        Preferences prefs = CodeStylePreferences.get(doc, FXSourceUtils.MIME_TYPE).getPreferences();
        return FmtOptions.codeStyleProducer.create(prefs);
    }
    
    // General tabs and indents ------------------------------------------------
    
    public boolean expandTabToSpaces() {
//        System.out.println("~~~ expand-tabs=" + preferences.get(SimpleValueNames.EXPAND_TABS, null));
        return preferences.getBoolean(expandTabToSpaces, getDefaultAsBoolean(expandTabToSpaces));
    }

    public int getTabSize() {
//        System.out.println("~~~ tab-size=" + preferences.get(SimpleValueNames.TAB_SIZE, null));
        return preferences.getInt(tabSize, getDefaultAsInt(tabSize));
    }

    public int getIndentSize() {
//        System.out.println("~~~ indent-shift-width=" + preferences.get(SimpleValueNames.INDENT_SHIFT_WIDTH, null));
        int indentLevel = preferences.getInt(indentSize, getDefaultAsInt(indentSize));

        if (indentLevel <= 0) {
//            System.out.println("~~~ expand-tabs=" + preferences.get(SimpleValueNames.EXPAND_TABS, null));
            boolean expandTabs = preferences.getBoolean(expandTabToSpaces, getDefaultAsBoolean(expandTabToSpaces));
            if (expandTabs) {
//                System.out.println("~~~ spaces-per-tab=" + preferences.get(SimpleValueNames.SPACES_PER_TAB, null));
                indentLevel = preferences.getInt(spacesPerTab, getDefaultAsInt(spacesPerTab));
            } else {
//                System.out.println("~~~ tab-size=" + preferences.get(SimpleValueNames.TAB_SIZE, null));
                indentLevel = preferences.getInt(tabSize, getDefaultAsInt(tabSize));
            }
        }
        
        return indentLevel;
    }

    public int getContinuationIndentSize() {
        return preferences.getInt(continuationIndentSize, getDefaultAsInt(continuationIndentSize));
    }

    public boolean indentTopLevelClassMembers() {
        return preferences.getBoolean(indentTopLevelClassMembers, getDefaultAsBoolean(indentTopLevelClassMembers));
    }
    
    public int getRightMargin() {
        return preferences.getInt(rightMargin, getDefaultAsInt(rightMargin));
    }

    public boolean addLeadingStarInComment() {
        return preferences.getBoolean(addLeadingStarInComment, getDefaultAsBoolean(addLeadingStarInComment));
    }

    // Code generation ---------------------------------------------------------
    
    public boolean preferLongerNames() {
        return preferences.getBoolean(preferLongerNames, getDefaultAsBoolean(preferLongerNames));
    }

    public String getFieldNamePrefix() {
        return preferences.get(fieldNamePrefix, getDefaultAsString(fieldNamePrefix));
    }

    public String getFieldNameSuffix() {
        return preferences.get(fieldNameSuffix, getDefaultAsString(fieldNameSuffix));
    }

    public String getStaticFieldNamePrefix() {
        return preferences.get(staticFieldNamePrefix, getDefaultAsString(staticFieldNamePrefix));
    }

    public String getStaticFieldNameSuffix() {
        return preferences.get(staticFieldNameSuffix, getDefaultAsString(staticFieldNameSuffix));
    }

    public String getParameterNamePrefix() {
        return preferences.get(parameterNamePrefix, getDefaultAsString(parameterNamePrefix));
    }

    public String getParameterNameSuffix() {
        return preferences.get(parameterNameSuffix, getDefaultAsString(parameterNameSuffix));
    }

    public String getLocalVarNamePrefix() {
        return preferences.get(localVarNamePrefix, getDefaultAsString(localVarNamePrefix));
    }

    public String getLocalVarNameSuffix() {
        return preferences.get(localVarNameSuffix, getDefaultAsString(localVarNameSuffix));
    }

    public boolean qualifyFieldAccess() {
        return preferences.getBoolean(qualifyFieldAccess, getDefaultAsBoolean(qualifyFieldAccess));
    }

    public boolean useIsForBooleanGetters() {
        return preferences.getBoolean(useIsForBooleanGetters, getDefaultAsBoolean(useIsForBooleanGetters));
    }

    public boolean addOverrideAnnotation() {
        return preferences.getBoolean(addOverrideAnnotation, getDefaultAsBoolean(addOverrideAnnotation));
    }

    public boolean makeLocalVarsFinal() {
        return preferences.getBoolean(makeLocalVarsFinal, getDefaultAsBoolean(makeLocalVarsFinal));
    }

    public boolean makeParametersFinal() {
        return preferences.getBoolean(makeParametersFinal, getDefaultAsBoolean(useFQNs));
    }

    // Alignment and braces ----------------------------------------------------
    
    public BracePlacement getClassDeclBracePlacement() {
        String placement = preferences.get(classDeclBracePlacement, getDefaultAsString(classDeclBracePlacement));
        return BracePlacement.valueOf(placement);
    }

    public BracePlacement getFunctionDeclBracePlacement() {
        String placement = preferences.get(functionDeclBracePlacement, getDefaultAsString(functionDeclBracePlacement));
        return BracePlacement.valueOf(placement);
    }

    public BracePlacement getObjectLiteralBracePlacement() {
        String placement = preferences.get(objectLiteralBracePlacement, getDefaultAsString(objectLiteralBracePlacement));
        return BracePlacement.valueOf(placement);
    }

    public BracePlacement getOnReplacePlacement() {
        String placement = preferences.get(onReplacePlacement, getDefaultAsString(onReplacePlacement));
        return BracePlacement.valueOf(placement);
    }

    public BracePlacement getOtherBracePlacement() {
        String placement = preferences.get(otherBracePlacement, getDefaultAsString(otherBracePlacement));
        return BracePlacement.valueOf(placement);
    }

    public boolean specialElseIf() {
        return preferences.getBoolean(specialElseIf, getDefaultAsBoolean(specialElseIf));
    }

    public BracesGenerationStyle redundantIfBraces() {
        String redundant = preferences.get(redundantIfBraces, getDefaultAsString(redundantIfBraces));
        return BracesGenerationStyle.valueOf(redundant);
    }

    public BracesGenerationStyle redundantForBraces() {
        String redundant = preferences.get(redundantForBraces, getDefaultAsString(redundantForBraces));
        return BracesGenerationStyle.valueOf(redundant);
    }

    public BracesGenerationStyle redundantWhileBraces() {
        String redundant = preferences.get(redundantWhileBraces, getDefaultAsString(redundantWhileBraces));
        return BracesGenerationStyle.valueOf(redundant);
    }

    public boolean alignMultilineMethodParams() {
        return preferences.getBoolean(alignMultilineMethodParams, getDefaultAsBoolean(alignMultilineMethodParams));
    }

    public boolean alignMultilineCallArgs() {
        return preferences.getBoolean(alignMultilineCallArgs, getDefaultAsBoolean(alignMultilineCallArgs));
    }

    public boolean alignMultilineAnnotationArgs() {
        return preferences.getBoolean(alignMultilineAnnotationArgs, getDefaultAsBoolean(alignMultilineAnnotationArgs));
    }

    public boolean alignMultilineImplements() {
        return preferences.getBoolean(alignMultilineImplements, getDefaultAsBoolean(alignMultilineImplements));
    }

    public boolean alignMultilineThrows() {
        return preferences.getBoolean(alignMultilineThrows, getDefaultAsBoolean(alignMultilineThrows));
    }

    public boolean alignMultilineBinaryOp() {
        return preferences.getBoolean(alignMultilineBinaryOp, getDefaultAsBoolean(alignMultilineBinaryOp));
    }

    public boolean alignMultilineAssignment() {
        return preferences.getBoolean(alignMultilineAssignment, getDefaultAsBoolean(alignMultilineAssignment));
    }

    public boolean alignSequenceInit() {
        return preferences.getBoolean(alignSequenceInit, getDefaultAsBoolean(alignSequenceInit));
    }

    public boolean placeElseOnNewLine() {
        return preferences.getBoolean(placeElseOnNewLine, getDefaultAsBoolean(placeElseOnNewLine));
    }

    public boolean placeCatchOnNewLine() {
        return preferences.getBoolean(placeCatchOnNewLine, getDefaultAsBoolean(placeCatchOnNewLine));
    }

    public boolean placeFinallyOnNewLine() {
        return preferences.getBoolean(placeFinallyOnNewLine, getDefaultAsBoolean(placeFinallyOnNewLine));
    }
    
    public boolean placeNewLineAfterModifiers() {
        return preferences.getBoolean(placeNewLineAfterModifiers, getDefaultAsBoolean(placeNewLineAfterModifiers));
    }

    // Wrapping ----------------------------------------------------------------
    
    public WrapStyle wrapExtendsImplementsKeyword() {
        String wrap = preferences.get(wrapExtendsImplementsKeyword, getDefaultAsString(wrapExtendsImplementsKeyword));
        return WrapStyle.valueOf(wrap);
    }

    public WrapStyle wrapExtendsImplementsList() {
        String wrap = preferences.get(wrapExtendsImplementsList, getDefaultAsString(wrapExtendsImplementsList));
        return WrapStyle.valueOf(wrap);
    }

    public WrapStyle wrapMethodParams() {
        String wrap = preferences.get(wrapFunctionParams, getDefaultAsString(wrapFunctionParams));
        return WrapStyle.valueOf(wrap);
    }

    public WrapStyle wrapMethodCallArgs() {
        String wrap = preferences.get(wrapFunctionCallArgs, getDefaultAsString(wrapFunctionCallArgs));
        return WrapStyle.valueOf(wrap);
    }

    public WrapStyle wrapChainedMethodCalls() {
        String wrap = preferences.get(wrapChainedFunctionCalls, getDefaultAsString(wrapChainedFunctionCalls));
        return WrapStyle.valueOf(wrap);
    }

    public WrapStyle wrapSequenceInit() {
        String wrap = preferences.get(wrapSequenceInit, getDefaultAsString(wrapSequenceInit));
        return WrapStyle.valueOf(wrap);
    }

    public WrapStyle wrapForStatement() {
        String wrap = preferences.get(wrapForStatement, getDefaultAsString(wrapForStatement));
        return WrapStyle.valueOf(wrap);
    }

    public WrapStyle wrapIfexpression() {
        String wrap = preferences.get(wrapIfExpression, getDefaultAsString(wrapIfExpression));
        return WrapStyle.valueOf(wrap);
    }

    public WrapStyle wrapWhileStatement() {
        String wrap = preferences.get(wrapWhileStatement, getDefaultAsString(wrapWhileStatement));
        return WrapStyle.valueOf(wrap);
    }

    public WrapStyle wrapBinaryOps() {
        String wrap = preferences.get(wrapBinaryOps, getDefaultAsString(wrapBinaryOps));
        return WrapStyle.valueOf(wrap);
    }

    public WrapStyle wrapAssignOps() {
        String wrap = preferences.get(wrapAssignOps, getDefaultAsString(wrapAssignOps));
        return WrapStyle.valueOf(wrap);
    }

    // Blank lines -------------------------------------------------------------
    
    public int getBlankLinesBeforePackage() {
        return preferences.getInt(blankLinesBeforePackage, getDefaultAsInt(blankLinesBeforePackage));
    }

    public int getBlankLinesAfterPackage() {
        return preferences.getInt(blankLinesAfterPackage, getDefaultAsInt(blankLinesAfterPackage));
    }

    public int getBlankLinesBeforeImports() {
        return preferences.getInt(blankLinesBeforeImports, getDefaultAsInt(blankLinesBeforeImports));
    }

    public int getBlankLinesAfterImports() {
        return preferences.getInt(blankLinesAfterImports, getDefaultAsInt(blankLinesAfterImports));
    }

    public int getBlankLinesBeforeClass() {
        return preferences.getInt(blankLinesBeforeClass, getDefaultAsInt(blankLinesBeforeClass));
    }

    public int getBlankLinesAfterClass() {
        return preferences.getInt(blankLinesAfterClass, getDefaultAsInt(blankLinesAfterClass));
    }

    public int getBlankLinesAfterClassHeader() {
        return preferences.getInt(blankLinesAfterClassHeader, getDefaultAsInt(blankLinesAfterClassHeader));
    }

    public int getBlankLinesBeforeFields() {
        return preferences.getInt(blankLinesBeforeFields, getDefaultAsInt(blankLinesBeforeFields));
    }

    public int getBlankLinesAfterFields() {
        return preferences.getInt(blankLinesAfterFields, getDefaultAsInt(blankLinesAfterFields));
    }

    public int getBlankLinesBeforeMethods() {
        return preferences.getInt(blankLinesBeforeMethods, getDefaultAsInt(blankLinesBeforeMethods));
    }

    public int getBlankLinesAfterMethods() {
        return preferences.getInt(blankLinesAfterMethods, getDefaultAsInt(blankLinesAfterMethods));
    }

    public int getBlankLinesBeforeNonClassExpression() {
        return preferences.getInt(blankLinesBeforeNonClassExpression, getDefaultAsInt(blankLinesBeforeNonClassExpression));
    }

    public int getBlankLinesAfterNonClassExpression() {
        return preferences.getInt(blankLinesAfterNonClassExpression, getDefaultAsInt(blankLinesAfterNonClassExpression));
    }

    // Spaces ------------------------------------------------------------------
    
    public boolean spaceBeforeElse() {
        return preferences.getBoolean(spaceBeforeElse, getDefaultAsBoolean(spaceBeforeElse));
    }

    public boolean spaceBeforeCatch() {
        return preferences.getBoolean(spaceBeforeCatch, getDefaultAsBoolean(spaceBeforeCatch));
    }

    public boolean spaceBeforeFinally() {
        return preferences.getBoolean(spaceBeforeFinally, getDefaultAsBoolean(spaceBeforeFinally));
    }

    public boolean spaceBeforeMethodDeclParen() {
        return preferences.getBoolean(spaceBeforeFunctionDeclParen, getDefaultAsBoolean(spaceBeforeFunctionDeclParen));
    }

    public boolean spaceBeforeMethodCallParen() {
        return preferences.getBoolean(spaceBeforeFunctionCallParen, getDefaultAsBoolean(spaceBeforeFunctionCallParen));
    }

    public boolean spaceBeforeIfParen() {
        return preferences.getBoolean(spaceBeforeIfParen, getDefaultAsBoolean(spaceBeforeIfParen));
    }

    public boolean spaceBeforeForParen() {
        return preferences.getBoolean(spaceBeforeForParen, getDefaultAsBoolean(spaceBeforeForParen));
    }

    public boolean spaceBeforeWhileParen() {
        return preferences.getBoolean(spaceBeforeWhileParen, getDefaultAsBoolean(spaceBeforeWhileParen));
    }

    public boolean spaceBeforeCatchParen() {
        return preferences.getBoolean(spaceBeforeCatchParen, getDefaultAsBoolean(spaceBeforeCatchParen));
    }

    public boolean spaceBeforeSwitchParen() {
        return preferences.getBoolean(spaceBeforeSwitchParen, getDefaultAsBoolean(spaceBeforeSwitchParen));
    }

    public boolean spaceBeforeSynchronizedParen() {
        return preferences.getBoolean(spaceBeforeSynchronizedParen, getDefaultAsBoolean(spaceBeforeSynchronizedParen));
    }

    public boolean spaceBeforeAnnotationParen() {
        return preferences.getBoolean(spaceBeforeAnnotationParen, getDefaultAsBoolean(spaceBeforeAnnotationParen));
    }

    public boolean spaceAroundUnaryOps() {
        return preferences.getBoolean(spaceAroundUnaryOps, getDefaultAsBoolean(spaceAroundUnaryOps));
    }

    public boolean spaceAroundBinaryOps() {
        return preferences.getBoolean(spaceAroundBinaryOps, getDefaultAsBoolean(spaceAroundBinaryOps));
    }

    public boolean spaceAroundRangeOps() {
        return preferences.getBoolean(spaceAroundRangeOps, getDefaultAsBoolean(spaceAroundRangeOps));
    }

    public boolean spaceAroundAssignOps() {
        return preferences.getBoolean(spaceAroundAssignOps, getDefaultAsBoolean(spaceAroundAssignOps));
    }

    public boolean spaceBeforeClassDeclLeftBrace() {
        return preferences.getBoolean(spaceBeforeClassDeclLeftBrace, getDefaultAsBoolean(spaceBeforeClassDeclLeftBrace));
    }

    public boolean spaceBeforeFunctionDeclLeftBrace() {
        return preferences.getBoolean(spaceBeforeFunctionDeclLeftBrace, getDefaultAsBoolean(spaceBeforeFunctionDeclLeftBrace));
    }

    public boolean spaceBeforeInitBlockLeftBrace() {
        return preferences.getBoolean(spaceBeforeInitBlockLeftBrace, getDefaultAsBoolean(spaceBeforeInitBlockLeftBrace));
    }

    public boolean spaceBeforePostInitBlockLeftBrace() {
        return preferences.getBoolean(spaceBeforePostInitBlockLeftBrace, getDefaultAsBoolean(spaceBeforePostInitBlockLeftBrace));
    }

    public boolean spaceBeforeObjectLiteralDeclLeftBrace() {
        return preferences.getBoolean(spaceBeforeObjectLiteralDeclLeftBrace, getDefaultAsBoolean(spaceBeforeObjectLiteralDeclLeftBrace));
    }

    public boolean spaceBeforeOnReplaceDeclLeftBrace() {
        return preferences.getBoolean(spaceBeforeOnReplaceDeclLeftBrace, getDefaultAsBoolean(spaceBeforeOnReplaceDeclLeftBrace));
    }

    public boolean spaceBeforeIfLeftBrace() {
        return preferences.getBoolean(spaceBeforeIfLeftBrace, getDefaultAsBoolean(spaceBeforeIfLeftBrace));
    }

    public boolean spaceBeforeElseLeftBrace() {
        return preferences.getBoolean(spaceBeforeElseLeftBrace, getDefaultAsBoolean(spaceBeforeElseLeftBrace));
    }

    public boolean spaceBeforeWhileLeftBrace() {
        return preferences.getBoolean(spaceBeforeWhileLeftBrace, getDefaultAsBoolean(spaceBeforeWhileLeftBrace));
    }

    public boolean spaceBeforeForLeftBrace() {
        return preferences.getBoolean(spaceBeforeForLeftBrace, getDefaultAsBoolean(spaceBeforeForLeftBrace));
    }

    public boolean spaceBeforeTryLeftBrace() {
        return preferences.getBoolean(spaceBeforeTryLeftBrace, getDefaultAsBoolean(spaceBeforeTryLeftBrace));
    }

    public boolean spaceBeforeCatchLeftBrace() {
        return preferences.getBoolean(spaceBeforeCatchLeftBrace, getDefaultAsBoolean(spaceBeforeCatchLeftBrace));
    }

    public boolean spaceBeforeFinallyLeftBrace() {
        return preferences.getBoolean(spaceBeforeFinallyLeftBrace, getDefaultAsBoolean(spaceBeforeFinallyLeftBrace));
    }

    public boolean spaceBeforeSequenceInitLeftBrace() {
        return preferences.getBoolean(spaceBeforeSequenceInitLeftBrace, getDefaultAsBoolean(spaceBeforeSequenceInitLeftBrace));
    }

    public boolean spaceWithinParens() {
        return preferences.getBoolean(spaceWithinParens, getDefaultAsBoolean(spaceWithinParens));
    }

    public boolean spaceWithinFunctionDeclParens() {
        return preferences.getBoolean(spaceWithinFunctionDeclParens, getDefaultAsBoolean(spaceWithinFunctionDeclParens));
    }

    public boolean spaceWithinFunctionCallParens() {
        return preferences.getBoolean(spaceWithinFunctionCallParens, getDefaultAsBoolean(spaceWithinFunctionCallParens));
    }

    public boolean spaceWithinIfParens() {
        return preferences.getBoolean(spaceWithinIfParens, getDefaultAsBoolean(spaceWithinIfParens));
    }

    public boolean spaceWithinForParens() {
        return preferences.getBoolean(spaceWithinForParens, getDefaultAsBoolean(spaceWithinForParens));
    }

    public boolean spaceWithinWhileParens() {
        return preferences.getBoolean(spaceWithinWhileParens, getDefaultAsBoolean(spaceWithinWhileParens));
    }

    public boolean spaceWithinCatchParens() {
        return preferences.getBoolean(spaceWithinCatchParens, getDefaultAsBoolean(spaceWithinCatchParens));
    }

    public boolean spaceWithinBraces() {
        return preferences.getBoolean(spaceWithinBraces, getDefaultAsBoolean(spaceWithinBraces));
    }

    public boolean spaceWithinArrayInitBrackets() {
        return preferences.getBoolean(spaceWithinSequenceInitBrackets, getDefaultAsBoolean(spaceWithinSequenceInitBrackets));
    }

    public boolean spaceBeforeComma() {
        return preferences.getBoolean(spaceBeforeComma, getDefaultAsBoolean(spaceBeforeComma));
    }

    public boolean spaceAfterComma() {
        return preferences.getBoolean(spaceAfterComma, getDefaultAsBoolean(spaceAfterComma));
    }

    public boolean spaceBeforeSemi() {
        return preferences.getBoolean(spaceBeforeSemi, getDefaultAsBoolean(spaceBeforeSemi));
    }

    public boolean spaceAfterSemi() {
        return preferences.getBoolean(spaceAfterSemi, getDefaultAsBoolean(spaceAfterSemi));
    }

    public boolean spaceBeforeColon() {
        return preferences.getBoolean(spaceBeforeColon, getDefaultAsBoolean(spaceBeforeColon));
    }

    public boolean spaceAfterColon() {
        return preferences.getBoolean(spaceAfterColon, getDefaultAsBoolean(spaceAfterColon));
    }

    // Imports -----------------------------------------------------------------

    public boolean useSingleClassImport() {
        return preferences.getBoolean(useSingleClassImport, getDefaultAsBoolean(useSingleClassImport));
    }

    public boolean useFQNs() {
        return preferences.getBoolean(useFQNs, getDefaultAsBoolean(useFQNs));
    }

    public int countForUsingStarImport() {
        return preferences.getInt(countForUsingStarImport, getDefaultAsInt(countForUsingStarImport));
    }

    public String[] getPackagesForStarImport() {
        return null;
    }

    // Nested classes ----------------------------------------------------------

    public enum BracePlacement {
        SAME_LINE,
        NEW_LINE,
        NEW_LINE_HALF_INDENTED,
        NEW_LINE_INDENTED
    }

    public enum BracesGenerationStyle {
        GENERATE,
        LEAVE_ALONE,
        ELIMINATE
    }
    
    public enum WrapStyle {
        WRAP_ALWAYS,
        WRAP_IF_LONG,
        WRAP_NEVER
    }
    
    // Communication with non public packages ----------------------------------
    
    private static class Producer implements FmtOptions.CodeStyleProducer {

        public CodeStyle create(Preferences preferences) {
            return new CodeStyle(preferences);
        }
        
    } 
    
}
