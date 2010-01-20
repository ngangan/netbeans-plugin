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
package org.netbeans.modules.javafx.fxd.composer.editor;

import com.sun.javafx.tools.fxd.container.scene.fxd.FXDSyntaxErrorException;
import java.util.ArrayList;
import java.util.List;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.editor.indent.api.IndentUtils;
import org.netbeans.modules.javafx.fxd.composer.editor.parser.FXDSyntaxErrorParser.FXDParserResult;
import org.netbeans.modules.parsing.spi.Parser.Result;
import org.netbeans.modules.parsing.spi.ParserResultTask;
import org.netbeans.modules.parsing.spi.Scheduler;
import org.netbeans.modules.parsing.spi.SchedulerEvent;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.netbeans.spi.editor.hints.ErrorDescriptionFactory;
import org.netbeans.spi.editor.hints.HintsController;
import org.netbeans.spi.editor.hints.Severity;
import org.openide.util.Exceptions;


/**
 *
 * @author Andrey Korostelev
 */
public class SyntaxErrorsHighlightingTask extends ParserResultTask {

    public SyntaxErrorsHighlightingTask() {
    }

    @Override
    public void run(Result result, SchedulerEvent event) {
        try {
            FXDParserResult fxdResult = (FXDParserResult) result;
            List<FXDSyntaxErrorException> syntaxErrors = fxdResult.getSyntaxErrors();
            Document document = result.getSnapshot().getSource().getDocument(false);
            List<ErrorDescription> errors = new ArrayList<ErrorDescription>();
            for (FXDSyntaxErrorException syntaxError : syntaxErrors) {
                TokenSequence<?> ts = result.getSnapshot().getTokenHierarchy().tokenSequence();
                ts.move(syntaxError.getOffset());
                if (ts.moveNext()) {
                    int ErrRow = getRow(syntaxError, (BaseDocument)document);
                    int ErrPosition = getPosition(syntaxError, (BaseDocument)document);

                    Token token = ts.token();
                    int start = ts.offset();
                    int end = start + token.length();
                    ErrorDescription errorDescription = ErrorDescriptionFactory.createErrorDescription(
                            Severity.ERROR,
                            syntaxError.getMessage()+ " at ["+ErrRow+","+ErrPosition+"]",
                            document,
                            document.createPosition(start),
                            document.createPosition(end));
                    errors.add(errorDescription);
                }
            }
            if(document != null){
                HintsController.setErrors(document, "simple-java", errors);
            }
        } catch (BadLocationException ex1) {
            Exceptions.printStackTrace(ex1);
        } catch (org.netbeans.modules.parsing.spi.ParseException ex1) {
            Exceptions.printStackTrace (ex1);
        }
    }

    private int getRow(FXDSyntaxErrorException syntaxError, BaseDocument document)
            throws BadLocationException {
        return Utilities.getRowCount(document, 0, syntaxError.getOffset());
    }

    private int getPosition(FXDSyntaxErrorException syntaxError, BaseDocument document)
            throws BadLocationException {
        int offset = syntaxError.getOffset();
        int rowStart = Utilities.getRowStart((BaseDocument) document, offset);
        int position = offset - rowStart;
        int tabs = syntaxError.getTabsInLastRow();
        if (tabs > 0){
            // replace 1 tab char by number of spaces in tab
            position = position - tabs + ( tabs * IndentUtils.tabSize(document));
        }
        return position;
    }


    @Override
    public int getPriority() {
        return 100;
    }

    @Override
    public Class<? extends Scheduler> getSchedulerClass() {
        return Scheduler.EDITOR_SENSITIVE_TASK_SCHEDULER;
    }

    @Override
    public void cancel() {
    }
}
