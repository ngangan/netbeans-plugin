/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.javafx.fxd.composer.editor;

import com.sun.javafx.tools.fxd.container.scene.fxd.FXDSyntaxErrorException;
import java.util.ArrayList;
import java.util.List;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
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
 * @author avk
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
                    Token token = ts.token();
                    int start = ts.offset();
                    int end = start + token.length();
                    ErrorDescription errorDescription = ErrorDescriptionFactory.createErrorDescription(
                            Severity.ERROR,
                            syntaxError.getMessage(),
                            document,
                            document.createPosition(start),
                            document.createPosition(end));
                    errors.add(errorDescription);
                }
            }
            HintsController.setErrors(document, "simple-java", errors);
        } catch (BadLocationException ex1) {
            Exceptions.printStackTrace(ex1);
        } catch (org.netbeans.modules.parsing.spi.ParseException ex1) {
            Exceptions.printStackTrace (ex1);
        }
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
