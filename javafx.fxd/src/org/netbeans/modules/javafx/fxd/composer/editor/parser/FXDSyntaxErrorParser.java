/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.fxd.composer.editor.parser;

import com.sun.javafx.tools.fxd.container.scene.fxd.FXDSyntaxErrorException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.event.ChangeListener;
import org.netbeans.api.lexer.Token;
import org.netbeans.api.lexer.TokenSequence;
import org.netbeans.modules.javafx.fxd.composer.lexer.FXDTokenId;
import org.netbeans.modules.parsing.api.Snapshot;
import org.netbeans.modules.parsing.api.Task;
import org.netbeans.modules.parsing.spi.ParseException;
import org.netbeans.modules.parsing.spi.Parser;
import org.netbeans.modules.parsing.spi.Parser.Result;
import org.netbeans.modules.parsing.spi.SourceModificationEvent;

/**
 *
 * @author avk
 */
public class FXDSyntaxErrorParser extends Parser {

    private static Logger logger = Logger.getLogger( FXDSyntaxErrorParser.class.getName() ); // NOI18N
    private Snapshot snapshot;

    @Override
    public void parse (Snapshot snapshot, Task task, SourceModificationEvent event) {
        this.snapshot = snapshot;
        TokenSequence<?> ts = snapshot.getTokenHierarchy().tokenSequence();
    }

    @Override
    public Result getResult (Task task) {
        return new FXDParserResult (snapshot);
    }

    @Override
    public void cancel () {
    }

    @Override
    public void addChangeListener (ChangeListener changeListener) {
    }

    @Override
    public void removeChangeListener (ChangeListener changeListener) {
    }


    public static class FXDParserResult extends Result {

        private boolean valid = true;

        FXDParserResult (Snapshot snapshot) {
            super (snapshot);
            logger.warning(">>>>>>>>TS = "+snapshot.getTokenHierarchy().tokenSequence());
        }

        @Override
        protected void invalidate () {
            valid = false;
        }

        public List<FXDSyntaxErrorException> getSyntaxErrors() throws ParseException{
            if (!valid) throw new org.netbeans.modules.parsing.spi.ParseException ();
            
            List<FXDSyntaxErrorException> l = new ArrayList<FXDSyntaxErrorException>();

            TokenSequence<?> ts = getSnapshot().getTokenHierarchy().tokenSequence();
            while (ts.moveNext()){
                Token t = ts.token();
                if (FXDTokenId.UNKNOWN == t.id()){
                    if (t.hasProperties()){
                        l.add((FXDSyntaxErrorException)t.getProperty(FXDSyntaxErrorException.class));
                    }
                }
            }

            return l;
        }
    }
}
