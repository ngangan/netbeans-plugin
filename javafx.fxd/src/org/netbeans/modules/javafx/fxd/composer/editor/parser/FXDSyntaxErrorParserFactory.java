/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.javafx.fxd.composer.editor.parser;

import java.util.Collection;
import org.netbeans.modules.parsing.api.Snapshot;
import org.netbeans.modules.parsing.spi.Parser;
import org.netbeans.modules.parsing.spi.ParserFactory;

/**
 *
 * @author avk
 */
public class FXDSyntaxErrorParserFactory extends ParserFactory {

    @Override
    public Parser createParser (Collection<Snapshot> snapshots) {
        return new FXDSyntaxErrorParser ();
    }
}
