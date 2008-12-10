/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
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
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.lib.javafx.lexer;

import org.netbeans.api.javafx.lexer.JFXStringTokenId;
import org.netbeans.api.lexer.Token;
import org.netbeans.spi.lexer.Lexer;
import org.netbeans.spi.lexer.LexerInput;
import org.netbeans.spi.lexer.LexerRestartInfo;
import org.netbeans.spi.lexer.TokenFactory;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Lexical analyzer for java string language.
 *
 * @author Miloslav Metelka
 * @version 1.00
 */

public class JFXStringLexer implements Lexer<JFXStringTokenId> {
    private static Logger log = Logger.getLogger(JFXStringLexer.class.getName());
    private static final int EOF = LexerInput.EOF;

    private LexerInput input;

    private TokenFactory<JFXStringTokenId> tokenFactory;
    private final boolean forceStringLiteralOnly;
    private boolean rl_slStartPossible = false; // represent possible start of RBRACE_LBRACE_STRING_LITERAL.

    public JFXStringLexer(LexerRestartInfo<JFXStringTokenId> info, boolean forceStringLiteralOnly) {
        this.forceStringLiteralOnly = forceStringLiteralOnly;
        this.input = info.input();
        this.tokenFactory = info.tokenFactory();
        assert (info.state() == null); // passed argument always null
    }

    public Object state() {
        return null;
    }

    public Token<JFXStringTokenId> nextToken() {
        while (true) {
            int ch = input.read();
            if (log.isLoggable(Level.FINE))
                log.fine("Reading character: " + (ch == EOF ? "<EOF>" : Character.toString((char) ch))); // NOI18N
            switch (ch) {
                case EOF:
                    if (input.readLength() > 0)
                        return token(JFXStringTokenId.TEXT);
                    else
                        return null;
                case '\\': //NOI18N
                    if (input.readLength() > 1) {// already read some text
                        input.backup(1);
                        return tokenFactory.createToken(JFXStringTokenId.TEXT, input.readLength());
                    }
                    switch (ch = input.read()) {
                        case '{': //NOI18N
                            return token(JFXStringTokenId.CODE_OPENING_BRACE_ESCAPE);
                        case '}': //NOI18N
                            return token(JFXStringTokenId.CODE_ENCLOSING_BRACE_ESCAPE);
                        case 'b': //NOI18N
                            return token(JFXStringTokenId.BACKSPACE);
                        case 'f': //NOI18N
                            return token(JFXStringTokenId.FORM_FEED);
                        case 'n': //NOI18N
                            return token(JFXStringTokenId.NEWLINE);
                        case 'r': //NOI18N
                            return token(JFXStringTokenId.CR);
                        case 't': //NOI18N
                            return token(JFXStringTokenId.TAB);
                        case '\'': //NOI18N
                            return token(JFXStringTokenId.SINGLE_QUOTE);
                        case '"': //NOI18N
                            return token(JFXStringTokenId.DOUBLE_QUOTE);
                        case '\\': //NOI18N
                            return token(JFXStringTokenId.BACKSLASH);
                        case 'u': //NOI18N
                            while ('u' == (ch = input.read())) { // NOI18N
                            }

                            for (int i = 0; ; i++) {
                                ch = Character.toLowerCase(ch);

                                if ((ch < '0' || ch > '9') && (ch < 'a' || ch > 'f')) { //NOI18N
                                    input.backup(1);
                                    return token(JFXStringTokenId.UNICODE_ESCAPE_INVALID);
                                }

                                if (i == 3) { // four digits checked, valid sequence
                                    return token(JFXStringTokenId.UNICODE_ESCAPE);
                                }

                                ch = input.read();
                            }

                        case '0': // NOI18N
                        case '1': // NOI18N
                        case '2': // NOI18N
                        case '3': // NOI18N
                            switch (input.read()) {
                                case '0': // NOI18N
                                case '1': // NOI18N
                                case '2': // NOI18N
                                case '3': // NOI18N
                                case '4': // NOI18N
                                case '5': // NOI18N
                                case '6': // NOI18N
                                case '7': //NOI18N
                                    switch (input.read()) {
                                        case '0': // NOI18N
                                        case '1': // NOI18N
                                        case '2': // NOI18N
                                        case '3': // NOI18N
                                        case '4': // NOI18N
                                        case '5': // NOI18N
                                        case '6': // NOI18N
                                        case '7': // NOI18N
                                            return token(JFXStringTokenId.OCTAL_ESCAPE);
                                    }
                                    input.backup(1);
                                    return token(JFXStringTokenId.OCTAL_ESCAPE_INVALID);
                            }
                            input.backup(1);
                            return token(JFXStringTokenId.OCTAL_ESCAPE_INVALID);
                    }
                    input.backup(1);
                    return token(JFXStringTokenId.ESCAPE_SEQUENCE_INVALID);
                case '{': // NOI18N
                    if (input.readLength() > 1 || forceStringLiteralOnly) {// already read some text
                        input.backup(1);
                        return tokenFactory.createToken(JFXStringTokenId.TEXT, input.readLength());
                    }
                    rl_slStartPossible = false;
                    return tokenFactory.createToken(JFXStringTokenId.CODE_OPENING_BRACE, input.readLength());

                case '}': // NOI18N
                    if (input.readLength() > 1 || forceStringLiteralOnly || rl_slStartPossible) {
                        return tokenFactory.createToken(JFXStringTokenId.TEXT, input.readLength());
                    } else {
                        //this character is code enclosing bracket only if it is first read character!
                        rl_slStartPossible = true;
                        return tokenFactory.createToken(JFXStringTokenId.CODE_ENCLOSING_BRACE, input.readLength());
                    }

            } // end of switch (ch)
        } // end of while(true)
    }

    private Token<JFXStringTokenId> token(JFXStringTokenId id) {
        return tokenFactory.createToken(id);
    }

    public void release() {
    }

}