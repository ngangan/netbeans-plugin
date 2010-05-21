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
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */

package org.netbeans.lib.javafx.lexer;

import com.sun.tools.mjavac.util.Log;
import org.antlr.runtime.*;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Lexer base class provide user code for grammar. This code is called from antlr generated lexer. The main
 * purpose is to cover differences between javafxc lexer customizations and this module.
 *
 * @author Rastislav Komara (<a href="mailto:rastislav .komara@sun.com">RKo</a>)
 */
public abstract class AbstractGeneratedLexerV4 extends Lexer {
    protected int previousTokenType;

    /**
     * A
     * @param charStream a
     * @param recognizerSharedState a
     */
    protected AbstractGeneratedLexerV4(CharStream charStream, RecognizerSharedState recognizerSharedState) {
        super(charStream, recognizerSharedState);
    }

    protected AbstractGeneratedLexerV4() {
    }

    protected AbstractGeneratedLexerV4(CharStream input) {
        super(input);
    }

    protected void checkIntLiteralRange(String text, int pos, int radix) {
    }
}
