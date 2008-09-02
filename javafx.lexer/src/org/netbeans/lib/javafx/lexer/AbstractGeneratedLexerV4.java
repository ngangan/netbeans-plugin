/*
 * Copyright (c) 2008, Your Corporation. All Rights Reserved.
 */

package org.netbeans.lib.javafx.lexer;

import com.sun.tools.javac.util.Log;
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
/*
    public final BraceQuoteTracker NULL_BQT = new BraceQuoteTracker(null, '\'', false);
    private BraceQuoteTracker quoteStack = NULL_BQT;
*/
    /**
     * The log to be used for error diagnostics.
     */
/*
    protected Log log;
    private static Logger logger = Logger.getLogger(Lexer.class.getName());
    private List<Token> tokens = new ArrayList<Token>();
*/
    protected int previousTokenType;

/*
    public void emit(Token token) {
        state.token = token;
        tokens.add(token);
    }

    public Token nextToken() {
        if (tokens.size() > 0) {
            return tokens.remove(0);

        }
        super.nextToken();
        if (tokens.size() == 0) {
            emit(Token.EOF_TOKEN);
        }
        return tokens.remove(0);
    }


    */
/**
     * Set the complete text of this token; it wipes any previous
     * changes to the text.
     */
/*
    @Override
    public void setText(String text) {
//        super.setText(text);
    }

*/

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


/*    *//**
     * Gets 'braceQuoteTracker'.
     *
     * @return Value for property 'braceQuoteTracker'.
     *//*
    public BraceQuoteTracker getBraceQuoteTracker() {
        return quoteStack;
    }

    *//**
     * Sets 'braceQuoteTracker'.
     *
     * @param stack Value to set for property 'braceQuoteTracker'.
     *//*
    public void setBraceQuoteTracker(BraceQuoteTracker stack) {
        quoteStack = stack;
    }

    *//**
     * Gets 'sharedState'.
     *
     * @return Value for property 'sharedState'.
     *//*
    public RecognizerSharedState getSharedState() {
        return state;
    }



    *//**
     * Trying to recover from error by consuming all characters until one matches correct follow token or EOF.
     *
     * @param re Exception from which we try to recover.
     *//*
    @Override
    public void recover(RecognitionException re) {
        logger.severe(getErrorMessage(re, getTokenNames()) + " Trying to recover from error. " + re.getClass().getSimpleName());
        final BitSet bitSet = computeErrorRecoverySet();
        consumeUntil(input, bitSet);
        input.consume(); // consuming last character.
    }

    @Override
    public void reportError(RecognitionException e) {
        if (e instanceof FailedPredicateException) {
            if (logger.isLoggable(Level.WARNING))
                logger.warning(e.getClass().getSimpleName() + " found unexpected type "
                        + Integer.toString(e.getUnexpectedType()) + " trying to recover from buggy source code.");
            // we just skip this character. Maybe the next one will be OK. Preventing endless loop.
            input.consume();
        } else {
            super.reportError(e);
        }
    }

    *//**
     * Sets 'sharedState'.
     *
     * @param state Value to set for property 'sharedState'.
     *//*
    public void setSharedState(RecognizerSharedState state) {
        this.state = state;
    }

    *//**
     * Creates new {@link org.netbeans.lib.javafx.lexer.Lexer.BraceQuoteTracker} instance. This is
     * factory like method to deal with non static inner class.
     *
     * @param prev            previous stack entry (stack top)
     * @param quote           quote char.
     * @param percentIsFormat true is PercentIsFormat[int] rules matches.
     * @return new instance of {@link org.netbeans.lib.javafx.lexer.Lexer.BraceQuoteTracker} which represents actual stack top.
     *//*
    BraceQuoteTracker createBQT(BraceQuoteTracker prev, char quote, boolean percentIsFormat) {
        if (prev == null) return NULL_BQT;
        return new BraceQuoteTracker(prev, quote, percentIsFormat);
    }

    protected void processString() {
    }

    protected void processTranslationKey() {
    }

    protected void enterBrace(int quote, boolean nextIsPercent) {
        quoteStack.enterBrace(quote, nextIsPercent);
    }

    protected void leaveQuote() {
        quoteStack.leaveQuote();
    }

    protected boolean rightBraceLikeQuote(int quote) {
        return quoteStack.rightBraceLikeQuote(quote);
    }

    protected void leaveBrace() {
        quoteStack.leaveBrace();
    }

    protected boolean percentIsFormat() {
        return quoteStack.percentIsFormat();
    }

    protected void resetPercentIsFormat() {
        quoteStack.resetPercentIsFormat();
    }

    protected void checkIntLiteralRange(String text, int pos, int radix) {
//
//        long value = Convert.string2long(text, radix);
//
//        pos = pos - text.length();
//
//        if (previousTokenType == v3Lexer.SUB) {
//            value = -value;
//            if ( value < Integer.MIN_VALUE )
//                log.error(pos, MsgSym.MESSAGE_JAVAFX_LITERAL_OUT_OF_RANGE, "small", new String("-" + text));
//
//        } else if (value > Integer.MAX_VALUE) {
//            log.error(pos, MsgSym.MESSAGE_JAVAFX_LITERAL_OUT_OF_RANGE, "big", text);
//
//        }
    }

    protected void processFormatString() {

    }

    *//**
     * Track "He{"l{"l"}o"} world" quotes
     *//*
    *//*static*//* class BraceQuoteTracker {
//        private *//*static*//* Logger log = Logger.getLogger(Lexer.BraceQuoteTracker.class.getName());
        //        private BraceQuoteTracker quoteStack = null;
        private int braceDepth;
        private char quote;
        private boolean percentIsFormat;
        private BraceQuoteTracker next;

        public BraceQuoteTracker(BraceQuoteTracker prev, char quote, boolean percentIsFormat) {
            this.quote = quote;
            this.percentIsFormat = percentIsFormat;
            this.braceDepth = 1;
            this.next = prev;
        }

        *//*static*//* void enterBrace(int quote, boolean percentIsFormat) {
            if (quote == 0) {  // exisiting string expression or non string expression
                if (quoteStack != NULL_BQT) {
//                    if (log.isLoggable(Level.INFO)) log.info("+B");
                    ++quoteStack.braceDepth;
                    quoteStack.percentIsFormat = percentIsFormat;
                }
            } else {
                quoteStack = new BraceQuoteTracker(quoteStack, (char) quote, percentIsFormat); // push
//                if (log.isLoggable(Level.INFO)) log.info("+B PUSH => " + quoteStack);
            }
        }

        *//**
         * Return quote kind if we are reentering a quote
         *
         * @return return quote on stack.
         *//*
        *//*static*//* char leaveBrace() {
//            if (log.isLoggable(Level.INFO)) log.info("-B");
            if (quoteStack != NULL_BQT && --quoteStack.braceDepth == 0) {
                return quoteStack.quote;
            }
            return 0;
        }

        *//*static*//* boolean rightBraceLikeQuote(int quote) {
            final boolean b = quoteStack != NULL_BQT && quoteStack.braceDepth == 1 && (quote == 0 || quoteStack.quote == (char) quote);
//            if (log.isLoggable(Level.INFO)) log.info("rightBraceLikeQuote: " + b);
            return b;
        }

        *//*static*//* void leaveQuote() {
            assert (quoteStack != NULL_BQT && quoteStack.braceDepth == 0);
            quoteStack = quoteStack.next; // pop
//            if (log.isLoggable(Level.INFO)) log.info("+\" POP => " + quoteStack);
        }

        *//*static*//* boolean percentIsFormat() {
            return quoteStack != NULL_BQT && quoteStack.percentIsFormat;
        }

        *//*static*//* void resetPercentIsFormat() {
            quoteStack.percentIsFormat = false;
        }

        *//*static*//* boolean inBraceQuote() {
            final boolean b = quoteStack != NULL_BQT;
//            if (log.isLoggable(Level.INFO)) log.info("inBraceQuote: " + b);
            return b;
        }


        *//**
         * {@inheritDoc}
         *//*
        public String toString() {
            return "BQT[" +
                    "depth=" + braceDepth +
                    ", quote=" + Integer.toString(quote) +
                    ", pif=" + percentIsFormat +
                    ", next=" + next +
                    ']';
        }


        *//**
         * Gets 'braceDepth'.
         *
         * @return Value for property 'braceDepth'.
         *//*
        public int getBraceDepth() {
            return braceDepth;
        }

        *//**
         * Gets 'quote'.
         *
         * @return Value for property 'quote'.
         *//*
        public char getQuote() {
            return quote;
        }

        *//**
         * Gets 'percentIsFormat'.
         *
         * @return Value for property 'percentIsFormat'.
         *//*
        public boolean isPercentIsFormat() {
            return percentIsFormat;
        }

        *//**
         * Gets 'next'.
         *
         * @return Value for property 'next'.
         *//*
        public BraceQuoteTracker getNext() {
            return next;
        }

        *//**
         * Sets brace depth for actual record. Brace depth should be counted from current depth in stack. But. If there
         * are 2 similar braces in sequence only depht is increased, not number of levels.
         *
         * @param depth the number of quotes in line.
         *//*
        void setBraceDepth(int depth) {
            this.braceDepth = depth;
        }
    }*/
}