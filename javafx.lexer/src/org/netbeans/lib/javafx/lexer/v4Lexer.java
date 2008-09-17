// $ANTLR 3.1 E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g 2008-09-02 14:34:43


package org.netbeans.lib.javafx.lexer;

import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.Convert;
import com.sun.tools.javac.util.Log;
import com.sun.tools.javafx.util.MsgSym;



import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
public class v4Lexer extends AbstractGeneratedLexerV4 {
    public static final int LAZY=39;
    public static final int COMMA=85;
    public static final int DEF=18;
    public static final int AS=7;
    public static final int NOTEQ=104;
    public static final int INTO=36;
    public static final int TranslationKeyBody=119;
    public static final int FALSE=23;
    public static final int ABSTRACT=4;
    public static final int THEN=64;
    public static final int STEP=62;
    public static final int PLUSPLUS=80;
    public static final int IMPORT=30;
    public static final int PACKAGE=48;
    public static final int SIZEOF=60;
    public static final int PIPE=79;
    public static final int CONTINUE=17;
    public static final int ON=45;
    public static final int DOT=86;
    public static final int SingleQuoteBody=110;
    public static final int PRIVATE=50;
    public static final int Letter=128;
    public static final int AND=6;
    public static final int FUNCTION=28;
    public static final int TRIGGER=67;
    public static final int STRING_LITERAL=111;
    public static final int RBRACKET=83;
    public static final int RPAREN=82;
    public static final int RBRACE_LBRACE_STRING_LITERAL=116;
    public static final int ASSERT=8;
    public static final int PLUS=94;
    public static final int FINALLY=24;
    public static final int EXTENDS=22;
    public static final int AT=9;
    public static final int PUBLIC_READABLE=54;
    public static final int TIME_LITERAL=121;
    public static final int SUPER=63;
    public static final int DECIMAL_LITERAL=122;
    public static final int WS=131;
    public static final int SUBSUB=108;
    public static final int NEW=41;
    public static final int PUBLIC_READ=55;
    public static final int EQ=88;
    public static final int EXCLUSIVE=21;
    public static final int LT=90;
    public static final int BOUND=13;
    public static final int LINE_COMMENT=134;
    public static final int EQEQ=87;
    public static final int QUOTE_LBRACE_STRING_LITERAL=113;
    public static final int FLOATING_POINT_LITERAL=127;
    public static final int CATCH=15;
    public static final int STATIC=61;
    public static final int SEMI=84;
    public static final int ELSE=20;
    public static final int INDEXOF=31;
    public static final int FORMAT_STRING_LITERAL=118;
    public static final int LTEQ=92;
    public static final int FIRST=25;
    public static final int BREAK=14;
    public static final int NULL=44;
    public static final int QUES=106;
    public static final int COLON=105;
    public static final int DOTDOT=81;
    public static final int IDENTIFIER=130;
    public static final int NextIsPercent=112;
    public static final int INSERT=34;
    public static final int TRUE=68;
    public static final int DOC_COMMENT=133;
    public static final int POUND=78;
    public static final int POSTINIT=49;
    public static final int THROW=66;
    public static final int WHERE=73;
    public static final int PUBLIC=53;
    public static final int LTGT=91;
    public static final int PERCENT=98;
    public static final int TYPEOF=71;
    public static final int LAST=38;
    public static final int LBRACKET=76;
    public static final int MOD=40;
    public static final int INIT=33;
    public static final int OCTAL_LITERAL=123;
    public static final int HEX_LITERAL=124;
    public static final int OR=46;
    public static final int LBRACE=114;
    public static final int AFTER=5;
    public static final int RBRACE=117;
    public static final int PROTECTED=51;
    public static final int INVERSE=37;
    public static final int SUBEQ=100;
    public static final int INSTANCEOF=35;
    public static final int TRANSLATION_KEY=120;
    public static final int LPAREN=77;
    public static final int DoubleQuoteBody=109;
    public static final int SLASHEQ=102;
    public static final int FROM=27;
    public static final int PERCENTEQ=103;
    public static final int DELETE=19;
    public static final int Exponent=126;
    public static final int SLASH=97;
    public static final int WHILE=74;
    public static final int STAREQ=101;
    public static final int READABLE=56;
    public static final int PLUSEQ=99;
    public static final int PUBLIC_INIT=52;
    public static final int REPLACE=57;
    public static final int GT=89;
    public static final int COMMENT=132;
    public static final int OVERRIDE=47;
    public static final int GTEQ=93;
    public static final int THIS=65;
    public static final int WITH=75;
    public static final int IN=32;
    public static final int REVERSE=59;
    public static final int INVALIDC=136;
    public static final int JavaIDDigit=129;
    public static final int VAR=72;
    public static final int CLASS=16;
    public static final int TWEEN=70;
    public static final int RETURN=58;
    public static final int IF=29;
    public static final int SUCHTHAT=107;
    public static final int EOF=-1;
    public static final int FOR=26;
    public static final int LAST_TOKEN=135;
    public static final int NON_WRITABLE=42;
    public static final int BEFORE=11;
    public static final int STAR=96;
    public static final int ATTRIBUTE=10;
    public static final int SUB=95;
    public static final int BIND=12;
    public static final int Digits=125;
    public static final int NOT=43;
    public static final int TRY=69;
    public static final int RBRACE_QUOTE_STRING_LITERAL=115;


        // Constructor that creates a message log sink for the 
        // current context.
        // 
        public v4Lexer(Context context, CharStream input) {
        	this(input);
            this.log = Log.instance(context);
        }

    	// Return the token type that we are using to indicate a 
    	// manufactured ';'.
    	// 
        protected int getSyntheticSemiType() {
            return SEMI;
        }

    // quote context --
        static final int CUR_QUOTE_CTX	= 0;	// 0 = use current quote context
        static final int SNG_QUOTE_CTX	= 1;	// 1 = single quote quote context
        static final int DBL_QUOTE_CTX	= 2;	// 2 = double quote quote context


    // delegates
    // delegators

    public v4Lexer() {;} 
    public v4Lexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public v4Lexer(CharStream input, RecognizerSharedState state) {
        super(input,state);

    }
    public String getGrammarFileName() { return "E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g"; }

    // $ANTLR start "ABSTRACT"
    public final void mABSTRACT() throws RecognitionException {
        try {
            int _type = ABSTRACT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:126:11: ( 'abstract' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:126:13: 'abstract'
            {
            match("abstract"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ABSTRACT"

    // $ANTLR start "AFTER"
    public final void mAFTER() throws RecognitionException {
        try {
            int _type = AFTER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:127:9: ( 'after' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:127:11: 'after'
            {
            match("after"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "AFTER"

    // $ANTLR start "AND"
    public final void mAND() throws RecognitionException {
        try {
            int _type = AND;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:128:8: ( 'and' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:128:10: 'and'
            {
            match("and"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "AND"

    // $ANTLR start "AS"
    public final void mAS() throws RecognitionException {
        try {
            int _type = AS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:129:7: ( 'as' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:129:9: 'as'
            {
            match("as"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "AS"

    // $ANTLR start "ASSERT"
    public final void mASSERT() throws RecognitionException {
        try {
            int _type = ASSERT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:130:10: ( 'assert' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:130:12: 'assert'
            {
            match("assert"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ASSERT"

    // $ANTLR start "AT"
    public final void mAT() throws RecognitionException {
        try {
            int _type = AT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:131:7: ( 'at' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:131:9: 'at'
            {
            match("at"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "AT"

    // $ANTLR start "ATTRIBUTE"
    public final void mATTRIBUTE() throws RecognitionException {
        try {
            int _type = ATTRIBUTE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:132:12: ( 'attribute' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:132:14: 'attribute'
            {
            match("attribute"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ATTRIBUTE"

    // $ANTLR start "BEFORE"
    public final void mBEFORE() throws RecognitionException {
        try {
            int _type = BEFORE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:133:10: ( 'before' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:133:12: 'before'
            {
            match("before"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "BEFORE"

    // $ANTLR start "BIND"
    public final void mBIND() throws RecognitionException {
        try {
            int _type = BIND;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:134:8: ( 'bind' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:134:10: 'bind'
            {
            match("bind"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "BIND"

    // $ANTLR start "BOUND"
    public final void mBOUND() throws RecognitionException {
        try {
            int _type = BOUND;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:135:9: ( 'bound' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:135:11: 'bound'
            {
            match("bound"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "BOUND"

    // $ANTLR start "BREAK"
    public final void mBREAK() throws RecognitionException {
        try {
            int _type = BREAK;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:136:9: ( 'break' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:136:11: 'break'
            {
            match("break"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "BREAK"

    // $ANTLR start "CATCH"
    public final void mCATCH() throws RecognitionException {
        try {
            int _type = CATCH;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:137:9: ( 'catch' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:137:11: 'catch'
            {
            match("catch"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "CATCH"

    // $ANTLR start "CLASS"
    public final void mCLASS() throws RecognitionException {
        try {
            int _type = CLASS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:138:9: ( 'class' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:138:11: 'class'
            {
            match("class"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "CLASS"

    // $ANTLR start "CONTINUE"
    public final void mCONTINUE() throws RecognitionException {
        try {
            int _type = CONTINUE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:139:11: ( 'continue' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:139:13: 'continue'
            {
            match("continue"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "CONTINUE"

    // $ANTLR start "DEF"
    public final void mDEF() throws RecognitionException {
        try {
            int _type = DEF;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:140:8: ( 'def' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:140:10: 'def'
            {
            match("def"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DEF"

    // $ANTLR start "DELETE"
    public final void mDELETE() throws RecognitionException {
        try {
            int _type = DELETE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:141:10: ( 'delete' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:141:12: 'delete'
            {
            match("delete"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DELETE"

    // $ANTLR start "ELSE"
    public final void mELSE() throws RecognitionException {
        try {
            int _type = ELSE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:142:8: ( 'else' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:142:10: 'else'
            {
            match("else"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ELSE"

    // $ANTLR start "EXCLUSIVE"
    public final void mEXCLUSIVE() throws RecognitionException {
        try {
            int _type = EXCLUSIVE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:143:12: ( 'exclusive' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:143:14: 'exclusive'
            {
            match("exclusive"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "EXCLUSIVE"

    // $ANTLR start "EXTENDS"
    public final void mEXTENDS() throws RecognitionException {
        try {
            int _type = EXTENDS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:144:11: ( 'extends' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:144:13: 'extends'
            {
            match("extends"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "EXTENDS"

    // $ANTLR start "FALSE"
    public final void mFALSE() throws RecognitionException {
        try {
            int _type = FALSE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:145:9: ( 'false' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:145:11: 'false'
            {
            match("false"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "FALSE"

    // $ANTLR start "FINALLY"
    public final void mFINALLY() throws RecognitionException {
        try {
            int _type = FINALLY;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:146:11: ( 'finally' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:146:13: 'finally'
            {
            match("finally"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "FINALLY"

    // $ANTLR start "FIRST"
    public final void mFIRST() throws RecognitionException {
        try {
            int _type = FIRST;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:147:9: ( 'first' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:147:11: 'first'
            {
            match("first"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "FIRST"

    // $ANTLR start "FOR"
    public final void mFOR() throws RecognitionException {
        try {
            int _type = FOR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:148:8: ( 'for' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:148:10: 'for'
            {
            match("for"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "FOR"

    // $ANTLR start "FROM"
    public final void mFROM() throws RecognitionException {
        try {
            int _type = FROM;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:149:8: ( 'from' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:149:10: 'from'
            {
            match("from"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "FROM"

    // $ANTLR start "FUNCTION"
    public final void mFUNCTION() throws RecognitionException {
        try {
            int _type = FUNCTION;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:150:11: ( 'function' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:150:13: 'function'
            {
            match("function"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "FUNCTION"

    // $ANTLR start "IF"
    public final void mIF() throws RecognitionException {
        try {
            int _type = IF;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:151:7: ( 'if' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:151:9: 'if'
            {
            match("if"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "IF"

    // $ANTLR start "IMPORT"
    public final void mIMPORT() throws RecognitionException {
        try {
            int _type = IMPORT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:152:10: ( 'import' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:152:12: 'import'
            {
            match("import"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "IMPORT"

    // $ANTLR start "INDEXOF"
    public final void mINDEXOF() throws RecognitionException {
        try {
            int _type = INDEXOF;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:153:11: ( 'indexof' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:153:13: 'indexof'
            {
            match("indexof"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "INDEXOF"

    // $ANTLR start "IN"
    public final void mIN() throws RecognitionException {
        try {
            int _type = IN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:154:7: ( 'in' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:154:9: 'in'
            {
            match("in"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "IN"

    // $ANTLR start "INIT"
    public final void mINIT() throws RecognitionException {
        try {
            int _type = INIT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:155:8: ( 'init' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:155:10: 'init'
            {
            match("init"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "INIT"

    // $ANTLR start "INSERT"
    public final void mINSERT() throws RecognitionException {
        try {
            int _type = INSERT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:156:10: ( 'insert' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:156:12: 'insert'
            {
            match("insert"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "INSERT"

    // $ANTLR start "INSTANCEOF"
    public final void mINSTANCEOF() throws RecognitionException {
        try {
            int _type = INSTANCEOF;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:157:13: ( 'instanceof' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:157:15: 'instanceof'
            {
            match("instanceof"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "INSTANCEOF"

    // $ANTLR start "INTO"
    public final void mINTO() throws RecognitionException {
        try {
            int _type = INTO;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:158:8: ( 'into' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:158:10: 'into'
            {
            match("into"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "INTO"

    // $ANTLR start "INVERSE"
    public final void mINVERSE() throws RecognitionException {
        try {
            int _type = INVERSE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:159:11: ( 'inverse' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:159:13: 'inverse'
            {
            match("inverse"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "INVERSE"

    // $ANTLR start "LAST"
    public final void mLAST() throws RecognitionException {
        try {
            int _type = LAST;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:160:8: ( 'last' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:160:10: 'last'
            {
            match("last"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LAST"

    // $ANTLR start "LAZY"
    public final void mLAZY() throws RecognitionException {
        try {
            int _type = LAZY;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:161:8: ( 'lazy' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:161:10: 'lazy'
            {
            match("lazy"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LAZY"

    // $ANTLR start "MOD"
    public final void mMOD() throws RecognitionException {
        try {
            int _type = MOD;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:162:8: ( 'mod' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:162:10: 'mod'
            {
            match("mod"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "MOD"

    // $ANTLR start "NEW"
    public final void mNEW() throws RecognitionException {
        try {
            int _type = NEW;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:163:8: ( 'new' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:163:10: 'new'
            {
            match("new"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NEW"

    // $ANTLR start "NON_WRITABLE"
    public final void mNON_WRITABLE() throws RecognitionException {
        try {
            int _type = NON_WRITABLE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:164:14: ( 'non-writable' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:164:16: 'non-writable'
            {
            match("non-writable"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NON_WRITABLE"

    // $ANTLR start "NOT"
    public final void mNOT() throws RecognitionException {
        try {
            int _type = NOT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:165:8: ( 'not' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:165:10: 'not'
            {
            match("not"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NOT"

    // $ANTLR start "NULL"
    public final void mNULL() throws RecognitionException {
        try {
            int _type = NULL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:166:8: ( 'null' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:166:10: 'null'
            {
            match("null"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NULL"

    // $ANTLR start "ON"
    public final void mON() throws RecognitionException {
        try {
            int _type = ON;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:167:7: ( 'on' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:167:9: 'on'
            {
            match("on"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ON"

    // $ANTLR start "OR"
    public final void mOR() throws RecognitionException {
        try {
            int _type = OR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:168:7: ( 'or' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:168:9: 'or'
            {
            match("or"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "OR"

    // $ANTLR start "OVERRIDE"
    public final void mOVERRIDE() throws RecognitionException {
        try {
            int _type = OVERRIDE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:169:11: ( 'override' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:169:13: 'override'
            {
            match("override"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "OVERRIDE"

    // $ANTLR start "PACKAGE"
    public final void mPACKAGE() throws RecognitionException {
        try {
            int _type = PACKAGE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:170:11: ( 'package' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:170:13: 'package'
            {
            match("package"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PACKAGE"

    // $ANTLR start "POSTINIT"
    public final void mPOSTINIT() throws RecognitionException {
        try {
            int _type = POSTINIT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:171:11: ( 'postinit' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:171:13: 'postinit'
            {
            match("postinit"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "POSTINIT"

    // $ANTLR start "PRIVATE"
    public final void mPRIVATE() throws RecognitionException {
        try {
            int _type = PRIVATE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:172:11: ( 'private' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:172:13: 'private'
            {
            match("private"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PRIVATE"

    // $ANTLR start "PROTECTED"
    public final void mPROTECTED() throws RecognitionException {
        try {
            int _type = PROTECTED;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:173:12: ( 'protected' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:173:14: 'protected'
            {
            match("protected"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PROTECTED"

    // $ANTLR start "PUBLIC_INIT"
    public final void mPUBLIC_INIT() throws RecognitionException {
        try {
            int _type = PUBLIC_INIT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:174:17: ( 'public-init' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:174:19: 'public-init'
            {
            match("public-init"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PUBLIC_INIT"

    // $ANTLR start "PUBLIC"
    public final void mPUBLIC() throws RecognitionException {
        try {
            int _type = PUBLIC;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:175:10: ( 'public' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:175:12: 'public'
            {
            match("public"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PUBLIC"

    // $ANTLR start "PUBLIC_READABLE"
    public final void mPUBLIC_READABLE() throws RecognitionException {
        try {
            int _type = PUBLIC_READABLE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:176:17: ( 'public-readable' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:176:19: 'public-readable'
            {
            match("public-readable"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PUBLIC_READABLE"

    // $ANTLR start "PUBLIC_READ"
    public final void mPUBLIC_READ() throws RecognitionException {
        try {
            int _type = PUBLIC_READ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:177:17: ( 'public-read' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:177:19: 'public-read'
            {
            match("public-read"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PUBLIC_READ"

    // $ANTLR start "READABLE"
    public final void mREADABLE() throws RecognitionException {
        try {
            int _type = READABLE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:178:11: ( 'readable' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:178:13: 'readable'
            {
            match("readable"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "READABLE"

    // $ANTLR start "REPLACE"
    public final void mREPLACE() throws RecognitionException {
        try {
            int _type = REPLACE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:179:11: ( 'replace' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:179:13: 'replace'
            {
            match("replace"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "REPLACE"

    // $ANTLR start "RETURN"
    public final void mRETURN() throws RecognitionException {
        try {
            int _type = RETURN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:180:10: ( 'return' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:180:12: 'return'
            {
            match("return"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RETURN"

    // $ANTLR start "REVERSE"
    public final void mREVERSE() throws RecognitionException {
        try {
            int _type = REVERSE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:181:11: ( 'reverse' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:181:13: 'reverse'
            {
            match("reverse"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "REVERSE"

    // $ANTLR start "SIZEOF"
    public final void mSIZEOF() throws RecognitionException {
        try {
            int _type = SIZEOF;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:182:10: ( 'sizeof' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:182:12: 'sizeof'
            {
            match("sizeof"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SIZEOF"

    // $ANTLR start "STATIC"
    public final void mSTATIC() throws RecognitionException {
        try {
            int _type = STATIC;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:183:10: ( 'static' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:183:12: 'static'
            {
            match("static"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "STATIC"

    // $ANTLR start "STEP"
    public final void mSTEP() throws RecognitionException {
        try {
            int _type = STEP;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:184:8: ( 'step' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:184:10: 'step'
            {
            match("step"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "STEP"

    // $ANTLR start "SUPER"
    public final void mSUPER() throws RecognitionException {
        try {
            int _type = SUPER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:185:9: ( 'super' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:185:11: 'super'
            {
            match("super"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SUPER"

    // $ANTLR start "THEN"
    public final void mTHEN() throws RecognitionException {
        try {
            int _type = THEN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:186:8: ( 'then' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:186:10: 'then'
            {
            match("then"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "THEN"

    // $ANTLR start "THIS"
    public final void mTHIS() throws RecognitionException {
        try {
            int _type = THIS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:187:8: ( 'this' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:187:10: 'this'
            {
            match("this"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "THIS"

    // $ANTLR start "THROW"
    public final void mTHROW() throws RecognitionException {
        try {
            int _type = THROW;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:188:9: ( 'throw' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:188:11: 'throw'
            {
            match("throw"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "THROW"

    // $ANTLR start "TRIGGER"
    public final void mTRIGGER() throws RecognitionException {
        try {
            int _type = TRIGGER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:189:11: ( 'trigger' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:189:13: 'trigger'
            {
            match("trigger"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "TRIGGER"

    // $ANTLR start "TRUE"
    public final void mTRUE() throws RecognitionException {
        try {
            int _type = TRUE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:190:8: ( 'true' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:190:10: 'true'
            {
            match("true"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "TRUE"

    // $ANTLR start "TRY"
    public final void mTRY() throws RecognitionException {
        try {
            int _type = TRY;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:191:8: ( 'try' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:191:10: 'try'
            {
            match("try"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "TRY"

    // $ANTLR start "TWEEN"
    public final void mTWEEN() throws RecognitionException {
        try {
            int _type = TWEEN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:192:9: ( 'tween' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:192:11: 'tween'
            {
            match("tween"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "TWEEN"

    // $ANTLR start "TYPEOF"
    public final void mTYPEOF() throws RecognitionException {
        try {
            int _type = TYPEOF;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:193:10: ( 'typeof' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:193:12: 'typeof'
            {
            match("typeof"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "TYPEOF"

    // $ANTLR start "VAR"
    public final void mVAR() throws RecognitionException {
        try {
            int _type = VAR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:194:8: ( 'var' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:194:10: 'var'
            {
            match("var"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "VAR"

    // $ANTLR start "WHERE"
    public final void mWHERE() throws RecognitionException {
        try {
            int _type = WHERE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:195:9: ( 'where' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:195:11: 'where'
            {
            match("where"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WHERE"

    // $ANTLR start "WHILE"
    public final void mWHILE() throws RecognitionException {
        try {
            int _type = WHILE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:196:9: ( 'while' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:196:11: 'while'
            {
            match("while"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WHILE"

    // $ANTLR start "WITH"
    public final void mWITH() throws RecognitionException {
        try {
            int _type = WITH;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:197:8: ( 'with' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:197:10: 'with'
            {
            match("with"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WITH"

    // $ANTLR start "LBRACKET"
    public final void mLBRACKET() throws RecognitionException {
        try {
            int _type = LBRACKET;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:202:10: ( '[' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:202:12: '['
            {
            match('['); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LBRACKET"

    // $ANTLR start "LPAREN"
    public final void mLPAREN() throws RecognitionException {
        try {
            int _type = LPAREN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:203:9: ( '(' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:203:11: '('
            {
            match('('); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LPAREN"

    // $ANTLR start "POUND"
    public final void mPOUND() throws RecognitionException {
        try {
            int _type = POUND;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:204:8: ( '#' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:204:10: '#'
            {
            match('#'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "POUND"

    // $ANTLR start "PIPE"
    public final void mPIPE() throws RecognitionException {
        try {
            int _type = PIPE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:205:7: ( '|' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:205:9: '|'
            {
            match('|'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PIPE"

    // $ANTLR start "PLUSPLUS"
    public final void mPLUSPLUS() throws RecognitionException {
        try {
            int _type = PLUSPLUS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:206:10: ( '++' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:206:12: '++'
            {
            match("++"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PLUSPLUS"

    // $ANTLR start "DOTDOT"
    public final void mDOTDOT() throws RecognitionException {
        try {
            int _type = DOTDOT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:207:9: ( '..' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:207:11: '..'
            {
            match(".."); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DOTDOT"

    // $ANTLR start "RPAREN"
    public final void mRPAREN() throws RecognitionException {
        try {
            int _type = RPAREN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:208:9: ( ')' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:208:11: ')'
            {
            match(')'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RPAREN"

    // $ANTLR start "RBRACKET"
    public final void mRBRACKET() throws RecognitionException {
        try {
            int _type = RBRACKET;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:209:10: ( ']' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:209:12: ']'
            {
            match(']'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RBRACKET"

    // $ANTLR start "SEMI"
    public final void mSEMI() throws RecognitionException {
        try {
            int _type = SEMI;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:210:7: ( ';' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:210:9: ';'
            {
            match(';'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SEMI"

    // $ANTLR start "COMMA"
    public final void mCOMMA() throws RecognitionException {
        try {
            int _type = COMMA;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:211:8: ( ',' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:211:10: ','
            {
            match(','); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "COMMA"

    // $ANTLR start "DOT"
    public final void mDOT() throws RecognitionException {
        try {
            int _type = DOT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:212:7: ( '.' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:212:9: '.'
            {
            match('.'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DOT"

    // $ANTLR start "EQEQ"
    public final void mEQEQ() throws RecognitionException {
        try {
            int _type = EQEQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:213:7: ( '==' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:213:9: '=='
            {
            match("=="); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "EQEQ"

    // $ANTLR start "EQ"
    public final void mEQ() throws RecognitionException {
        try {
            int _type = EQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:214:6: ( '=' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:214:8: '='
            {
            match('='); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "EQ"

    // $ANTLR start "GT"
    public final void mGT() throws RecognitionException {
        try {
            int _type = GT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:215:6: ( '>' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:215:8: '>'
            {
            match('>'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "GT"

    // $ANTLR start "LT"
    public final void mLT() throws RecognitionException {
        try {
            int _type = LT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:216:6: ( '<' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:216:8: '<'
            {
            match('<'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LT"

    // $ANTLR start "LTGT"
    public final void mLTGT() throws RecognitionException {
        try {
            int _type = LTGT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:217:7: ( '<>' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:217:9: '<>'
            {
            match("<>"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LTGT"

    // $ANTLR start "LTEQ"
    public final void mLTEQ() throws RecognitionException {
        try {
            int _type = LTEQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:218:7: ( '<=' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:218:9: '<='
            {
            match("<="); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LTEQ"

    // $ANTLR start "GTEQ"
    public final void mGTEQ() throws RecognitionException {
        try {
            int _type = GTEQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:219:7: ( '>=' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:219:9: '>='
            {
            match(">="); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "GTEQ"

    // $ANTLR start "PLUS"
    public final void mPLUS() throws RecognitionException {
        try {
            int _type = PLUS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:220:7: ( '+' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:220:9: '+'
            {
            match('+'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PLUS"

    // $ANTLR start "SUB"
    public final void mSUB() throws RecognitionException {
        try {
            int _type = SUB;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:221:7: ( '-' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:221:9: '-'
            {
            match('-'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SUB"

    // $ANTLR start "STAR"
    public final void mSTAR() throws RecognitionException {
        try {
            int _type = STAR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:222:7: ( '*' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:222:9: '*'
            {
            match('*'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "STAR"

    // $ANTLR start "SLASH"
    public final void mSLASH() throws RecognitionException {
        try {
            int _type = SLASH;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:223:8: ( '/' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:223:10: '/'
            {
            match('/'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SLASH"

    // $ANTLR start "PERCENT"
    public final void mPERCENT() throws RecognitionException {
        try {
            int _type = PERCENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:224:10: ( '%' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:224:12: '%'
            {
            match('%'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PERCENT"

    // $ANTLR start "PLUSEQ"
    public final void mPLUSEQ() throws RecognitionException {
        try {
            int _type = PLUSEQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:225:9: ( '+=' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:225:11: '+='
            {
            match("+="); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PLUSEQ"

    // $ANTLR start "SUBEQ"
    public final void mSUBEQ() throws RecognitionException {
        try {
            int _type = SUBEQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:226:8: ( '-=' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:226:10: '-='
            {
            match("-="); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SUBEQ"

    // $ANTLR start "STAREQ"
    public final void mSTAREQ() throws RecognitionException {
        try {
            int _type = STAREQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:227:9: ( '*=' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:227:11: '*='
            {
            match("*="); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "STAREQ"

    // $ANTLR start "SLASHEQ"
    public final void mSLASHEQ() throws RecognitionException {
        try {
            int _type = SLASHEQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:228:10: ( '/=' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:228:12: '/='
            {
            match("/="); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SLASHEQ"

    // $ANTLR start "PERCENTEQ"
    public final void mPERCENTEQ() throws RecognitionException {
        try {
            int _type = PERCENTEQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:229:11: ( '%=' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:229:13: '%='
            {
            match("%="); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PERCENTEQ"

    // $ANTLR start "NOTEQ"
    public final void mNOTEQ() throws RecognitionException {
        try {
            int _type = NOTEQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:230:8: ( '!=' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:230:10: '!='
            {
            match("!="); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NOTEQ"

    // $ANTLR start "COLON"
    public final void mCOLON() throws RecognitionException {
        try {
            int _type = COLON;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:231:8: ( ':' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:231:10: ':'
            {
            match(':'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "COLON"

    // $ANTLR start "QUES"
    public final void mQUES() throws RecognitionException {
        try {
            int _type = QUES;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:232:7: ( '?' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:232:9: '?'
            {
            match('?'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "QUES"

    // $ANTLR start "SUCHTHAT"
    public final void mSUCHTHAT() throws RecognitionException {
        try {
            int _type = SUCHTHAT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:233:10: ( '=>' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:233:12: '=>'
            {
            match("=>"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SUCHTHAT"

    // $ANTLR start "SUBSUB"
    public final void mSUBSUB() throws RecognitionException {
        try {
            int _type = SUBSUB;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:234:9: ( '--' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:234:11: '--'
            {
            match("--"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SUBSUB"

    // $ANTLR start "STRING_LITERAL"
    public final void mSTRING_LITERAL() throws RecognitionException {
        try {
            int _type = STRING_LITERAL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:247:2: ( '\"' DoubleQuoteBody '\"' | '\\'' SingleQuoteBody '\\'' )
            int alt1=2;
            int LA1_0 = input.LA(1);

            if ( (LA1_0=='\"') ) {
                alt1=1;
            }
            else if ( (LA1_0=='\'') ) {
                alt1=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                NoViableAltException nvae =
                    new NoViableAltException("", 1, 0, input);

                throw nvae;
            }
            switch (alt1) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:247:4: '\"' DoubleQuoteBody '\"'
                    {
                    match('\"'); if (state.failed) return ;
                    mDoubleQuoteBody(); if (state.failed) return ;
                    match('\"'); if (state.failed) return ;
                    if ( state.backtracking==0 ) {
                       processString(); 
                    }

                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:251:4: '\\'' SingleQuoteBody '\\''
                    {
                    match('\''); if (state.failed) return ;
                    mSingleQuoteBody(); if (state.failed) return ;
                    match('\''); if (state.failed) return ;
                    if ( state.backtracking==0 ) {
                       processString(); 
                    }

                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "STRING_LITERAL"

    // $ANTLR start "QUOTE_LBRACE_STRING_LITERAL"
    public final void mQUOTE_LBRACE_STRING_LITERAL() throws RecognitionException {
        try {
            int _type = QUOTE_LBRACE_STRING_LITERAL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:271:2: ( '\"' DoubleQuoteBody '{' NextIsPercent[DBL_QUOTE_CTX] | '\\'' SingleQuoteBody '{' NextIsPercent[SNG_QUOTE_CTX] )
            int alt2=2;
            int LA2_0 = input.LA(1);

            if ( (LA2_0=='\"') ) {
                alt2=1;
            }
            else if ( (LA2_0=='\'') ) {
                alt2=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                NoViableAltException nvae =
                    new NoViableAltException("", 2, 0, input);

                throw nvae;
            }
            switch (alt2) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:271:4: '\"' DoubleQuoteBody '{' NextIsPercent[DBL_QUOTE_CTX]
                    {
                    match('\"'); if (state.failed) return ;
                    mDoubleQuoteBody(); if (state.failed) return ;
                    match('{'); if (state.failed) return ;
                    if ( state.backtracking==0 ) {
                       processString(); 
                    }
                    mNextIsPercent(DBL_QUOTE_CTX); if (state.failed) return ;

                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:277:4: '\\'' SingleQuoteBody '{' NextIsPercent[SNG_QUOTE_CTX]
                    {
                    match('\''); if (state.failed) return ;
                    mSingleQuoteBody(); if (state.failed) return ;
                    match('{'); if (state.failed) return ;
                    if ( state.backtracking==0 ) {
                       processString(); 
                    }
                    mNextIsPercent(SNG_QUOTE_CTX); if (state.failed) return ;

                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "QUOTE_LBRACE_STRING_LITERAL"

    // $ANTLR start "LBRACE"
    public final void mLBRACE() throws RecognitionException {
        try {
            int _type = LBRACE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:292:2: ( '{' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:292:4: '{'
            {
            match('{'); if (state.failed) return ;
            if ( state.backtracking==0 ) {
               enterBrace(0, false); 
            }

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LBRACE"

    // $ANTLR start "RBRACE_QUOTE_STRING_LITERAL"
    public final void mRBRACE_QUOTE_STRING_LITERAL() throws RecognitionException {
        try {
            int _type = RBRACE_QUOTE_STRING_LITERAL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:305:2: ({...}? => '}' DoubleQuoteBody '\"' | {...}? => '}' SingleQuoteBody '\\'' )
            int alt3=2;
            alt3 = dfa3.predict(input);
            switch (alt3) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:305:4: {...}? => '}' DoubleQuoteBody '\"'
                    {
                    if ( !(( rightBraceLikeQuote(DBL_QUOTE_CTX) )) ) {
                        if (state.backtracking>0) {state.failed=true; return ;}
                        throw new FailedPredicateException(input, "RBRACE_QUOTE_STRING_LITERAL", " rightBraceLikeQuote(DBL_QUOTE_CTX) ");
                    }
                    match('}'); if (state.failed) return ;
                    mDoubleQuoteBody(); if (state.failed) return ;
                    match('\"'); if (state.failed) return ;
                    if ( state.backtracking==0 ) {
                       
                      		  			leaveBrace(); 
                      		         	leaveQuote(); 
                      		         	processString(); 
                      		       	
                    }

                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:315:4: {...}? => '}' SingleQuoteBody '\\''
                    {
                    if ( !(( rightBraceLikeQuote(SNG_QUOTE_CTX) )) ) {
                        if (state.backtracking>0) {state.failed=true; return ;}
                        throw new FailedPredicateException(input, "RBRACE_QUOTE_STRING_LITERAL", " rightBraceLikeQuote(SNG_QUOTE_CTX) ");
                    }
                    match('}'); if (state.failed) return ;
                    mSingleQuoteBody(); if (state.failed) return ;
                    match('\''); if (state.failed) return ;
                    if ( state.backtracking==0 ) {
                       
                      		  			leaveBrace(); 
                      		         	leaveQuote(); 
                      		         	processString(); 
                      		      	
                    }

                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RBRACE_QUOTE_STRING_LITERAL"

    // $ANTLR start "RBRACE_LBRACE_STRING_LITERAL"
    public final void mRBRACE_LBRACE_STRING_LITERAL() throws RecognitionException {
        try {
            int _type = RBRACE_LBRACE_STRING_LITERAL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:334:2: ({...}? => '}' DoubleQuoteBody '{' NextIsPercent[CUR_QUOTE_CTX] | {...}? => '}' SingleQuoteBody '{' NextIsPercent[CUR_QUOTE_CTX] )
            int alt4=2;
            alt4 = dfa4.predict(input);
            switch (alt4) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:334:4: {...}? => '}' DoubleQuoteBody '{' NextIsPercent[CUR_QUOTE_CTX]
                    {
                    if ( !(( rightBraceLikeQuote(DBL_QUOTE_CTX) )) ) {
                        if (state.backtracking>0) {state.failed=true; return ;}
                        throw new FailedPredicateException(input, "RBRACE_LBRACE_STRING_LITERAL", " rightBraceLikeQuote(DBL_QUOTE_CTX) ");
                    }
                    match('}'); if (state.failed) return ;
                    mDoubleQuoteBody(); if (state.failed) return ;
                    match('{'); if (state.failed) return ;
                    if ( state.backtracking==0 ) {
                       
                      		  			leaveBrace(); 
                      					processString(); 
                      		    	
                    }
                    mNextIsPercent(CUR_QUOTE_CTX); if (state.failed) return ;

                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:345:4: {...}? => '}' SingleQuoteBody '{' NextIsPercent[CUR_QUOTE_CTX]
                    {
                    if ( !(( rightBraceLikeQuote(SNG_QUOTE_CTX) )) ) {
                        if (state.backtracking>0) {state.failed=true; return ;}
                        throw new FailedPredicateException(input, "RBRACE_LBRACE_STRING_LITERAL", " rightBraceLikeQuote(SNG_QUOTE_CTX) ");
                    }
                    match('}'); if (state.failed) return ;
                    mSingleQuoteBody(); if (state.failed) return ;
                    match('{'); if (state.failed) return ;
                    if ( state.backtracking==0 ) {
                       
                      				  			leaveBrace(); 
                      				       		processString(); 
                      				       	
                    }
                    mNextIsPercent(CUR_QUOTE_CTX); if (state.failed) return ;

                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RBRACE_LBRACE_STRING_LITERAL"

    // $ANTLR start "RBRACE"
    public final void mRBRACE() throws RecognitionException {
        try {
            int _type = RBRACE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:365:2: ({...}? => '}' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:365:4: {...}? => '}'
            {
            if ( !(( !rightBraceLikeQuote(CUR_QUOTE_CTX) )) ) {
                if (state.backtracking>0) {state.failed=true; return ;}
                throw new FailedPredicateException(input, "RBRACE", " !rightBraceLikeQuote(CUR_QUOTE_CTX) ");
            }
            match('}'); if (state.failed) return ;
            if ( state.backtracking==0 ) {
               leaveBrace(); 
            }

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RBRACE"

    // $ANTLR start "DoubleQuoteBody"
    public final void mDoubleQuoteBody() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:375:2: ( (~ ( '{' | '\"' | '\\\\' ) | '\\\\' . )* )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:375:5: (~ ( '{' | '\"' | '\\\\' ) | '\\\\' . )*
            {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:375:5: (~ ( '{' | '\"' | '\\\\' ) | '\\\\' . )*
            loop5:
            do {
                int alt5=3;
                int LA5_0 = input.LA(1);

                if ( ((LA5_0>='\u0000' && LA5_0<='!')||(LA5_0>='#' && LA5_0<='[')||(LA5_0>=']' && LA5_0<='z')||(LA5_0>='|' && LA5_0<='\uFFFE')) ) {
                    alt5=1;
                }
                else if ( (LA5_0=='\\') ) {
                    alt5=2;
                }


                switch (alt5) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:375:6: ~ ( '{' | '\"' | '\\\\' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='!')||(input.LA(1)>='#' && input.LA(1)<='[')||(input.LA(1)>=']' && input.LA(1)<='z')||(input.LA(1)>='|' && input.LA(1)<='\uFFFE') ) {
            	        input.consume();
            	    state.failed=false;
            	    }
            	    else {
            	        if (state.backtracking>0) {state.failed=true; return ;}
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;
            	case 2 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:375:23: '\\\\' .
            	    {
            	    match('\\'); if (state.failed) return ;
            	    matchAny(); if (state.failed) return ;

            	    }
            	    break;

            	default :
            	    break loop5;
                }
            } while (true);


            }

        }
        finally {
        }
    }
    // $ANTLR end "DoubleQuoteBody"

    // $ANTLR start "SingleQuoteBody"
    public final void mSingleQuoteBody() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:382:2: ( (~ ( '{' | '\\'' | '\\\\' ) | '\\\\' . )* )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:382:5: (~ ( '{' | '\\'' | '\\\\' ) | '\\\\' . )*
            {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:382:5: (~ ( '{' | '\\'' | '\\\\' ) | '\\\\' . )*
            loop6:
            do {
                int alt6=3;
                int LA6_0 = input.LA(1);

                if ( ((LA6_0>='\u0000' && LA6_0<='&')||(LA6_0>='(' && LA6_0<='[')||(LA6_0>=']' && LA6_0<='z')||(LA6_0>='|' && LA6_0<='\uFFFE')) ) {
                    alt6=1;
                }
                else if ( (LA6_0=='\\') ) {
                    alt6=2;
                }


                switch (alt6) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:382:6: ~ ( '{' | '\\'' | '\\\\' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='&')||(input.LA(1)>='(' && input.LA(1)<='[')||(input.LA(1)>=']' && input.LA(1)<='z')||(input.LA(1)>='|' && input.LA(1)<='\uFFFE') ) {
            	        input.consume();
            	    state.failed=false;
            	    }
            	    else {
            	        if (state.backtracking>0) {state.failed=true; return ;}
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;
            	case 2 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:382:24: '\\\\' .
            	    {
            	    match('\\'); if (state.failed) return ;
            	    matchAny(); if (state.failed) return ;

            	    }
            	    break;

            	default :
            	    break loop6;
                }
            } while (true);


            }

        }
        finally {
        }
    }
    // $ANTLR end "SingleQuoteBody"

    // $ANTLR start "NextIsPercent"
    public final void mNextIsPercent(int quoteContext) throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:393:2: ( ( ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )* '%' )=> | )
            int alt7=2;
            int LA7_0 = input.LA(1);

            if ( (synpred1_v4Lexer()) ) {
                alt7=1;
            }
            else if ( (true) ) {
                alt7=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                NoViableAltException nvae =
                    new NoViableAltException("", 7, 0, input);

                throw nvae;
            }
            switch (alt7) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:393:4: ( ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )* '%' )=>
                    {
                    if ( state.backtracking==0 ) {
                       enterBrace(quoteContext, true); 
                    }

                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:397:5: 
                    {
                    if ( state.backtracking==0 ) {
                       enterBrace(quoteContext, false); 
                    }

                    }
                    break;

            }
        }
        finally {
        }
    }
    // $ANTLR end "NextIsPercent"

    // $ANTLR start "FORMAT_STRING_LITERAL"
    public final void mFORMAT_STRING_LITERAL() throws RecognitionException {
        try {
            int _type = FORMAT_STRING_LITERAL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:407:2: ({...}? => '%' (~ ' ' )* )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:407:5: {...}? => '%' (~ ' ' )*
            {
            if ( !(( percentIsFormat() )) ) {
                if (state.backtracking>0) {state.failed=true; return ;}
                throw new FailedPredicateException(input, "FORMAT_STRING_LITERAL", " percentIsFormat() ");
            }
            match('%'); if (state.failed) return ;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:409:8: (~ ' ' )*
            loop8:
            do {
                int alt8=2;
                int LA8_0 = input.LA(1);

                if ( ((LA8_0>='\u0000' && LA8_0<='\u001F')||(LA8_0>='!' && LA8_0<='\uFFFE')) ) {
                    alt8=1;
                }


                switch (alt8) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:409:9: ~ ' '
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\u001F')||(input.LA(1)>='!' && input.LA(1)<='\uFFFE') ) {
            	        input.consume();
            	    state.failed=false;
            	    }
            	    else {
            	        if (state.backtracking>0) {state.failed=true; return ;}
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop8;
                }
            } while (true);

            if ( state.backtracking==0 ) {
               
              					processFormatString();
              	 				resetPercentIsFormat(); 
              	 			
            }

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "FORMAT_STRING_LITERAL"

    // $ANTLR start "TRANSLATION_KEY"
    public final void mTRANSLATION_KEY() throws RecognitionException {
        try {
            int _type = TRANSLATION_KEY;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:422:2: ( '##' ( '[' TranslationKeyBody ']' )? )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:422:4: '##' ( '[' TranslationKeyBody ']' )?
            {
            match("##"); if (state.failed) return ;

            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:423:6: ( '[' TranslationKeyBody ']' )?
            int alt9=2;
            int LA9_0 = input.LA(1);

            if ( (LA9_0=='[') ) {
                alt9=1;
            }
            switch (alt9) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:424:10: '[' TranslationKeyBody ']'
                    {
                    match('['); if (state.failed) return ;
                    mTranslationKeyBody(); if (state.failed) return ;
                    match(']'); if (state.failed) return ;

                    }
                    break;

            }

            if ( state.backtracking==0 ) {
               
                      	processTranslationKey(); 
                      
            }

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "TRANSLATION_KEY"

    // $ANTLR start "TranslationKeyBody"
    public final void mTranslationKeyBody() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:436:2: ( (~ ( '[' | ']' | '\\\\' ) | '\\\\' . )+ )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:436:4: (~ ( '[' | ']' | '\\\\' ) | '\\\\' . )+
            {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:436:4: (~ ( '[' | ']' | '\\\\' ) | '\\\\' . )+
            int cnt10=0;
            loop10:
            do {
                int alt10=3;
                int LA10_0 = input.LA(1);

                if ( ((LA10_0>='\u0000' && LA10_0<='Z')||(LA10_0>='^' && LA10_0<='\uFFFE')) ) {
                    alt10=1;
                }
                else if ( (LA10_0=='\\') ) {
                    alt10=2;
                }


                switch (alt10) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:436:5: ~ ( '[' | ']' | '\\\\' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='Z')||(input.LA(1)>='^' && input.LA(1)<='\uFFFE') ) {
            	        input.consume();
            	    state.failed=false;
            	    }
            	    else {
            	        if (state.backtracking>0) {state.failed=true; return ;}
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;
            	case 2 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:436:25: '\\\\' .
            	    {
            	    match('\\'); if (state.failed) return ;
            	    matchAny(); if (state.failed) return ;

            	    }
            	    break;

            	default :
            	    if ( cnt10 >= 1 ) break loop10;
            	    if (state.backtracking>0) {state.failed=true; return ;}
                        EarlyExitException eee =
                            new EarlyExitException(10, input);
                        throw eee;
                }
                cnt10++;
            } while (true);


            }

        }
        finally {
        }
    }
    // $ANTLR end "TranslationKeyBody"

    // $ANTLR start "TIME_LITERAL"
    public final void mTIME_LITERAL() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:454:25: ()
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:454:28: 
            {
            }

        }
        finally {
        }
    }
    // $ANTLR end "TIME_LITERAL"

    // $ANTLR start "DECIMAL_LITERAL"
    public final void mDECIMAL_LITERAL() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:462:27: ()
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:462:29: 
            {
            }

        }
        finally {
        }
    }
    // $ANTLR end "DECIMAL_LITERAL"

    // $ANTLR start "OCTAL_LITERAL"
    public final void mOCTAL_LITERAL() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:467:26: ()
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:467:29: 
            {
            }

        }
        finally {
        }
    }
    // $ANTLR end "OCTAL_LITERAL"

    // $ANTLR start "HEX_LITERAL"
    public final void mHEX_LITERAL() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:475:24: ()
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:475:26: 
            {
            }

        }
        finally {
        }
    }
    // $ANTLR end "HEX_LITERAL"

    // $ANTLR start "FLOATING_POINT_LITERAL"
    public final void mFLOATING_POINT_LITERAL() throws RecognitionException {
        try {
            int _type = FLOATING_POINT_LITERAL;
            int _channel = DEFAULT_TOKEN_CHANNEL;

            	// Indicates out of range digit
            	//
            	boolean rangeError = false;
            	

            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:499:5: ( '0' ( ( 'x' | 'X' ) ( ( ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ) | ( 'g' .. 'z' | 'G' .. 'Z' ) )+ ({...}? => '.' ( Digits )? | ) | ) | ( '0' .. '7' | '8' .. '9' )+ ({...}? => '.' ( Digits )? | ) | ( 'm' ( 's' )? | 's' | 'h' ) | {...}? => '.' ( Digits ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | ) | ) | ) | ( '1' .. '9' ) ( Digits )? ({...}? => '.' ( Digits )? ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | ) | ( ( 'm' ( 's' )? | 's' | 'h' ) | Exponent | ) ) | '.' ( Digits ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | ) | '.' | ) )
            int alt41=3;
            switch ( input.LA(1) ) {
            case '0':
                {
                alt41=1;
                }
                break;
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                {
                alt41=2;
                }
                break;
            case '.':
                {
                alt41=3;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return ;}
                NoViableAltException nvae =
                    new NoViableAltException("", 41, 0, input);

                throw nvae;
            }

            switch (alt41) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:505:6: '0' ( ( 'x' | 'X' ) ( ( ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ) | ( 'g' .. 'z' | 'G' .. 'Z' ) )+ ({...}? => '.' ( Digits )? | ) | ) | ( '0' .. '7' | '8' .. '9' )+ ({...}? => '.' ( Digits )? | ) | ( 'm' ( 's' )? | 's' | 'h' ) | {...}? => '.' ( Digits ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | ) | ) | )
                    {
                    match('0'); if (state.failed) return ;
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:506:7: ( ( 'x' | 'X' ) ( ( ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ) | ( 'g' .. 'z' | 'G' .. 'Z' ) )+ ({...}? => '.' ( Digits )? | ) | ) | ( '0' .. '7' | '8' .. '9' )+ ({...}? => '.' ( Digits )? | ) | ( 'm' ( 's' )? | 's' | 'h' ) | {...}? => '.' ( Digits ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | ) | ) | )
                    int alt25=5;
                    int LA25_0 = input.LA(1);

                    if ( (LA25_0=='X'||LA25_0=='x') ) {
                        alt25=1;
                    }
                    else if ( ((LA25_0>='0' && LA25_0<='9')) ) {
                        alt25=2;
                    }
                    else if ( (LA25_0=='h'||LA25_0=='m'||LA25_0=='s') ) {
                        alt25=3;
                    }
                    else if ( (LA25_0=='.') && (( input.LA(2) != '.'))) {
                        alt25=4;
                    }
                    else {
                        alt25=5;}
                    switch (alt25) {
                        case 1 :
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:507:10: ( 'x' | 'X' ) ( ( ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ) | ( 'g' .. 'z' | 'G' .. 'Z' ) )+ ({...}? => '.' ( Digits )? | ) | )
                            {
                            if ( input.LA(1)=='X'||input.LA(1)=='x' ) {
                                input.consume();
                            state.failed=false;
                            }
                            else {
                                if (state.backtracking>0) {state.failed=true; return ;}
                                MismatchedSetException mse = new MismatchedSetException(null,input);
                                recover(mse);
                                throw mse;}

                            if ( state.backtracking==0 ) {

                                  			  	// Always set the type, so the parser is not confused
                                  			  	//
                                  			  	_type = HEX_LITERAL;
                                  			  
                            }
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:514:10: ( ( ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ) | ( 'g' .. 'z' | 'G' .. 'Z' ) )+ ({...}? => '.' ( Digits )? | ) | )
                            int alt14=2;
                            int LA14_0 = input.LA(1);

                            if ( ((LA14_0>='0' && LA14_0<='9')||(LA14_0>='A' && LA14_0<='Z')||(LA14_0>='a' && LA14_0<='z')) ) {
                                alt14=1;
                            }
                            else {
                                alt14=2;}
                            switch (alt14) {
                                case 1 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:517:13: ( ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ) | ( 'g' .. 'z' | 'G' .. 'Z' ) )+ ({...}? => '.' ( Digits )? | )
                                    {
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:517:13: ( ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ) | ( 'g' .. 'z' | 'G' .. 'Z' ) )+
                                    int cnt11=0;
                                    loop11:
                                    do {
                                        int alt11=3;
                                        int LA11_0 = input.LA(1);

                                        if ( ((LA11_0>='0' && LA11_0<='9')||(LA11_0>='A' && LA11_0<='F')||(LA11_0>='a' && LA11_0<='f')) ) {
                                            alt11=1;
                                        }
                                        else if ( ((LA11_0>='G' && LA11_0<='Z')||(LA11_0>='g' && LA11_0<='z')) ) {
                                            alt11=2;
                                        }


                                        switch (alt11) {
                                    	case 1 :
                                    	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:518:16: ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' )
                                    	    {
                                    	    if ( (input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='A' && input.LA(1)<='F')||(input.LA(1)>='a' && input.LA(1)<='f') ) {
                                    	        input.consume();
                                    	    state.failed=false;
                                    	    }
                                    	    else {
                                    	        if (state.backtracking>0) {state.failed=true; return ;}
                                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                                    	        recover(mse);
                                    	        throw mse;}


                                    	    }
                                    	    break;
                                    	case 2 :
                                    	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:519:16: ( 'g' .. 'z' | 'G' .. 'Z' )
                                    	    {
                                    	    if ( (input.LA(1)>='G' && input.LA(1)<='Z')||(input.LA(1)>='g' && input.LA(1)<='z') ) {
                                    	        input.consume();
                                    	    state.failed=false;
                                    	    }
                                    	    else {
                                    	        if (state.backtracking>0) {state.failed=true; return ;}
                                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                                    	        recover(mse);
                                    	        throw mse;}

                                    	    if ( state.backtracking==0 ) {

                                    	          			  	  			rangeError = true;	// Signal at least one bad digit
                                    	          			  	  		
                                    	    }

                                    	    }
                                    	    break;

                                    	default :
                                    	    if ( cnt11 >= 1 ) break loop11;
                                    	    if (state.backtracking>0) {state.failed=true; return ;}
                                                EarlyExitException eee =
                                                    new EarlyExitException(11, input);
                                                throw eee;
                                        }
                                        cnt11++;
                                    } while (true);

                                    if ( state.backtracking==0 ) {

                                          			  	  		setText(getText().substring(2, getText().length()));
                                          			  	  		if	(rangeError)
                                          			  	  		{
                                          			  	  			// Error - malformed hex constant
                                          			  	  			//
                                          			  	  			log.error(getCharIndex()-1, MsgSym.MESSAGE_JAVAFX_HEX_MALFORMED);
                                          			  	  		}
                                          			  	  		else
                                          			  	  		{
                                      								checkIntLiteralRange(getText(), getCharIndex(), 16); 
                                      							}
                                          			  	  
                                    }
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:540:13: ({...}? => '.' ( Digits )? | )
                                    int alt13=2;
                                    int LA13_0 = input.LA(1);

                                    if ( (LA13_0=='.') && (( input.LA(2) != '.'))) {
                                        alt13=1;
                                    }
                                    else {
                                        alt13=2;}
                                    switch (alt13) {
                                        case 1 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:546:16: {...}? => '.' ( Digits )?
                                            {
                                            if ( !(( input.LA(2) != '.')) ) {
                                                if (state.backtracking>0) {state.failed=true; return ;}
                                                throw new FailedPredicateException(input, "FLOATING_POINT_LITERAL", " input.LA(2) != '.'");
                                            }
                                            match('.'); if (state.failed) return ;
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:546:45: ( Digits )?
                                            int alt12=2;
                                            int LA12_0 = input.LA(1);

                                            if ( ((LA12_0>='0' && LA12_0<='9')) ) {
                                                alt12=1;
                                            }
                                            switch (alt12) {
                                                case 1 :
                                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:546:45: Digits
                                                    {
                                                    mDigits(); if (state.failed) return ;

                                                    }
                                                    break;

                                            }

                                            if ( state.backtracking==0 ) {
                                               
                                                  			  	  					// Error - malformed hex constant
                                                  			  	  					//
                                                  			  	  					log.error(getCharIndex()-1, MsgSym.MESSAGE_JAVAFX_HEX_FLOAT);
                                                  			  	  				
                                            }

                                            }
                                            break;
                                        case 2 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:555:13: 
                                            {
                                            }
                                            break;

                                    }


                                    }
                                    break;
                                case 2 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:559:12: 
                                    {
                                    if ( state.backtracking==0 ) {

                                          			  			log.error(getCharIndex()-1, MsgSym.MESSAGE_JAVAFX_HEX_MISSING);
                                          			  		
                                    }

                                    }
                                    break;

                            }


                            }
                            break;
                        case 2 :
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:571:9: ( '0' .. '7' | '8' .. '9' )+ ({...}? => '.' ( Digits )? | )
                            {
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:571:9: ( '0' .. '7' | '8' .. '9' )+
                            int cnt15=0;
                            loop15:
                            do {
                                int alt15=3;
                                int LA15_0 = input.LA(1);

                                if ( ((LA15_0>='0' && LA15_0<='7')) ) {
                                    alt15=1;
                                }
                                else if ( ((LA15_0>='8' && LA15_0<='9')) ) {
                                    alt15=2;
                                }


                                switch (alt15) {
                            	case 1 :
                            	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:572:12: '0' .. '7'
                            	    {
                            	    matchRange('0','7'); if (state.failed) return ;

                            	    }
                            	    break;
                            	case 2 :
                            	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:574:12: '8' .. '9'
                            	    {
                            	    matchRange('8','9'); if (state.failed) return ;
                            	    if ( state.backtracking==0 ) {
                            	       
                            	          							rangeError = true; // Signal that at least one digit was wrong
                            	          						
                            	    }

                            	    }
                            	    break;

                            	default :
                            	    if ( cnt15 >= 1 ) break loop15;
                            	    if (state.backtracking>0) {state.failed=true; return ;}
                                        EarlyExitException eee =
                                            new EarlyExitException(15, input);
                                        throw eee;
                                }
                                cnt15++;
                            } while (true);

                            if ( state.backtracking==0 ) {

                                  					// Always set the type to octal, so the parser does not see
                                  					// a lexing error, even though the compiler knows there is an
                                  					// error.
                                  					//
                                  					_type = OCTAL_LITERAL;
                                  					
                                  					if	(rangeError)
                                  					{
                                  						log.error(getCharIndex()-1, MsgSym.MESSAGE_JAVAFX_OCTAL_MALFORMED);
                                  					}
                                  					else
                                  					{
                                  						checkIntLiteralRange(getText(), getCharIndex(), 8); 
                                  					}
                                  				
                            }
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:597:10: ({...}? => '.' ( Digits )? | )
                            int alt17=2;
                            int LA17_0 = input.LA(1);

                            if ( (LA17_0=='.') && (( input.LA(2) != '.'))) {
                                alt17=1;
                            }
                            else {
                                alt17=2;}
                            switch (alt17) {
                                case 1 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:601:15: {...}? => '.' ( Digits )?
                                    {
                                    if ( !(( input.LA(2) != '.')) ) {
                                        if (state.backtracking>0) {state.failed=true; return ;}
                                        throw new FailedPredicateException(input, "FLOATING_POINT_LITERAL", " input.LA(2) != '.'");
                                    }
                                    match('.'); if (state.failed) return ;
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:601:44: ( Digits )?
                                    int alt16=2;
                                    int LA16_0 = input.LA(1);

                                    if ( ((LA16_0>='0' && LA16_0<='9')) ) {
                                        alt16=1;
                                    }
                                    switch (alt16) {
                                        case 1 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:601:44: Digits
                                            {
                                            mDigits(); if (state.failed) return ;

                                            }
                                            break;

                                    }

                                    if ( state.backtracking==0 ) {
                                       
                                          			  	  				log.error(getCharIndex()-1, MsgSym.MESSAGE_JAVAFX_OCTAL_FLOAT);
                                          			  	  			
                                    }

                                    }
                                    break;
                                case 2 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:607:13: 
                                    {
                                    }
                                    break;

                            }


                            }
                            break;
                        case 3 :
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:612:9: ( 'm' ( 's' )? | 's' | 'h' )
                            {
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:612:9: ( 'm' ( 's' )? | 's' | 'h' )
                            int alt19=3;
                            switch ( input.LA(1) ) {
                            case 'm':
                                {
                                alt19=1;
                                }
                                break;
                            case 's':
                                {
                                alt19=2;
                                }
                                break;
                            case 'h':
                                {
                                alt19=3;
                                }
                                break;
                            default:
                                if (state.backtracking>0) {state.failed=true; return ;}
                                NoViableAltException nvae =
                                    new NoViableAltException("", 19, 0, input);

                                throw nvae;
                            }

                            switch (alt19) {
                                case 1 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:612:10: 'm' ( 's' )?
                                    {
                                    match('m'); if (state.failed) return ;
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:612:14: ( 's' )?
                                    int alt18=2;
                                    int LA18_0 = input.LA(1);

                                    if ( (LA18_0=='s') ) {
                                        alt18=1;
                                    }
                                    switch (alt18) {
                                        case 1 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:612:14: 's'
                                            {
                                            match('s'); if (state.failed) return ;

                                            }
                                            break;

                                    }


                                    }
                                    break;
                                case 2 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:612:21: 's'
                                    {
                                    match('s'); if (state.failed) return ;

                                    }
                                    break;
                                case 3 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:612:27: 'h'
                                    {
                                    match('h'); if (state.failed) return ;

                                    }
                                    break;

                            }

                            if ( state.backtracking==0 ) {
                               _type = TIME_LITERAL; 
                            }

                            }
                            break;
                        case 4 :
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:619:9: {...}? => '.' ( Digits ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | ) | )
                            {
                            if ( !(( input.LA(2) != '.')) ) {
                                if (state.backtracking>0) {state.failed=true; return ;}
                                throw new FailedPredicateException(input, "FLOATING_POINT_LITERAL", " input.LA(2) != '.'");
                            }
                            match('.'); if (state.failed) return ;
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:620:10: ( Digits ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | ) | )
                            int alt24=2;
                            int LA24_0 = input.LA(1);

                            if ( ((LA24_0>='0' && LA24_0<='9')) ) {
                                alt24=1;
                            }
                            else {
                                alt24=2;}
                            switch (alt24) {
                                case 1 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:623:13: Digits ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | )
                                    {
                                    mDigits(); if (state.failed) return ;
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:623:20: ( Exponent )?
                                    int alt20=2;
                                    int LA20_0 = input.LA(1);

                                    if ( (LA20_0=='E'||LA20_0=='e') ) {
                                        alt20=1;
                                    }
                                    switch (alt20) {
                                        case 1 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:623:20: Exponent
                                            {
                                            mExponent(); if (state.failed) return ;

                                            }
                                            break;

                                    }

                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:625:14: ( ( 'm' ( 's' )? | 's' | 'h' ) | )
                                    int alt23=2;
                                    int LA23_0 = input.LA(1);

                                    if ( (LA23_0=='h'||LA23_0=='m'||LA23_0=='s') ) {
                                        alt23=1;
                                    }
                                    else {
                                        alt23=2;}
                                    switch (alt23) {
                                        case 1 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:626:18: ( 'm' ( 's' )? | 's' | 'h' )
                                            {
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:626:18: ( 'm' ( 's' )? | 's' | 'h' )
                                            int alt22=3;
                                            switch ( input.LA(1) ) {
                                            case 'm':
                                                {
                                                alt22=1;
                                                }
                                                break;
                                            case 's':
                                                {
                                                alt22=2;
                                                }
                                                break;
                                            case 'h':
                                                {
                                                alt22=3;
                                                }
                                                break;
                                            default:
                                                if (state.backtracking>0) {state.failed=true; return ;}
                                                NoViableAltException nvae =
                                                    new NoViableAltException("", 22, 0, input);

                                                throw nvae;
                                            }

                                            switch (alt22) {
                                                case 1 :
                                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:626:19: 'm' ( 's' )?
                                                    {
                                                    match('m'); if (state.failed) return ;
                                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:626:23: ( 's' )?
                                                    int alt21=2;
                                                    int LA21_0 = input.LA(1);

                                                    if ( (LA21_0=='s') ) {
                                                        alt21=1;
                                                    }
                                                    switch (alt21) {
                                                        case 1 :
                                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:626:23: 's'
                                                            {
                                                            match('s'); if (state.failed) return ;

                                                            }
                                                            break;

                                                    }


                                                    }
                                                    break;
                                                case 2 :
                                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:626:30: 's'
                                                    {
                                                    match('s'); if (state.failed) return ;

                                                    }
                                                    break;
                                                case 3 :
                                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:626:36: 'h'
                                                    {
                                                    match('h'); if (state.failed) return ;

                                                    }
                                                    break;

                                            }

                                            if ( state.backtracking==0 ) {
                                               _type = TIME_LITERAL; 
                                            }

                                            }
                                            break;
                                        case 2 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:632:14: 
                                            {
                                            if ( state.backtracking==0 ) {
                                               _type = FLOATING_POINT_LITERAL; 
                                            }

                                            }
                                            break;

                                    }


                                    }
                                    break;
                                case 2 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:637:12: 
                                    {
                                    if ( state.backtracking==0 ) {
                                       _type = FLOATING_POINT_LITERAL; 
                                    }

                                    }
                                    break;

                            }


                            }
                            break;
                        case 5 :
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:643:9: 
                            {
                            if ( state.backtracking==0 ) {
                               
                                  					_type = DECIMAL_LITERAL;
                                  					checkIntLiteralRange(getText(), getCharIndex(), 10);
                                  				
                            }

                            }
                            break;

                    }


                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:652:6: ( '1' .. '9' ) ( Digits )? ({...}? => '.' ( Digits )? ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | ) | ( ( 'm' ( 's' )? | 's' | 'h' ) | Exponent | ) )
                    {
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:652:6: ( '1' .. '9' )
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:652:7: '1' .. '9'
                    {
                    matchRange('1','9'); if (state.failed) return ;

                    }

                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:652:17: ( Digits )?
                    int alt26=2;
                    int LA26_0 = input.LA(1);

                    if ( ((LA26_0>='0' && LA26_0<='9')) ) {
                        alt26=1;
                    }
                    switch (alt26) {
                        case 1 :
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:652:17: Digits
                            {
                            mDigits(); if (state.failed) return ;

                            }
                            break;

                    }

                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:656:7: ({...}? => '.' ( Digits )? ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | ) | ( ( 'm' ( 's' )? | 's' | 'h' ) | Exponent | ) )
                    int alt35=2;
                    int LA35_0 = input.LA(1);

                    if ( (LA35_0=='.') && (( input.LA(2) != '.'))) {
                        alt35=1;
                    }
                    else {
                        alt35=2;}
                    switch (alt35) {
                        case 1 :
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:658:8: {...}? => '.' ( Digits )? ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | )
                            {
                            if ( !(( input.LA(2) != '.')) ) {
                                if (state.backtracking>0) {state.failed=true; return ;}
                                throw new FailedPredicateException(input, "FLOATING_POINT_LITERAL", " input.LA(2) != '.'");
                            }
                            match('.'); if (state.failed) return ;
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:660:15: ( Digits )?
                            int alt27=2;
                            int LA27_0 = input.LA(1);

                            if ( ((LA27_0>='0' && LA27_0<='9')) ) {
                                alt27=1;
                            }
                            switch (alt27) {
                                case 1 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:660:15: Digits
                                    {
                                    mDigits(); if (state.failed) return ;

                                    }
                                    break;

                            }

                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:660:23: ( Exponent )?
                            int alt28=2;
                            int LA28_0 = input.LA(1);

                            if ( (LA28_0=='E'||LA28_0=='e') ) {
                                alt28=1;
                            }
                            switch (alt28) {
                                case 1 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:660:23: Exponent
                                    {
                                    mExponent(); if (state.failed) return ;

                                    }
                                    break;

                            }

                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:662:9: ( ( 'm' ( 's' )? | 's' | 'h' ) | )
                            int alt31=2;
                            int LA31_0 = input.LA(1);

                            if ( (LA31_0=='h'||LA31_0=='m'||LA31_0=='s') ) {
                                alt31=1;
                            }
                            else {
                                alt31=2;}
                            switch (alt31) {
                                case 1 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:663:12: ( 'm' ( 's' )? | 's' | 'h' )
                                    {
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:663:12: ( 'm' ( 's' )? | 's' | 'h' )
                                    int alt30=3;
                                    switch ( input.LA(1) ) {
                                    case 'm':
                                        {
                                        alt30=1;
                                        }
                                        break;
                                    case 's':
                                        {
                                        alt30=2;
                                        }
                                        break;
                                    case 'h':
                                        {
                                        alt30=3;
                                        }
                                        break;
                                    default:
                                        if (state.backtracking>0) {state.failed=true; return ;}
                                        NoViableAltException nvae =
                                            new NoViableAltException("", 30, 0, input);

                                        throw nvae;
                                    }

                                    switch (alt30) {
                                        case 1 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:663:13: 'm' ( 's' )?
                                            {
                                            match('m'); if (state.failed) return ;
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:663:17: ( 's' )?
                                            int alt29=2;
                                            int LA29_0 = input.LA(1);

                                            if ( (LA29_0=='s') ) {
                                                alt29=1;
                                            }
                                            switch (alt29) {
                                                case 1 :
                                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:663:17: 's'
                                                    {
                                                    match('s'); if (state.failed) return ;

                                                    }
                                                    break;

                                            }


                                            }
                                            break;
                                        case 2 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:663:24: 's'
                                            {
                                            match('s'); if (state.failed) return ;

                                            }
                                            break;
                                        case 3 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:663:30: 'h'
                                            {
                                            match('h'); if (state.failed) return ;

                                            }
                                            break;

                                    }

                                    if ( state.backtracking==0 ) {
                                       _type = TIME_LITERAL; 
                                    }

                                    }
                                    break;
                                case 2 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:669:11: 
                                    {
                                    if ( state.backtracking==0 ) {
                                       _type = FLOATING_POINT_LITERAL; 
                                    }

                                    }
                                    break;

                            }


                            }
                            break;
                        case 2 :
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:674:9: ( ( 'm' ( 's' )? | 's' | 'h' ) | Exponent | )
                            {
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:674:9: ( ( 'm' ( 's' )? | 's' | 'h' ) | Exponent | )
                            int alt34=3;
                            switch ( input.LA(1) ) {
                            case 'h':
                            case 'm':
                            case 's':
                                {
                                alt34=1;
                                }
                                break;
                            case 'E':
                            case 'e':
                                {
                                alt34=2;
                                }
                                break;
                            default:
                                alt34=3;}

                            switch (alt34) {
                                case 1 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:675:12: ( 'm' ( 's' )? | 's' | 'h' )
                                    {
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:675:12: ( 'm' ( 's' )? | 's' | 'h' )
                                    int alt33=3;
                                    switch ( input.LA(1) ) {
                                    case 'm':
                                        {
                                        alt33=1;
                                        }
                                        break;
                                    case 's':
                                        {
                                        alt33=2;
                                        }
                                        break;
                                    case 'h':
                                        {
                                        alt33=3;
                                        }
                                        break;
                                    default:
                                        if (state.backtracking>0) {state.failed=true; return ;}
                                        NoViableAltException nvae =
                                            new NoViableAltException("", 33, 0, input);

                                        throw nvae;
                                    }

                                    switch (alt33) {
                                        case 1 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:675:13: 'm' ( 's' )?
                                            {
                                            match('m'); if (state.failed) return ;
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:675:17: ( 's' )?
                                            int alt32=2;
                                            int LA32_0 = input.LA(1);

                                            if ( (LA32_0=='s') ) {
                                                alt32=1;
                                            }
                                            switch (alt32) {
                                                case 1 :
                                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:675:17: 's'
                                                    {
                                                    match('s'); if (state.failed) return ;

                                                    }
                                                    break;

                                            }


                                            }
                                            break;
                                        case 2 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:675:24: 's'
                                            {
                                            match('s'); if (state.failed) return ;

                                            }
                                            break;
                                        case 3 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:675:30: 'h'
                                            {
                                            match('h'); if (state.failed) return ;

                                            }
                                            break;

                                    }

                                    if ( state.backtracking==0 ) {
                                       _type = TIME_LITERAL; 
                                    }

                                    }
                                    break;
                                case 2 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:679:12: Exponent
                                    {
                                    mExponent(); if (state.failed) return ;
                                    if ( state.backtracking==0 ) {

                                      				    			_type = FLOATING_POINT_LITERAL;
                                      				    		
                                    }

                                    }
                                    break;
                                case 3 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:687:11: 
                                    {
                                    if ( state.backtracking==0 ) {
                                       
                                      				    			_type = DECIMAL_LITERAL; 
                                      				    			checkIntLiteralRange(getText(), getCharIndex(), 10); 
                                      				    		
                                    }

                                    }
                                    break;

                            }


                            }
                            break;

                    }


                    }
                    break;
                case 3 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:695:6: '.' ( Digits ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | ) | '.' | )
                    {
                    match('.'); if (state.failed) return ;
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:697:7: ( Digits ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | ) | '.' | )
                    int alt40=3;
                    switch ( input.LA(1) ) {
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        {
                        alt40=1;
                        }
                        break;
                    case '.':
                        {
                        alt40=2;
                        }
                        break;
                    default:
                        alt40=3;}

                    switch (alt40) {
                        case 1 :
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:699:10: Digits ( Exponent )? ( ( 'm' ( 's' )? | 's' | 'h' ) | )
                            {
                            mDigits(); if (state.failed) return ;
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:699:17: ( Exponent )?
                            int alt36=2;
                            int LA36_0 = input.LA(1);

                            if ( (LA36_0=='E'||LA36_0=='e') ) {
                                alt36=1;
                            }
                            switch (alt36) {
                                case 1 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:699:17: Exponent
                                    {
                                    mExponent(); if (state.failed) return ;

                                    }
                                    break;

                            }

                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:701:11: ( ( 'm' ( 's' )? | 's' | 'h' ) | )
                            int alt39=2;
                            int LA39_0 = input.LA(1);

                            if ( (LA39_0=='h'||LA39_0=='m'||LA39_0=='s') ) {
                                alt39=1;
                            }
                            else {
                                alt39=2;}
                            switch (alt39) {
                                case 1 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:702:13: ( 'm' ( 's' )? | 's' | 'h' )
                                    {
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:702:13: ( 'm' ( 's' )? | 's' | 'h' )
                                    int alt38=3;
                                    switch ( input.LA(1) ) {
                                    case 'm':
                                        {
                                        alt38=1;
                                        }
                                        break;
                                    case 's':
                                        {
                                        alt38=2;
                                        }
                                        break;
                                    case 'h':
                                        {
                                        alt38=3;
                                        }
                                        break;
                                    default:
                                        if (state.backtracking>0) {state.failed=true; return ;}
                                        NoViableAltException nvae =
                                            new NoViableAltException("", 38, 0, input);

                                        throw nvae;
                                    }

                                    switch (alt38) {
                                        case 1 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:702:14: 'm' ( 's' )?
                                            {
                                            match('m'); if (state.failed) return ;
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:702:18: ( 's' )?
                                            int alt37=2;
                                            int LA37_0 = input.LA(1);

                                            if ( (LA37_0=='s') ) {
                                                alt37=1;
                                            }
                                            switch (alt37) {
                                                case 1 :
                                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:702:18: 's'
                                                    {
                                                    match('s'); if (state.failed) return ;

                                                    }
                                                    break;

                                            }


                                            }
                                            break;
                                        case 2 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:702:25: 's'
                                            {
                                            match('s'); if (state.failed) return ;

                                            }
                                            break;
                                        case 3 :
                                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:702:31: 'h'
                                            {
                                            match('h'); if (state.failed) return ;

                                            }
                                            break;

                                    }

                                    if ( state.backtracking==0 ) {
                                       _type = TIME_LITERAL; 
                                    }

                                    }
                                    break;
                                case 2 :
                                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:708:12: 
                                    {
                                    if ( state.backtracking==0 ) {
                                       _type = FLOATING_POINT_LITERAL; 
                                    }

                                    }
                                    break;

                            }


                            }
                            break;
                        case 2 :
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:714:9: '.'
                            {
                            match('.'); if (state.failed) return ;
                            if ( state.backtracking==0 ) {

                                  					_type = DOTDOT;	// Yes, it was ..
                                  				
                            }

                            }
                            break;
                        case 3 :
                            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:722:9: 
                            {
                            if ( state.backtracking==0 ) {
                               _type = DOT; 
                            }

                            }
                            break;

                    }


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "FLOATING_POINT_LITERAL"

    // $ANTLR start "Digits"
    public final void mDigits() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:729:2: ( ( '0' .. '9' )+ )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:729:4: ( '0' .. '9' )+
            {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:729:4: ( '0' .. '9' )+
            int cnt42=0;
            loop42:
            do {
                int alt42=2;
                int LA42_0 = input.LA(1);

                if ( ((LA42_0>='0' && LA42_0<='9')) ) {
                    alt42=1;
                }


                switch (alt42) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:729:5: '0' .. '9'
            	    {
            	    matchRange('0','9'); if (state.failed) return ;

            	    }
            	    break;

            	default :
            	    if ( cnt42 >= 1 ) break loop42;
            	    if (state.backtracking>0) {state.failed=true; return ;}
                        EarlyExitException eee =
                            new EarlyExitException(42, input);
                        throw eee;
                }
                cnt42++;
            } while (true);


            }

        }
        finally {
        }
    }
    // $ANTLR end "Digits"

    // $ANTLR start "Exponent"
    public final void mExponent() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:734:2: ( ( 'e' | 'E' ) ( '+' | '-' )? ( Digits | ) )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:734:5: ( 'e' | 'E' ) ( '+' | '-' )? ( Digits | )
            {
            if ( input.LA(1)=='E'||input.LA(1)=='e' ) {
                input.consume();
            state.failed=false;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:734:15: ( '+' | '-' )?
            int alt43=2;
            int LA43_0 = input.LA(1);

            if ( (LA43_0=='+'||LA43_0=='-') ) {
                alt43=1;
            }
            switch (alt43) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:
                    {
                    if ( input.LA(1)=='+'||input.LA(1)=='-' ) {
                        input.consume();
                    state.failed=false;
                    }
                    else {
                        if (state.backtracking>0) {state.failed=true; return ;}
                        MismatchedSetException mse = new MismatchedSetException(null,input);
                        recover(mse);
                        throw mse;}


                    }
                    break;

            }

            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:736:4: ( Digits | )
            int alt44=2;
            int LA44_0 = input.LA(1);

            if ( ((LA44_0>='0' && LA44_0<='9')) ) {
                alt44=1;
            }
            else {
                alt44=2;}
            switch (alt44) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:737:7: Digits
                    {
                    mDigits(); if (state.failed) return ;

                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:738:7: 
                    {
                    if ( state.backtracking==0 ) {
                       log.error(getCharIndex()-1, MsgSym.MESSAGE_JAVAFX_EXPONENT_MALFORMED); 
                    }

                    }
                    break;

            }


            }

        }
        finally {
        }
    }
    // $ANTLR end "Exponent"

    // $ANTLR start "IDENTIFIER"
    public final void mIDENTIFIER() throws RecognitionException {
        try {
            int _type = IDENTIFIER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:751:2: ( Letter ( Letter | JavaIDDigit )* | '<<' (~ '>' | '>' ~ '>' )* ( '>' )* '>>' )
            int alt48=2;
            int LA48_0 = input.LA(1);

            if ( (LA48_0=='$'||(LA48_0>='A' && LA48_0<='Z')||LA48_0=='_'||(LA48_0>='a' && LA48_0<='z')||(LA48_0>='\u00C0' && LA48_0<='\u00D6')||(LA48_0>='\u00D8' && LA48_0<='\u00F6')||(LA48_0>='\u00F8' && LA48_0<='\u1FFF')||(LA48_0>='\u3040' && LA48_0<='\u318F')||(LA48_0>='\u3300' && LA48_0<='\u337F')||(LA48_0>='\u3400' && LA48_0<='\u3D2D')||(LA48_0>='\u4E00' && LA48_0<='\u9FFF')||(LA48_0>='\uF900' && LA48_0<='\uFAFF')) ) {
                alt48=1;
            }
            else if ( (LA48_0=='<') ) {
                alt48=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                NoViableAltException nvae =
                    new NoViableAltException("", 48, 0, input);

                throw nvae;
            }
            switch (alt48) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:751:4: Letter ( Letter | JavaIDDigit )*
                    {
                    mLetter(); if (state.failed) return ;
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:751:11: ( Letter | JavaIDDigit )*
                    loop45:
                    do {
                        int alt45=2;
                        int LA45_0 = input.LA(1);

                        if ( (LA45_0=='$'||(LA45_0>='0' && LA45_0<='9')||(LA45_0>='A' && LA45_0<='Z')||LA45_0=='_'||(LA45_0>='a' && LA45_0<='z')||(LA45_0>='\u00C0' && LA45_0<='\u00D6')||(LA45_0>='\u00D8' && LA45_0<='\u00F6')||(LA45_0>='\u00F8' && LA45_0<='\u1FFF')||(LA45_0>='\u3040' && LA45_0<='\u318F')||(LA45_0>='\u3300' && LA45_0<='\u337F')||(LA45_0>='\u3400' && LA45_0<='\u3D2D')||(LA45_0>='\u4E00' && LA45_0<='\u9FFF')||(LA45_0>='\uF900' && LA45_0<='\uFAFF')) ) {
                            alt45=1;
                        }


                        switch (alt45) {
                    	case 1 :
                    	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:
                    	    {
                    	    if ( input.LA(1)=='$'||(input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z')||(input.LA(1)>='\u00C0' && input.LA(1)<='\u00D6')||(input.LA(1)>='\u00D8' && input.LA(1)<='\u00F6')||(input.LA(1)>='\u00F8' && input.LA(1)<='\u1FFF')||(input.LA(1)>='\u3040' && input.LA(1)<='\u318F')||(input.LA(1)>='\u3300' && input.LA(1)<='\u337F')||(input.LA(1)>='\u3400' && input.LA(1)<='\u3D2D')||(input.LA(1)>='\u4E00' && input.LA(1)<='\u9FFF')||(input.LA(1)>='\uF900' && input.LA(1)<='\uFAFF') ) {
                    	        input.consume();
                    	    state.failed=false;
                    	    }
                    	    else {
                    	        if (state.backtracking>0) {state.failed=true; return ;}
                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                    	        recover(mse);
                    	        throw mse;}


                    	    }
                    	    break;

                    	default :
                    	    break loop45;
                        }
                    } while (true);


                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:752:4: '<<' (~ '>' | '>' ~ '>' )* ( '>' )* '>>'
                    {
                    match("<<"); if (state.failed) return ;

                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:752:9: (~ '>' | '>' ~ '>' )*
                    loop46:
                    do {
                        int alt46=3;
                        int LA46_0 = input.LA(1);

                        if ( (LA46_0=='>') ) {
                            int LA46_1 = input.LA(2);

                            if ( ((LA46_1>='\u0000' && LA46_1<='=')||(LA46_1>='?' && LA46_1<='\uFFFE')) ) {
                                alt46=2;
                            }


                        }
                        else if ( ((LA46_0>='\u0000' && LA46_0<='=')||(LA46_0>='?' && LA46_0<='\uFFFE')) ) {
                            alt46=1;
                        }


                        switch (alt46) {
                    	case 1 :
                    	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:752:10: ~ '>'
                    	    {
                    	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='=')||(input.LA(1)>='?' && input.LA(1)<='\uFFFE') ) {
                    	        input.consume();
                    	    state.failed=false;
                    	    }
                    	    else {
                    	        if (state.backtracking>0) {state.failed=true; return ;}
                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                    	        recover(mse);
                    	        throw mse;}


                    	    }
                    	    break;
                    	case 2 :
                    	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:752:16: '>' ~ '>'
                    	    {
                    	    match('>'); if (state.failed) return ;
                    	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='=')||(input.LA(1)>='?' && input.LA(1)<='\uFFFE') ) {
                    	        input.consume();
                    	    state.failed=false;
                    	    }
                    	    else {
                    	        if (state.backtracking>0) {state.failed=true; return ;}
                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                    	        recover(mse);
                    	        throw mse;}


                    	    }
                    	    break;

                    	default :
                    	    break loop46;
                        }
                    } while (true);

                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:752:27: ( '>' )*
                    loop47:
                    do {
                        int alt47=2;
                        int LA47_0 = input.LA(1);

                        if ( (LA47_0=='>') ) {
                            int LA47_1 = input.LA(2);

                            if ( (LA47_1=='>') ) {
                                int LA47_2 = input.LA(3);

                                if ( (LA47_2=='>') ) {
                                    alt47=1;
                                }


                            }


                        }


                        switch (alt47) {
                    	case 1 :
                    	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:752:27: '>'
                    	    {
                    	    match('>'); if (state.failed) return ;

                    	    }
                    	    break;

                    	default :
                    	    break loop47;
                        }
                    } while (true);

                    match(">>"); if (state.failed) return ;

                    if ( state.backtracking==0 ) {
                       
                      				setText(getText().substring(2, getText().length()-2)); 
                      			
                    }

                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "IDENTIFIER"

    // $ANTLR start "Letter"
    public final void mLetter() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:763:5: ( '\\u0024' | '\\u0041' .. '\\u005a' | '\\u005f' | '\\u0061' .. '\\u007a' | '\\u00c0' .. '\\u00d6' | '\\u00d8' .. '\\u00f6' | '\\u00f8' .. '\\u00ff' | '\\u0100' .. '\\u1fff' | '\\u3040' .. '\\u318f' | '\\u3300' .. '\\u337f' | '\\u3400' .. '\\u3d2d' | '\\u4e00' .. '\\u9fff' | '\\uf900' .. '\\ufaff' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:
            {
            if ( input.LA(1)=='$'||(input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z')||(input.LA(1)>='\u00C0' && input.LA(1)<='\u00D6')||(input.LA(1)>='\u00D8' && input.LA(1)<='\u00F6')||(input.LA(1)>='\u00F8' && input.LA(1)<='\u1FFF')||(input.LA(1)>='\u3040' && input.LA(1)<='\u318F')||(input.LA(1)>='\u3300' && input.LA(1)<='\u337F')||(input.LA(1)>='\u3400' && input.LA(1)<='\u3D2D')||(input.LA(1)>='\u4E00' && input.LA(1)<='\u9FFF')||(input.LA(1)>='\uF900' && input.LA(1)<='\uFAFF') ) {
                input.consume();
            state.failed=false;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
        }
    }
    // $ANTLR end "Letter"

    // $ANTLR start "JavaIDDigit"
    public final void mJavaIDDigit() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:782:5: ( '\\u0030' .. '\\u0039' | '\\u0660' .. '\\u0669' | '\\u06f0' .. '\\u06f9' | '\\u0966' .. '\\u096f' | '\\u09e6' .. '\\u09ef' | '\\u0a66' .. '\\u0a6f' | '\\u0ae6' .. '\\u0aef' | '\\u0b66' .. '\\u0b6f' | '\\u0be7' .. '\\u0bef' | '\\u0c66' .. '\\u0c6f' | '\\u0ce6' .. '\\u0cef' | '\\u0d66' .. '\\u0d6f' | '\\u0e50' .. '\\u0e59' | '\\u0ed0' .. '\\u0ed9' | '\\u1040' .. '\\u1049' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:
            {
            if ( (input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='\u0660' && input.LA(1)<='\u0669')||(input.LA(1)>='\u06F0' && input.LA(1)<='\u06F9')||(input.LA(1)>='\u0966' && input.LA(1)<='\u096F')||(input.LA(1)>='\u09E6' && input.LA(1)<='\u09EF')||(input.LA(1)>='\u0A66' && input.LA(1)<='\u0A6F')||(input.LA(1)>='\u0AE6' && input.LA(1)<='\u0AEF')||(input.LA(1)>='\u0B66' && input.LA(1)<='\u0B6F')||(input.LA(1)>='\u0BE7' && input.LA(1)<='\u0BEF')||(input.LA(1)>='\u0C66' && input.LA(1)<='\u0C6F')||(input.LA(1)>='\u0CE6' && input.LA(1)<='\u0CEF')||(input.LA(1)>='\u0D66' && input.LA(1)<='\u0D6F')||(input.LA(1)>='\u0E50' && input.LA(1)<='\u0E59')||(input.LA(1)>='\u0ED0' && input.LA(1)<='\u0ED9')||(input.LA(1)>='\u1040' && input.LA(1)<='\u1049') ) {
                input.consume();
            state.failed=false;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

        }
        finally {
        }
    }
    // $ANTLR end "JavaIDDigit"

    // $ANTLR start "WS"
    public final void mWS() throws RecognitionException {
        try {
            int _type = WS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:803:5: ( ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' ) )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:803:8: ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )
            {
            if ( (input.LA(1)>='\t' && input.LA(1)<='\n')||(input.LA(1)>='\f' && input.LA(1)<='\r')||input.LA(1)==' ' ) {
                input.consume();
            state.failed=false;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            if ( state.backtracking==0 ) {

              			_channel=HIDDEN;
              		
            }

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WS"

    // $ANTLR start "COMMENT"
    public final void mCOMMENT() throws RecognitionException {
        try {
            int _type = COMMENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:816:5: ( '/*' ( options {greedy=false; } : . )* '*/' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:816:9: '/*' ( options {greedy=false; } : . )* '*/'
            {
            match("/*"); if (state.failed) return ;

            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:816:14: ( options {greedy=false; } : . )*
            loop49:
            do {
                int alt49=2;
                int LA49_0 = input.LA(1);

                if ( (LA49_0=='*') ) {
                    int LA49_1 = input.LA(2);

                    if ( (LA49_1=='/') ) {
                        alt49=2;
                    }
                    else if ( ((LA49_1>='\u0000' && LA49_1<='.')||(LA49_1>='0' && LA49_1<='\uFFFE')) ) {
                        alt49=1;
                    }


                }
                else if ( ((LA49_0>='\u0000' && LA49_0<=')')||(LA49_0>='+' && LA49_0<='\uFFFE')) ) {
                    alt49=1;
                }


                switch (alt49) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:816:42: .
            	    {
            	    matchAny(); if (state.failed) return ;

            	    }
            	    break;

            	default :
            	    break loop49;
                }
            } while (true);

            match("*/"); if (state.failed) return ;

            if ( state.backtracking==0 ) {

                  		_channel=HIDDEN;
                  	
            }

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "COMMENT"

    // $ANTLR start "DOC_COMMENT"
    public final void mDOC_COMMENT() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:825:2: ( '/**' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:825:4: '/**'
            {
            match("/**"); if (state.failed) return ;


            }

        }
        finally {
        }
    }
    // $ANTLR end "DOC_COMMENT"

    // $ANTLR start "LINE_COMMENT"
    public final void mLINE_COMMENT() throws RecognitionException {
        try {
            int _type = LINE_COMMENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:829:5: ( '//' (~ ( '\\n' | '\\r' ) )* ( '\\r' )? ( '\\n' | EOF ) )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:829:8: '//' (~ ( '\\n' | '\\r' ) )* ( '\\r' )? ( '\\n' | EOF )
            {
            match("//"); if (state.failed) return ;

            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:829:13: (~ ( '\\n' | '\\r' ) )*
            loop50:
            do {
                int alt50=2;
                int LA50_0 = input.LA(1);

                if ( ((LA50_0>='\u0000' && LA50_0<='\t')||(LA50_0>='\u000B' && LA50_0<='\f')||(LA50_0>='\u000E' && LA50_0<='\uFFFE')) ) {
                    alt50=1;
                }


                switch (alt50) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:829:13: ~ ( '\\n' | '\\r' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\t')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\uFFFE') ) {
            	        input.consume();
            	    state.failed=false;
            	    }
            	    else {
            	        if (state.backtracking>0) {state.failed=true; return ;}
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop50;
                }
            } while (true);

            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:829:27: ( '\\r' )?
            int alt51=2;
            int LA51_0 = input.LA(1);

            if ( (LA51_0=='\r') ) {
                alt51=1;
            }
            switch (alt51) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:829:27: '\\r'
                    {
                    match('\r'); if (state.failed) return ;

                    }
                    break;

            }

            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:829:33: ( '\\n' | EOF )
            int alt52=2;
            int LA52_0 = input.LA(1);

            if ( (LA52_0=='\n') ) {
                alt52=1;
            }
            else {
                alt52=2;}
            switch (alt52) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:829:34: '\\n'
                    {
                    match('\n'); if (state.failed) return ;

                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:829:39: EOF
                    {
                    match(EOF); if (state.failed) return ;

                    }
                    break;

            }

            if ( state.backtracking==0 ) {

                  		_channel=HIDDEN;
                  	
            }

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LINE_COMMENT"

    // $ANTLR start "LAST_TOKEN"
    public final void mLAST_TOKEN() throws RecognitionException {
        try {
            int _type = LAST_TOKEN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:837:5: ( '~~~~~~~~' {...}? '~~~~~~~~' )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:837:7: '~~~~~~~~' {...}? '~~~~~~~~'
            {
            match("~~~~~~~~"); if (state.failed) return ;

            if ( !((false)) ) {
                if (state.backtracking>0) {state.failed=true; return ;}
                throw new FailedPredicateException(input, "LAST_TOKEN", "false");
            }
            match("~~~~~~~~"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LAST_TOKEN"

    // $ANTLR start "INVALIDC"
    public final void mINVALIDC() throws RecognitionException {
        try {
            int _type = INVALIDC;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:846:2: ( . )
            // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:846:4: .
            {
            matchAny(); if (state.failed) return ;
            if ( state.backtracking==0 ) {

              			// We assume it isn't safe to print as otherwise we would have matched it
              			//	
              			log.error(getCharIndex()-1, MsgSym.MESSAGE_JAVAFX_BAD_CHARACTER, "\\u" + Integer.toHexString( getText().charAt(0) ) );
              		
            }

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "INVALIDC"

    public void mTokens() throws RecognitionException {
        // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:8: ( ABSTRACT | AFTER | AND | AS | ASSERT | AT | ATTRIBUTE | BEFORE | BIND | BOUND | BREAK | CATCH | CLASS | CONTINUE | DEF | DELETE | ELSE | EXCLUSIVE | EXTENDS | FALSE | FINALLY | FIRST | FOR | FROM | FUNCTION | IF | IMPORT | INDEXOF | IN | INIT | INSERT | INSTANCEOF | INTO | INVERSE | LAST | LAZY | MOD | NEW | NON_WRITABLE | NOT | NULL | ON | OR | OVERRIDE | PACKAGE | POSTINIT | PRIVATE | PROTECTED | PUBLIC_INIT | PUBLIC | PUBLIC_READABLE | PUBLIC_READ | READABLE | REPLACE | RETURN | REVERSE | SIZEOF | STATIC | STEP | SUPER | THEN | THIS | THROW | TRIGGER | TRUE | TRY | TWEEN | TYPEOF | VAR | WHERE | WHILE | WITH | LBRACKET | LPAREN | POUND | PIPE | PLUSPLUS | DOTDOT | RPAREN | RBRACKET | SEMI | COMMA | DOT | EQEQ | EQ | GT | LT | LTGT | LTEQ | GTEQ | PLUS | SUB | STAR | SLASH | PERCENT | PLUSEQ | SUBEQ | STAREQ | SLASHEQ | PERCENTEQ | NOTEQ | COLON | QUES | SUCHTHAT | SUBSUB | STRING_LITERAL | QUOTE_LBRACE_STRING_LITERAL | LBRACE | RBRACE_QUOTE_STRING_LITERAL | RBRACE_LBRACE_STRING_LITERAL | RBRACE | FORMAT_STRING_LITERAL | TRANSLATION_KEY | FLOATING_POINT_LITERAL | IDENTIFIER | WS | COMMENT | LINE_COMMENT | LAST_TOKEN | INVALIDC )
        int alt53=120;
        alt53 = dfa53.predict(input);
        switch (alt53) {
            case 1 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:10: ABSTRACT
                {
                mABSTRACT(); if (state.failed) return ;

                }
                break;
            case 2 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:19: AFTER
                {
                mAFTER(); if (state.failed) return ;

                }
                break;
            case 3 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:25: AND
                {
                mAND(); if (state.failed) return ;

                }
                break;
            case 4 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:29: AS
                {
                mAS(); if (state.failed) return ;

                }
                break;
            case 5 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:32: ASSERT
                {
                mASSERT(); if (state.failed) return ;

                }
                break;
            case 6 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:39: AT
                {
                mAT(); if (state.failed) return ;

                }
                break;
            case 7 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:42: ATTRIBUTE
                {
                mATTRIBUTE(); if (state.failed) return ;

                }
                break;
            case 8 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:52: BEFORE
                {
                mBEFORE(); if (state.failed) return ;

                }
                break;
            case 9 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:59: BIND
                {
                mBIND(); if (state.failed) return ;

                }
                break;
            case 10 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:64: BOUND
                {
                mBOUND(); if (state.failed) return ;

                }
                break;
            case 11 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:70: BREAK
                {
                mBREAK(); if (state.failed) return ;

                }
                break;
            case 12 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:76: CATCH
                {
                mCATCH(); if (state.failed) return ;

                }
                break;
            case 13 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:82: CLASS
                {
                mCLASS(); if (state.failed) return ;

                }
                break;
            case 14 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:88: CONTINUE
                {
                mCONTINUE(); if (state.failed) return ;

                }
                break;
            case 15 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:97: DEF
                {
                mDEF(); if (state.failed) return ;

                }
                break;
            case 16 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:101: DELETE
                {
                mDELETE(); if (state.failed) return ;

                }
                break;
            case 17 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:108: ELSE
                {
                mELSE(); if (state.failed) return ;

                }
                break;
            case 18 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:113: EXCLUSIVE
                {
                mEXCLUSIVE(); if (state.failed) return ;

                }
                break;
            case 19 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:123: EXTENDS
                {
                mEXTENDS(); if (state.failed) return ;

                }
                break;
            case 20 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:131: FALSE
                {
                mFALSE(); if (state.failed) return ;

                }
                break;
            case 21 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:137: FINALLY
                {
                mFINALLY(); if (state.failed) return ;

                }
                break;
            case 22 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:145: FIRST
                {
                mFIRST(); if (state.failed) return ;

                }
                break;
            case 23 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:151: FOR
                {
                mFOR(); if (state.failed) return ;

                }
                break;
            case 24 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:155: FROM
                {
                mFROM(); if (state.failed) return ;

                }
                break;
            case 25 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:160: FUNCTION
                {
                mFUNCTION(); if (state.failed) return ;

                }
                break;
            case 26 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:169: IF
                {
                mIF(); if (state.failed) return ;

                }
                break;
            case 27 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:172: IMPORT
                {
                mIMPORT(); if (state.failed) return ;

                }
                break;
            case 28 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:179: INDEXOF
                {
                mINDEXOF(); if (state.failed) return ;

                }
                break;
            case 29 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:187: IN
                {
                mIN(); if (state.failed) return ;

                }
                break;
            case 30 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:190: INIT
                {
                mINIT(); if (state.failed) return ;

                }
                break;
            case 31 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:195: INSERT
                {
                mINSERT(); if (state.failed) return ;

                }
                break;
            case 32 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:202: INSTANCEOF
                {
                mINSTANCEOF(); if (state.failed) return ;

                }
                break;
            case 33 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:213: INTO
                {
                mINTO(); if (state.failed) return ;

                }
                break;
            case 34 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:218: INVERSE
                {
                mINVERSE(); if (state.failed) return ;

                }
                break;
            case 35 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:226: LAST
                {
                mLAST(); if (state.failed) return ;

                }
                break;
            case 36 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:231: LAZY
                {
                mLAZY(); if (state.failed) return ;

                }
                break;
            case 37 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:236: MOD
                {
                mMOD(); if (state.failed) return ;

                }
                break;
            case 38 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:240: NEW
                {
                mNEW(); if (state.failed) return ;

                }
                break;
            case 39 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:244: NON_WRITABLE
                {
                mNON_WRITABLE(); if (state.failed) return ;

                }
                break;
            case 40 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:257: NOT
                {
                mNOT(); if (state.failed) return ;

                }
                break;
            case 41 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:261: NULL
                {
                mNULL(); if (state.failed) return ;

                }
                break;
            case 42 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:266: ON
                {
                mON(); if (state.failed) return ;

                }
                break;
            case 43 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:269: OR
                {
                mOR(); if (state.failed) return ;

                }
                break;
            case 44 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:272: OVERRIDE
                {
                mOVERRIDE(); if (state.failed) return ;

                }
                break;
            case 45 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:281: PACKAGE
                {
                mPACKAGE(); if (state.failed) return ;

                }
                break;
            case 46 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:289: POSTINIT
                {
                mPOSTINIT(); if (state.failed) return ;

                }
                break;
            case 47 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:298: PRIVATE
                {
                mPRIVATE(); if (state.failed) return ;

                }
                break;
            case 48 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:306: PROTECTED
                {
                mPROTECTED(); if (state.failed) return ;

                }
                break;
            case 49 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:316: PUBLIC_INIT
                {
                mPUBLIC_INIT(); if (state.failed) return ;

                }
                break;
            case 50 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:328: PUBLIC
                {
                mPUBLIC(); if (state.failed) return ;

                }
                break;
            case 51 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:335: PUBLIC_READABLE
                {
                mPUBLIC_READABLE(); if (state.failed) return ;

                }
                break;
            case 52 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:351: PUBLIC_READ
                {
                mPUBLIC_READ(); if (state.failed) return ;

                }
                break;
            case 53 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:363: READABLE
                {
                mREADABLE(); if (state.failed) return ;

                }
                break;
            case 54 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:372: REPLACE
                {
                mREPLACE(); if (state.failed) return ;

                }
                break;
            case 55 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:380: RETURN
                {
                mRETURN(); if (state.failed) return ;

                }
                break;
            case 56 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:387: REVERSE
                {
                mREVERSE(); if (state.failed) return ;

                }
                break;
            case 57 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:395: SIZEOF
                {
                mSIZEOF(); if (state.failed) return ;

                }
                break;
            case 58 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:402: STATIC
                {
                mSTATIC(); if (state.failed) return ;

                }
                break;
            case 59 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:409: STEP
                {
                mSTEP(); if (state.failed) return ;

                }
                break;
            case 60 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:414: SUPER
                {
                mSUPER(); if (state.failed) return ;

                }
                break;
            case 61 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:420: THEN
                {
                mTHEN(); if (state.failed) return ;

                }
                break;
            case 62 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:425: THIS
                {
                mTHIS(); if (state.failed) return ;

                }
                break;
            case 63 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:430: THROW
                {
                mTHROW(); if (state.failed) return ;

                }
                break;
            case 64 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:436: TRIGGER
                {
                mTRIGGER(); if (state.failed) return ;

                }
                break;
            case 65 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:444: TRUE
                {
                mTRUE(); if (state.failed) return ;

                }
                break;
            case 66 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:449: TRY
                {
                mTRY(); if (state.failed) return ;

                }
                break;
            case 67 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:453: TWEEN
                {
                mTWEEN(); if (state.failed) return ;

                }
                break;
            case 68 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:459: TYPEOF
                {
                mTYPEOF(); if (state.failed) return ;

                }
                break;
            case 69 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:466: VAR
                {
                mVAR(); if (state.failed) return ;

                }
                break;
            case 70 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:470: WHERE
                {
                mWHERE(); if (state.failed) return ;

                }
                break;
            case 71 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:476: WHILE
                {
                mWHILE(); if (state.failed) return ;

                }
                break;
            case 72 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:482: WITH
                {
                mWITH(); if (state.failed) return ;

                }
                break;
            case 73 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:487: LBRACKET
                {
                mLBRACKET(); if (state.failed) return ;

                }
                break;
            case 74 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:496: LPAREN
                {
                mLPAREN(); if (state.failed) return ;

                }
                break;
            case 75 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:503: POUND
                {
                mPOUND(); if (state.failed) return ;

                }
                break;
            case 76 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:509: PIPE
                {
                mPIPE(); if (state.failed) return ;

                }
                break;
            case 77 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:514: PLUSPLUS
                {
                mPLUSPLUS(); if (state.failed) return ;

                }
                break;
            case 78 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:523: DOTDOT
                {
                mDOTDOT(); if (state.failed) return ;

                }
                break;
            case 79 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:530: RPAREN
                {
                mRPAREN(); if (state.failed) return ;

                }
                break;
            case 80 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:537: RBRACKET
                {
                mRBRACKET(); if (state.failed) return ;

                }
                break;
            case 81 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:546: SEMI
                {
                mSEMI(); if (state.failed) return ;

                }
                break;
            case 82 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:551: COMMA
                {
                mCOMMA(); if (state.failed) return ;

                }
                break;
            case 83 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:557: DOT
                {
                mDOT(); if (state.failed) return ;

                }
                break;
            case 84 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:561: EQEQ
                {
                mEQEQ(); if (state.failed) return ;

                }
                break;
            case 85 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:566: EQ
                {
                mEQ(); if (state.failed) return ;

                }
                break;
            case 86 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:569: GT
                {
                mGT(); if (state.failed) return ;

                }
                break;
            case 87 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:572: LT
                {
                mLT(); if (state.failed) return ;

                }
                break;
            case 88 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:575: LTGT
                {
                mLTGT(); if (state.failed) return ;

                }
                break;
            case 89 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:580: LTEQ
                {
                mLTEQ(); if (state.failed) return ;

                }
                break;
            case 90 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:585: GTEQ
                {
                mGTEQ(); if (state.failed) return ;

                }
                break;
            case 91 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:590: PLUS
                {
                mPLUS(); if (state.failed) return ;

                }
                break;
            case 92 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:595: SUB
                {
                mSUB(); if (state.failed) return ;

                }
                break;
            case 93 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:599: STAR
                {
                mSTAR(); if (state.failed) return ;

                }
                break;
            case 94 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:604: SLASH
                {
                mSLASH(); if (state.failed) return ;

                }
                break;
            case 95 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:610: PERCENT
                {
                mPERCENT(); if (state.failed) return ;

                }
                break;
            case 96 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:618: PLUSEQ
                {
                mPLUSEQ(); if (state.failed) return ;

                }
                break;
            case 97 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:625: SUBEQ
                {
                mSUBEQ(); if (state.failed) return ;

                }
                break;
            case 98 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:631: STAREQ
                {
                mSTAREQ(); if (state.failed) return ;

                }
                break;
            case 99 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:638: SLASHEQ
                {
                mSLASHEQ(); if (state.failed) return ;

                }
                break;
            case 100 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:646: PERCENTEQ
                {
                mPERCENTEQ(); if (state.failed) return ;

                }
                break;
            case 101 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:656: NOTEQ
                {
                mNOTEQ(); if (state.failed) return ;

                }
                break;
            case 102 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:662: COLON
                {
                mCOLON(); if (state.failed) return ;

                }
                break;
            case 103 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:668: QUES
                {
                mQUES(); if (state.failed) return ;

                }
                break;
            case 104 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:673: SUCHTHAT
                {
                mSUCHTHAT(); if (state.failed) return ;

                }
                break;
            case 105 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:682: SUBSUB
                {
                mSUBSUB(); if (state.failed) return ;

                }
                break;
            case 106 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:689: STRING_LITERAL
                {
                mSTRING_LITERAL(); if (state.failed) return ;

                }
                break;
            case 107 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:704: QUOTE_LBRACE_STRING_LITERAL
                {
                mQUOTE_LBRACE_STRING_LITERAL(); if (state.failed) return ;

                }
                break;
            case 108 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:732: LBRACE
                {
                mLBRACE(); if (state.failed) return ;

                }
                break;
            case 109 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:739: RBRACE_QUOTE_STRING_LITERAL
                {
                mRBRACE_QUOTE_STRING_LITERAL(); if (state.failed) return ;

                }
                break;
            case 110 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:767: RBRACE_LBRACE_STRING_LITERAL
                {
                mRBRACE_LBRACE_STRING_LITERAL(); if (state.failed) return ;

                }
                break;
            case 111 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:796: RBRACE
                {
                mRBRACE(); if (state.failed) return ;

                }
                break;
            case 112 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:803: FORMAT_STRING_LITERAL
                {
                mFORMAT_STRING_LITERAL(); if (state.failed) return ;

                }
                break;
            case 113 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:825: TRANSLATION_KEY
                {
                mTRANSLATION_KEY(); if (state.failed) return ;

                }
                break;
            case 114 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:841: FLOATING_POINT_LITERAL
                {
                mFLOATING_POINT_LITERAL(); if (state.failed) return ;

                }
                break;
            case 115 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:864: IDENTIFIER
                {
                mIDENTIFIER(); if (state.failed) return ;

                }
                break;
            case 116 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:875: WS
                {
                mWS(); if (state.failed) return ;

                }
                break;
            case 117 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:878: COMMENT
                {
                mCOMMENT(); if (state.failed) return ;

                }
                break;
            case 118 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:886: LINE_COMMENT
                {
                mLINE_COMMENT(); if (state.failed) return ;

                }
                break;
            case 119 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:899: LAST_TOKEN
                {
                mLAST_TOKEN(); if (state.failed) return ;

                }
                break;
            case 120 :
                // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:1:910: INVALIDC
                {
                mINVALIDC(); if (state.failed) return ;

                }
                break;

        }

    }

    // $ANTLR start synpred1_v4Lexer
    public final void synpred1_v4Lexer_fragment() throws RecognitionException {   
        // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:393:4: ( ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )* '%' )
        // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:393:5: ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )* '%'
        {
        // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:393:5: ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )*
        loop54:
        do {
            int alt54=2;
            int LA54_0 = input.LA(1);

            if ( ((LA54_0>='\t' && LA54_0<='\n')||(LA54_0>='\f' && LA54_0<='\r')||LA54_0==' ') ) {
                alt54=1;
            }


            switch (alt54) {
        	case 1 :
        	    // E:\\SunWork\\nbjfxp\\localrep\\main\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v4Lexer.g:
        	    {
        	    if ( (input.LA(1)>='\t' && input.LA(1)<='\n')||(input.LA(1)>='\f' && input.LA(1)<='\r')||input.LA(1)==' ' ) {
        	        input.consume();
        	    state.failed=false;
        	    }
        	    else {
        	        if (state.backtracking>0) {state.failed=true; return ;}
        	        MismatchedSetException mse = new MismatchedSetException(null,input);
        	        recover(mse);
        	        throw mse;}


        	    }
        	    break;

        	default :
        	    break loop54;
            }
        } while (true);

        match('%'); if (state.failed) return ;

        }
    }
    // $ANTLR end synpred1_v4Lexer

    public final boolean synpred1_v4Lexer() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred1_v4Lexer_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }


    protected DFA3 dfa3 = new DFA3(this);
    protected DFA4 dfa4 = new DFA4(this);
    protected DFA53 dfa53 = new DFA53(this);
    static final String DFA3_eotS =
        "\4\uffff\1\7\1\10\3\uffff";
    static final String DFA3_eofS =
        "\11\uffff";
    static final String DFA3_minS =
        "\1\175\6\0\2\uffff";
    static final String DFA3_maxS =
        "\1\175\6\ufffe\2\uffff";
    static final String DFA3_acceptS =
        "\7\uffff\1\1\1\2";
    static final String DFA3_specialS =
        "\1\6\1\2\1\5\1\1\1\0\1\4\1\3\2\uffff}>";
    static final String[] DFA3_transitionS = {
            "\1\1",
            "\42\2\1\4\4\2\1\5\64\2\1\3\36\2\1\uffff\uff83\2",
            "\42\2\1\4\4\2\1\5\64\2\1\3\36\2\1\uffff\uff83\2",
            "\uffff\6",
            "\173\10\1\uffff\uff83\10",
            "\173\7\1\uffff\uff83\7",
            "\42\2\1\4\4\2\1\5\64\2\1\3\36\2\1\uffff\uff83\2",
            "",
            ""
    };

    static final short[] DFA3_eot = DFA.unpackEncodedString(DFA3_eotS);
    static final short[] DFA3_eof = DFA.unpackEncodedString(DFA3_eofS);
    static final char[] DFA3_min = DFA.unpackEncodedStringToUnsignedChars(DFA3_minS);
    static final char[] DFA3_max = DFA.unpackEncodedStringToUnsignedChars(DFA3_maxS);
    static final short[] DFA3_accept = DFA.unpackEncodedString(DFA3_acceptS);
    static final short[] DFA3_special = DFA.unpackEncodedString(DFA3_specialS);
    static final short[][] DFA3_transition;

    static {
        int numStates = DFA3_transitionS.length;
        DFA3_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA3_transition[i] = DFA.unpackEncodedString(DFA3_transitionS[i]);
        }
    }

    class DFA3 extends DFA {

        public DFA3(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 3;
            this.eot = DFA3_eot;
            this.eof = DFA3_eof;
            this.min = DFA3_min;
            this.max = DFA3_max;
            this.accept = DFA3_accept;
            this.special = DFA3_special;
            this.transition = DFA3_transition;
        }
        public String getDescription() {
            return "304:1: RBRACE_QUOTE_STRING_LITERAL : ({...}? => '}' DoubleQuoteBody '\"' | {...}? => '}' SingleQuoteBody '\\'' );";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            IntStream input = _input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA3_4 = input.LA(1);

                         
                        int index3_4 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA3_4>='\u0000' && LA3_4<='z')||(LA3_4>='|' && LA3_4<='\uFFFE')) && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 8;}

                        else s = 7;

                         
                        input.seek(index3_4);
                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA3_3 = input.LA(1);

                         
                        int index3_3 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA3_3>='\u0000' && LA3_3<='\uFFFE')) && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 6;}

                         
                        input.seek(index3_3);
                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA3_1 = input.LA(1);

                         
                        int index3_1 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA3_1>='\u0000' && LA3_1<='!')||(LA3_1>='#' && LA3_1<='&')||(LA3_1>='(' && LA3_1<='[')||(LA3_1>=']' && LA3_1<='z')||(LA3_1>='|' && LA3_1<='\uFFFE')) && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 2;}

                        else if ( (LA3_1=='\\') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 3;}

                        else if ( (LA3_1=='\"') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 4;}

                        else if ( (LA3_1=='\'') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 5;}

                         
                        input.seek(index3_1);
                        if ( s>=0 ) return s;
                        break;
                    case 3 : 
                        int LA3_6 = input.LA(1);

                         
                        int index3_6 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA3_6=='\'') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 5;}

                        else if ( (LA3_6=='\"') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 4;}

                        else if ( (LA3_6=='\\') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 3;}

                        else if ( ((LA3_6>='\u0000' && LA3_6<='!')||(LA3_6>='#' && LA3_6<='&')||(LA3_6>='(' && LA3_6<='[')||(LA3_6>=']' && LA3_6<='z')||(LA3_6>='|' && LA3_6<='\uFFFE')) && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 2;}

                         
                        input.seek(index3_6);
                        if ( s>=0 ) return s;
                        break;
                    case 4 : 
                        int LA3_5 = input.LA(1);

                         
                        int index3_5 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA3_5>='\u0000' && LA3_5<='z')||(LA3_5>='|' && LA3_5<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 7;}

                        else s = 8;

                         
                        input.seek(index3_5);
                        if ( s>=0 ) return s;
                        break;
                    case 5 : 
                        int LA3_2 = input.LA(1);

                         
                        int index3_2 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA3_2=='\"') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 4;}

                        else if ( (LA3_2=='\'') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 5;}

                        else if ( (LA3_2=='\\') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 3;}

                        else if ( ((LA3_2>='\u0000' && LA3_2<='!')||(LA3_2>='#' && LA3_2<='&')||(LA3_2>='(' && LA3_2<='[')||(LA3_2>=']' && LA3_2<='z')||(LA3_2>='|' && LA3_2<='\uFFFE')) && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 2;}

                         
                        input.seek(index3_2);
                        if ( s>=0 ) return s;
                        break;
                    case 6 : 
                        int LA3_0 = input.LA(1);

                         
                        int index3_0 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA3_0=='}') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 1;}

                         
                        input.seek(index3_0);
                        if ( s>=0 ) return s;
                        break;
            }
            if (state.backtracking>0) {state.failed=true; return -1;}
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 3, _s, input);
            error(nvae);
            throw nvae;
        }
    }
    static final String DFA4_eotS =
        "\10\uffff";
    static final String DFA4_eofS =
        "\10\uffff";
    static final String DFA4_minS =
        "\1\175\4\0\2\uffff\1\0";
    static final String DFA4_maxS =
        "\1\175\3\ufffe\1\0\2\uffff\1\ufffe";
    static final String DFA4_acceptS =
        "\5\uffff\1\2\1\1\1\uffff";
    static final String DFA4_specialS =
        "\1\2\1\5\1\3\1\1\1\0\2\uffff\1\4}>";
    static final String[] DFA4_transitionS = {
            "\1\1",
            "\42\2\1\5\4\2\1\6\64\2\1\3\36\2\1\4\uff83\2",
            "\42\2\1\5\4\2\1\6\64\2\1\3\36\2\1\4\uff83\2",
            "\uffff\7",
            "\1\uffff",
            "",
            "",
            "\42\2\1\5\4\2\1\6\64\2\1\3\36\2\1\4\uff83\2"
    };

    static final short[] DFA4_eot = DFA.unpackEncodedString(DFA4_eotS);
    static final short[] DFA4_eof = DFA.unpackEncodedString(DFA4_eofS);
    static final char[] DFA4_min = DFA.unpackEncodedStringToUnsignedChars(DFA4_minS);
    static final char[] DFA4_max = DFA.unpackEncodedStringToUnsignedChars(DFA4_maxS);
    static final short[] DFA4_accept = DFA.unpackEncodedString(DFA4_acceptS);
    static final short[] DFA4_special = DFA.unpackEncodedString(DFA4_specialS);
    static final short[][] DFA4_transition;

    static {
        int numStates = DFA4_transitionS.length;
        DFA4_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA4_transition[i] = DFA.unpackEncodedString(DFA4_transitionS[i]);
        }
    }

    class DFA4 extends DFA {

        public DFA4(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 4;
            this.eot = DFA4_eot;
            this.eof = DFA4_eof;
            this.min = DFA4_min;
            this.max = DFA4_max;
            this.accept = DFA4_accept;
            this.special = DFA4_special;
            this.transition = DFA4_transition;
        }
        public String getDescription() {
            return "333:1: RBRACE_LBRACE_STRING_LITERAL : ({...}? => '}' DoubleQuoteBody '{' NextIsPercent[CUR_QUOTE_CTX] | {...}? => '}' SingleQuoteBody '{' NextIsPercent[CUR_QUOTE_CTX] );";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            IntStream input = _input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA4_4 = input.LA(1);

                         
                        int index4_4 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (( rightBraceLikeQuote(DBL_QUOTE_CTX) )) ) {s = 6;}

                        else if ( (( rightBraceLikeQuote(SNG_QUOTE_CTX) )) ) {s = 5;}

                         
                        input.seek(index4_4);
                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA4_3 = input.LA(1);

                         
                        int index4_3 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA4_3>='\u0000' && LA4_3<='\uFFFE')) && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 7;}

                         
                        input.seek(index4_3);
                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA4_0 = input.LA(1);

                         
                        int index4_0 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_0=='}') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 1;}

                         
                        input.seek(index4_0);
                        if ( s>=0 ) return s;
                        break;
                    case 3 : 
                        int LA4_2 = input.LA(1);

                         
                        int index4_2 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_2=='{') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 4;}

                        else if ( ((LA4_2>='\u0000' && LA4_2<='!')||(LA4_2>='#' && LA4_2<='&')||(LA4_2>='(' && LA4_2<='[')||(LA4_2>=']' && LA4_2<='z')||(LA4_2>='|' && LA4_2<='\uFFFE')) && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 2;}

                        else if ( (LA4_2=='\\') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 3;}

                        else if ( (LA4_2=='\'') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 6;}

                        else if ( (LA4_2=='\"') && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 5;}

                         
                        input.seek(index4_2);
                        if ( s>=0 ) return s;
                        break;
                    case 4 : 
                        int LA4_7 = input.LA(1);

                         
                        int index4_7 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_7=='{') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 4;}

                        else if ( ((LA4_7>='\u0000' && LA4_7<='!')||(LA4_7>='#' && LA4_7<='&')||(LA4_7>='(' && LA4_7<='[')||(LA4_7>=']' && LA4_7<='z')||(LA4_7>='|' && LA4_7<='\uFFFE')) && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 2;}

                        else if ( (LA4_7=='\\') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 3;}

                        else if ( (LA4_7=='\'') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 6;}

                        else if ( (LA4_7=='\"') && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 5;}

                         
                        input.seek(index4_7);
                        if ( s>=0 ) return s;
                        break;
                    case 5 : 
                        int LA4_1 = input.LA(1);

                         
                        int index4_1 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA4_1>='\u0000' && LA4_1<='!')||(LA4_1>='#' && LA4_1<='&')||(LA4_1>='(' && LA4_1<='[')||(LA4_1>=']' && LA4_1<='z')||(LA4_1>='|' && LA4_1<='\uFFFE')) && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 2;}

                        else if ( (LA4_1=='\\') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 3;}

                        else if ( (LA4_1=='{') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 4;}

                        else if ( (LA4_1=='\"') && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 5;}

                        else if ( (LA4_1=='\'') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 6;}

                         
                        input.seek(index4_1);
                        if ( s>=0 ) return s;
                        break;
            }
            if (state.backtracking>0) {state.failed=true; return -1;}
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 4, _s, input);
            error(nvae);
            throw nvae;
        }
    }
    static final String DFA53_eotS =
        "\1\uffff\21\65\2\uffff\1\142\1\uffff\1\146\1\150\4\uffff\1\160"+
        "\1\162\1\165\1\170\1\172\1\176\1\u0080\1\57\2\uffff\2\57\1\uffff"+
        "\1\u008c\4\uffff\1\57\1\uffff\3\65\1\u0098\1\u009a\1\uffff\17\65"+
        "\1\u00ad\1\65\1\u00b4\5\65\1\u00bc\1\u00bd\20\65\40\uffff\1\u00d9"+
        "\20\uffff\1\u00de\1\u00e2\2\uffff\2\65\1\u00e9\1\65\1\uffff\1\65"+
        "\1\uffff\7\65\1\u00f3\7\65\1\u00fb\2\65\1\uffff\6\65\1\uffff\2\65"+
        "\1\u0107\1\u0108\1\65\1\u010a\1\65\2\uffff\23\65\1\u011f\2\65\1"+
        "\u0122\3\65\17\uffff\2\65\1\uffff\3\65\1\u012e\5\65\1\uffff\1\65"+
        "\1\u0135\5\65\1\uffff\1\u013b\3\65\1\u013f\2\65\1\u0142\1\65\1\u0144"+
        "\1\u0145\4\uffff\1\u0146\14\65\1\u0153\1\65\1\u0155\1\u0156\2\65"+
        "\1\u0159\1\uffff\2\65\1\uffff\2\65\1\u015e\3\uffff\1\65\1\u0160"+
        "\3\65\1\uffff\1\u0164\1\u0165\1\u0166\1\u0167\2\65\1\uffff\2\65"+
        "\1\u016c\1\65\1\u016e\1\uffff\3\65\1\uffff\2\65\1\uffff\1\65\3\uffff"+
        "\14\65\1\uffff\1\u0181\2\uffff\1\u0182\1\65\1\uffff\1\u0184\1\65"+
        "\1\u0186\1\u0187\1\uffff\1\65\1\uffff\1\u0189\1\65\1\u018b\4\uffff"+
        "\1\65\1\u018d\2\65\1\uffff\1\65\1\uffff\1\65\1\u0192\1\65\1\u0194"+
        "\7\65\1\u019d\2\65\1\u01a0\1\65\1\u01a2\1\u01a3\2\uffff\1\65\1\uffff"+
        "\1\u01a5\2\uffff\1\65\1\uffff\1\65\1\uffff\1\65\1\uffff\1\65\1\u01aa"+
        "\1\u01ab\1\65\1\uffff\1\u01ad\1\uffff\1\65\1\u01af\1\65\1\u01b1"+
        "\1\65\1\u01b3\1\65\2\uffff\1\65\1\u01b8\1\uffff\1\u01b9\2\uffff"+
        "\1\u01ba\1\uffff\1\u01bb\1\65\1\u01bd\1\65\2\uffff\1\u01bf\1\uffff"+
        "\1\65\1\uffff\1\u01c1\1\uffff\1\u01c2\1\uffff\1\65\2\uffff\1\u01c5"+
        "\4\uffff\1\u01c6\1\uffff\1\u01c7\1\uffff\1\65\2\uffff\1\u01c9\4"+
        "\uffff\1\u01cb\3\uffff\1\u01ce\2\uffff";
    static final String DFA53_eofS =
        "\u01cf\uffff";
    static final String DFA53_minS =
        "\1\0\1\142\1\145\1\141\1\145\1\154\1\141\1\146\1\141\1\157\1\145"+
        "\1\156\1\141\1\145\1\151\1\150\1\141\1\150\2\uffff\1\43\1\uffff"+
        "\1\53\1\56\4\uffff\2\75\1\74\1\55\1\75\1\52\1\0\1\75\2\uffff\2\0"+
        "\1\uffff\1\0\4\uffff\1\176\1\uffff\1\163\1\164\1\144\2\44\1\uffff"+
        "\1\146\1\156\1\165\1\145\1\164\1\141\1\156\1\146\1\163\1\143\1\154"+
        "\1\156\1\162\1\157\1\156\1\44\1\160\1\44\1\163\1\144\1\167\1\156"+
        "\1\154\2\44\1\145\1\143\1\163\1\151\1\142\1\141\1\172\1\141\1\160"+
        "\1\145\1\151\1\145\1\160\1\162\1\145\1\164\40\uffff\1\0\5\uffff"+
        "\2\0\2\uffff\2\0\1\uffff\3\0\1\uffff\2\0\2\uffff\1\164\1\145\1\44"+
        "\1\145\1\uffff\1\162\1\uffff\1\157\1\144\1\156\1\141\1\143\1\163"+
        "\1\164\1\44\2\145\1\154\1\145\1\163\1\141\1\163\1\44\1\155\1\143"+
        "\1\uffff\1\157\1\145\1\164\1\145\1\157\1\145\1\uffff\1\164\1\171"+
        "\2\44\1\55\1\44\1\154\2\uffff\1\162\1\153\1\164\1\166\1\164\1\154"+
        "\1\144\1\154\1\165\2\145\1\164\1\160\1\145\1\156\1\163\1\157\1\147"+
        "\1\145\1\44\2\145\1\44\1\162\1\154\1\150\1\uffff\3\0\1\uffff\1\0"+
        "\2\uffff\2\0\2\uffff\2\0\1\uffff\2\162\1\uffff\1\162\1\151\1\162"+
        "\1\44\1\144\1\153\1\150\1\163\1\151\1\uffff\1\164\1\44\1\165\1\156"+
        "\1\145\1\154\1\164\1\uffff\1\44\1\164\1\162\1\170\1\44\1\162\1\141"+
        "\1\44\1\162\2\44\4\uffff\1\44\1\162\1\141\1\151\1\141\1\145\1\151"+
        "\2\141\2\162\1\157\1\151\1\44\1\162\2\44\1\167\1\147\1\44\1\uffff"+
        "\1\156\1\157\1\uffff\2\145\1\44\1\uffff\2\0\1\141\1\44\1\164\1\142"+
        "\1\145\1\uffff\4\44\1\156\1\145\1\uffff\1\163\1\144\1\44\1\154\1"+
        "\44\1\uffff\1\151\1\164\1\157\1\uffff\1\164\1\156\1\uffff\1\163"+
        "\3\uffff\1\151\1\147\1\156\1\164\2\143\1\142\1\143\1\156\1\163\1"+
        "\146\1\143\1\uffff\1\44\2\uffff\1\44\1\145\1\uffff\1\44\1\146\2"+
        "\44\1\uffff\1\143\1\uffff\1\44\1\165\1\44\4\uffff\1\165\1\44\1\151"+
        "\1\163\1\uffff\1\171\1\uffff\1\157\1\44\1\146\1\44\1\143\1\145\1"+
        "\144\1\145\1\151\1\145\1\164\1\44\1\154\1\145\1\44\1\145\2\44\2"+
        "\uffff\1\162\1\uffff\1\44\2\uffff\1\164\1\uffff\1\164\1\uffff\1"+
        "\145\1\uffff\1\166\2\44\1\156\1\uffff\1\44\1\uffff\1\145\1\44\1"+
        "\145\1\44\1\164\1\44\1\145\1\151\1\uffff\1\145\1\44\1\uffff\1\44"+
        "\2\uffff\1\44\1\uffff\1\44\1\145\1\44\1\145\2\uffff\1\44\1\uffff"+
        "\1\157\1\uffff\1\44\1\uffff\1\44\1\uffff\1\144\1\uffff\1\145\1\44"+
        "\4\uffff\1\44\1\uffff\1\44\1\uffff\1\146\2\uffff\1\44\1\141\3\uffff"+
        "\1\44\1\uffff\1\144\1\uffff\1\141\2\uffff";
    static final String DFA53_maxS =
        "\1\ufffe\1\164\1\162\1\157\1\145\1\170\1\165\1\156\1\141\1\157"+
        "\1\165\1\166\1\165\1\145\1\165\1\171\1\141\1\151\2\uffff\1\43\1"+
        "\uffff\1\75\1\71\4\uffff\1\76\1\75\1\76\3\75\1\ufffe\1\75\2\uffff"+
        "\2\ufffe\1\uffff\1\ufffe\4\uffff\1\176\1\uffff\1\163\1\164\1\144"+
        "\2\ufaff\1\uffff\1\146\1\156\1\165\1\145\1\164\1\141\1\156\1\154"+
        "\1\163\1\164\1\154\2\162\1\157\1\156\1\ufaff\1\160\1\ufaff\1\172"+
        "\1\144\1\167\1\164\1\154\2\ufaff\1\145\1\143\1\163\1\157\1\142\1"+
        "\166\1\172\1\145\1\160\1\162\1\171\1\145\1\160\1\162\1\151\1\164"+
        "\40\uffff\1\ufffe\5\uffff\2\ufffe\2\uffff\2\ufffe\1\uffff\1\0\2"+
        "\ufffe\1\uffff\2\ufffe\2\uffff\1\164\1\145\1\ufaff\1\145\1\uffff"+
        "\1\162\1\uffff\1\157\1\144\1\156\1\141\1\143\1\163\1\164\1\ufaff"+
        "\2\145\1\154\1\145\1\163\1\141\1\163\1\ufaff\1\155\1\143\1\uffff"+
        "\1\157\1\145\2\164\1\157\1\145\1\uffff\1\164\1\171\2\ufaff\1\55"+
        "\1\ufaff\1\154\2\uffff\1\162\1\153\1\164\1\166\1\164\1\154\1\144"+
        "\1\154\1\165\2\145\1\164\1\160\1\145\1\156\1\163\1\157\1\147\1\145"+
        "\1\ufaff\2\145\1\ufaff\1\162\1\154\1\150\1\uffff\1\0\2\ufffe\1\uffff"+
        "\1\ufffe\2\uffff\2\ufffe\2\uffff\2\ufffe\1\uffff\2\162\1\uffff\1"+
        "\162\1\151\1\162\1\ufaff\1\144\1\153\1\150\1\163\1\151\1\uffff\1"+
        "\164\1\ufaff\1\165\1\156\1\145\1\154\1\164\1\uffff\1\ufaff\1\164"+
        "\1\162\1\170\1\ufaff\1\162\1\141\1\ufaff\1\162\2\ufaff\4\uffff\1"+
        "\ufaff\1\162\1\141\1\151\1\141\1\145\1\151\2\141\2\162\1\157\1\151"+
        "\1\ufaff\1\162\2\ufaff\1\167\1\147\1\ufaff\1\uffff\1\156\1\157\1"+
        "\uffff\2\145\1\ufaff\1\uffff\2\ufffe\1\141\1\ufaff\1\164\1\142\1"+
        "\145\1\uffff\4\ufaff\1\156\1\145\1\uffff\1\163\1\144\1\ufaff\1\154"+
        "\1\ufaff\1\uffff\1\151\1\164\1\157\1\uffff\1\164\1\156\1\uffff\1"+
        "\163\3\uffff\1\151\1\147\1\156\1\164\2\143\1\142\1\143\1\156\1\163"+
        "\1\146\1\143\1\uffff\1\ufaff\2\uffff\1\ufaff\1\145\1\uffff\1\ufaff"+
        "\1\146\2\ufaff\1\uffff\1\143\1\uffff\1\ufaff\1\165\1\ufaff\4\uffff"+
        "\1\165\1\ufaff\1\151\1\163\1\uffff\1\171\1\uffff\1\157\1\ufaff\1"+
        "\146\1\ufaff\1\143\1\145\1\144\1\145\1\151\1\145\1\164\1\ufaff\1"+
        "\154\1\145\1\ufaff\1\145\2\ufaff\2\uffff\1\162\1\uffff\1\ufaff\2"+
        "\uffff\1\164\1\uffff\1\164\1\uffff\1\145\1\uffff\1\166\2\ufaff\1"+
        "\156\1\uffff\1\ufaff\1\uffff\1\145\1\ufaff\1\145\1\ufaff\1\164\1"+
        "\ufaff\1\145\1\162\1\uffff\1\145\1\ufaff\1\uffff\1\ufaff\2\uffff"+
        "\1\ufaff\1\uffff\1\ufaff\1\145\1\ufaff\1\145\2\uffff\1\ufaff\1\uffff"+
        "\1\157\1\uffff\1\ufaff\1\uffff\1\ufaff\1\uffff\1\144\1\uffff\1\145"+
        "\1\ufaff\4\uffff\1\ufaff\1\uffff\1\ufaff\1\uffff\1\146\2\uffff\1"+
        "\ufaff\1\141\3\uffff\1\ufaff\1\uffff\1\144\1\uffff\1\141\2\uffff";
    static final String DFA53_acceptS =
        "\22\uffff\1\111\1\112\1\uffff\1\114\2\uffff\1\117\1\120\1\121\1"+
        "\122\10\uffff\1\146\1\147\2\uffff\1\154\1\uffff\2\162\1\163\1\164"+
        "\1\uffff\1\170\5\uffff\1\163\51\uffff\1\111\1\112\1\161\1\113\1"+
        "\114\1\115\1\140\1\133\1\116\1\123\1\162\1\117\1\120\1\121\1\122"+
        "\1\124\1\150\1\125\1\132\1\126\1\130\1\131\1\127\1\141\1\151\1\134"+
        "\1\142\1\135\1\143\1\165\1\166\1\136\1\uffff\1\137\1\160\1\145\1"+
        "\146\1\147\2\uffff\1\153\1\152\2\uffff\1\154\3\uffff\1\156\2\uffff"+
        "\1\164\1\167\4\uffff\1\4\1\uffff\1\6\22\uffff\1\32\6\uffff\1\35"+
        "\7\uffff\1\52\1\53\32\uffff\1\116\3\uffff\1\157\1\uffff\1\155\1"+
        "\156\2\uffff\2\155\2\uffff\1\156\2\uffff\1\3\11\uffff\1\17\7\uffff"+
        "\1\27\13\uffff\1\45\1\46\1\47\1\50\24\uffff\1\102\2\uffff\1\105"+
        "\3\uffff\1\144\7\uffff\1\11\6\uffff\1\21\5\uffff\1\30\3\uffff\1"+
        "\36\2\uffff\1\41\1\uffff\1\43\1\44\1\51\14\uffff\1\73\1\uffff\1"+
        "\75\1\76\2\uffff\1\101\4\uffff\1\110\1\uffff\1\2\3\uffff\1\12\1"+
        "\13\1\14\1\15\4\uffff\1\24\1\uffff\1\26\22\uffff\1\74\1\77\1\uffff"+
        "\1\103\1\uffff\1\106\1\107\1\uffff\1\5\1\uffff\1\10\1\uffff\1\20"+
        "\4\uffff\1\33\1\uffff\1\37\10\uffff\1\62\2\uffff\1\67\1\uffff\1"+
        "\71\1\72\1\uffff\1\104\4\uffff\1\23\1\25\1\uffff\1\34\1\uffff\1"+
        "\42\1\uffff\1\55\1\uffff\1\57\1\uffff\1\61\2\uffff\1\66\1\70\1\100"+
        "\1\1\1\uffff\1\16\1\uffff\1\31\1\uffff\1\54\1\56\2\uffff\1\65\1"+
        "\7\1\22\1\uffff\1\60\1\uffff\1\40\1\uffff\1\63\1\64";
    static final String DFA53_specialS =
        "\42\uffff\1\15\6\uffff\1\16\125\uffff\1\3\14\uffff\1\10\1\4\1\7"+
        "\1\uffff\1\6\1\1\107\uffff\1\12\3\uffff\1\0\2\uffff\1\11\1\2\2\uffff"+
        "\1\17\1\5\101\uffff\1\14\1\13\u00a6\uffff}>";
    static final String[] DFA53_transitionS = {
            "\11\57\2\55\1\57\2\55\22\57\1\55\1\43\1\46\1\24\1\54\1\42\1"+
            "\57\1\47\1\23\1\30\1\40\1\26\1\33\1\37\1\27\1\41\1\52\11\53"+
            "\1\44\1\32\1\36\1\34\1\35\1\45\1\57\32\54\1\22\1\57\1\31\1\57"+
            "\1\54\1\57\1\1\1\2\1\3\1\4\1\5\1\6\2\54\1\7\2\54\1\10\1\11\1"+
            "\12\1\13\1\14\1\54\1\15\1\16\1\17\1\54\1\20\1\21\3\54\1\50\1"+
            "\25\1\51\1\56\101\57\27\54\1\57\37\54\1\57\u1f08\54\u1040\57"+
            "\u0150\54\u0170\57\u0080\54\u0080\57\u092e\54\u10d2\57\u5200"+
            "\54\u5900\57\u0200\54\u04ff\57",
            "\1\60\3\uffff\1\61\7\uffff\1\62\4\uffff\1\63\1\64",
            "\1\66\3\uffff\1\67\5\uffff\1\70\2\uffff\1\71",
            "\1\72\12\uffff\1\73\2\uffff\1\74",
            "\1\75",
            "\1\76\13\uffff\1\77",
            "\1\100\7\uffff\1\101\5\uffff\1\102\2\uffff\1\103\2\uffff\1"+
            "\104",
            "\1\105\6\uffff\1\106\1\107",
            "\1\110",
            "\1\111",
            "\1\112\11\uffff\1\113\5\uffff\1\114",
            "\1\115\3\uffff\1\116\3\uffff\1\117",
            "\1\120\15\uffff\1\121\2\uffff\1\122\2\uffff\1\123",
            "\1\124",
            "\1\125\12\uffff\1\126\1\127",
            "\1\130\11\uffff\1\131\4\uffff\1\132\1\uffff\1\133",
            "\1\134",
            "\1\135\1\136",
            "",
            "",
            "\1\141",
            "",
            "\1\144\21\uffff\1\145",
            "\1\147\1\uffff\12\151",
            "",
            "",
            "",
            "",
            "\1\156\1\157",
            "\1\161",
            "\1\65\1\164\1\163",
            "\1\167\17\uffff\1\166",
            "\1\171",
            "\1\174\4\uffff\1\175\15\uffff\1\173",
            "\40\u0081\1\uffff\34\u0081\1\177\uffc1\u0081",
            "\1\u0082",
            "",
            "",
            "\42\u0085\1\u0088\71\u0085\1\u0086\36\u0085\1\u0087\uff83"+
            "\u0085",
            "\47\u0089\1\u0088\64\u0089\1\u008a\36\u0089\1\u0087\uff83"+
            "\u0089",
            "",
            "\42\u008d\1\u0090\4\u008d\1\u0091\64\u008d\1\u008e\36\u008d"+
            "\1\u008f\uff83\u008d",
            "",
            "",
            "",
            "",
            "\1\u0093",
            "",
            "\1\u0094",
            "\1\u0095",
            "\1\u0096",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\22"+
            "\65\1\u0097\7\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08"+
            "\65\u1040\uffff\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e"+
            "\65\u10d2\uffff\u5200\65\u5900\uffff\u0200\65",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\23"+
            "\65\1\u0099\6\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08"+
            "\65\u1040\uffff\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e"+
            "\65\u10d2\uffff\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\u009b",
            "\1\u009c",
            "\1\u009d",
            "\1\u009e",
            "\1\u009f",
            "\1\u00a0",
            "\1\u00a1",
            "\1\u00a2\5\uffff\1\u00a3",
            "\1\u00a4",
            "\1\u00a5\20\uffff\1\u00a6",
            "\1\u00a7",
            "\1\u00a8\3\uffff\1\u00a9",
            "\1\u00aa",
            "\1\u00ab",
            "\1\u00ac",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u00ae",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\3"+
            "\65\1\u00af\4\65\1\u00b0\11\65\1\u00b1\1\u00b2\1\65\1\u00b3"+
            "\4\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u00b5\6\uffff\1\u00b6",
            "\1\u00b7",
            "\1\u00b8",
            "\1\u00b9\5\uffff\1\u00ba",
            "\1\u00bb",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u00be",
            "\1\u00bf",
            "\1\u00c0",
            "\1\u00c1\5\uffff\1\u00c2",
            "\1\u00c3",
            "\1\u00c4\16\uffff\1\u00c5\3\uffff\1\u00c6\1\uffff\1\u00c7",
            "\1\u00c8",
            "\1\u00c9\3\uffff\1\u00ca",
            "\1\u00cb",
            "\1\u00cc\3\uffff\1\u00cd\10\uffff\1\u00ce",
            "\1\u00cf\13\uffff\1\u00d0\3\uffff\1\u00d1",
            "\1\u00d2",
            "\1\u00d3",
            "\1\u00d4",
            "\1\u00d5\3\uffff\1\u00d6",
            "\1\u00d7",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "\40\u0081\1\uffff\uffde\u0081",
            "",
            "",
            "",
            "",
            "",
            "\42\u0085\1\u0088\71\u0085\1\u0086\36\u0085\1\u0087\uff83"+
            "\u0085",
            "\uffff\u00da",
            "",
            "",
            "\47\u0089\1\u0088\64\u0089\1\u008a\36\u0089\1\u0087\uff83"+
            "\u0089",
            "\uffff\u00db",
            "",
            "\1\uffff",
            "\42\u008d\1\u0090\4\u008d\1\u0091\64\u008d\1\u008e\36\u008d"+
            "\1\u008f\uff83\u008d",
            "\uffff\u00dd",
            "",
            "\47\u00e0\1\u00e2\64\u00e0\1\u00e1\36\u00e0\1\u00df\uff83"+
            "\u00e0",
            "\42\u00e4\1\u00e3\71\u00e4\1\u00e5\36\u00e4\1\u00e6\uff83"+
            "\u00e4",
            "",
            "",
            "\1\u00e7",
            "\1\u00e8",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u00ea",
            "",
            "\1\u00eb",
            "",
            "\1\u00ec",
            "\1\u00ed",
            "\1\u00ee",
            "\1\u00ef",
            "\1\u00f0",
            "\1\u00f1",
            "\1\u00f2",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u00f4",
            "\1\u00f5",
            "\1\u00f6",
            "\1\u00f7",
            "\1\u00f8",
            "\1\u00f9",
            "\1\u00fa",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u00fc",
            "\1\u00fd",
            "",
            "\1\u00fe",
            "\1\u00ff",
            "\1\u0100",
            "\1\u0101\16\uffff\1\u0102",
            "\1\u0103",
            "\1\u0104",
            "",
            "\1\u0105",
            "\1\u0106",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0109",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u010b",
            "",
            "",
            "\1\u010c",
            "\1\u010d",
            "\1\u010e",
            "\1\u010f",
            "\1\u0110",
            "\1\u0111",
            "\1\u0112",
            "\1\u0113",
            "\1\u0114",
            "\1\u0115",
            "\1\u0116",
            "\1\u0117",
            "\1\u0118",
            "\1\u0119",
            "\1\u011a",
            "\1\u011b",
            "\1\u011c",
            "\1\u011d",
            "\1\u011e",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0120",
            "\1\u0121",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0123",
            "\1\u0124",
            "\1\u0125",
            "",
            "\1\uffff",
            "\42\u0085\1\u0088\71\u0085\1\u0086\36\u0085\1\u0087\uff83"+
            "\u0085",
            "\47\u0089\1\u0088\64\u0089\1\u008a\36\u0089\1\u0087\uff83"+
            "\u0089",
            "",
            "\42\u008d\1\u0090\4\u008d\1\u0091\64\u008d\1\u008e\36\u008d"+
            "\1\u008f\uff83\u008d",
            "",
            "",
            "\47\u00e0\1\u00e2\64\u00e0\1\u00e1\36\u00e0\1\u00df\uff83"+
            "\u00e0",
            "\uffff\u0127",
            "",
            "",
            "\42\u00e4\1\u00e3\71\u00e4\1\u00e5\36\u00e4\1\u00e6\uff83"+
            "\u00e4",
            "\uffff\u0128",
            "",
            "\1\u0129",
            "\1\u012a",
            "",
            "\1\u012b",
            "\1\u012c",
            "\1\u012d",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u012f",
            "\1\u0130",
            "\1\u0131",
            "\1\u0132",
            "\1\u0133",
            "",
            "\1\u0134",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0136",
            "\1\u0137",
            "\1\u0138",
            "\1\u0139",
            "\1\u013a",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u013c",
            "\1\u013d",
            "\1\u013e",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0140",
            "\1\u0141",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0143",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "",
            "",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0147",
            "\1\u0148",
            "\1\u0149",
            "\1\u014a",
            "\1\u014b",
            "\1\u014c",
            "\1\u014d",
            "\1\u014e",
            "\1\u014f",
            "\1\u0150",
            "\1\u0151",
            "\1\u0152",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0154",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0157",
            "\1\u0158",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\u015a",
            "\1\u015b",
            "",
            "\1\u015c",
            "\1\u015d",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\47\u00e0\1\u00e2\64\u00e0\1\u00e1\36\u00e0\1\u00df\uff83"+
            "\u00e0",
            "\42\u00e4\1\u00e3\71\u00e4\1\u00e5\36\u00e4\1\u00e6\uff83"+
            "\u00e4",
            "\1\u015f",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0161",
            "\1\u0162",
            "\1\u0163",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0168",
            "\1\u0169",
            "",
            "\1\u016a",
            "\1\u016b",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u016d",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\u016f",
            "\1\u0170",
            "\1\u0171",
            "",
            "\1\u0172",
            "\1\u0173",
            "",
            "\1\u0174",
            "",
            "",
            "",
            "\1\u0175",
            "\1\u0176",
            "\1\u0177",
            "\1\u0178",
            "\1\u0179",
            "\1\u017a",
            "\1\u017b",
            "\1\u017c",
            "\1\u017d",
            "\1\u017e",
            "\1\u017f",
            "\1\u0180",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0183",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0185",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\u0188",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u018a",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "",
            "",
            "",
            "\1\u018c",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u018e",
            "\1\u018f",
            "",
            "\1\u0190",
            "",
            "\1\u0191",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0193",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u0195",
            "\1\u0196",
            "\1\u0197",
            "\1\u0198",
            "\1\u0199",
            "\1\u019a",
            "\1\u019b",
            "\1\65\10\uffff\1\u019c\2\uffff\12\65\7\uffff\32\65\4\uffff"+
            "\1\65\1\uffff\32\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08"+
            "\65\u1040\uffff\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e"+
            "\65\u10d2\uffff\u5200\65\u5900\uffff\u0200\65",
            "\1\u019e",
            "\1\u019f",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u01a1",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "",
            "\1\u01a4",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "",
            "\1\u01a6",
            "",
            "\1\u01a7",
            "",
            "\1\u01a8",
            "",
            "\1\u01a9",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u01ac",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\u01ae",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u01b0",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u01b2",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u01b4",
            "\1\u01b5\10\uffff\1\u01b6",
            "",
            "\1\u01b7",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u01bc",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u01be",
            "",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\u01c0",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\u01c3",
            "",
            "\1\u01c4",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "",
            "",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\u01c8",
            "",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "\1\u01ca",
            "",
            "",
            "",
            "\1\65\13\uffff\12\65\7\uffff\32\65\4\uffff\1\65\1\uffff\32"+
            "\65\105\uffff\27\65\1\uffff\37\65\1\uffff\u1f08\65\u1040\uffff"+
            "\u0150\65\u0170\uffff\u0080\65\u0080\uffff\u092e\65\u10d2\uffff"+
            "\u5200\65\u5900\uffff\u0200\65",
            "",
            "\1\u01cc",
            "",
            "\1\u01cd",
            "",
            ""
    };

    static final short[] DFA53_eot = DFA.unpackEncodedString(DFA53_eotS);
    static final short[] DFA53_eof = DFA.unpackEncodedString(DFA53_eofS);
    static final char[] DFA53_min = DFA.unpackEncodedStringToUnsignedChars(DFA53_minS);
    static final char[] DFA53_max = DFA.unpackEncodedStringToUnsignedChars(DFA53_maxS);
    static final short[] DFA53_accept = DFA.unpackEncodedString(DFA53_acceptS);
    static final short[] DFA53_special = DFA.unpackEncodedString(DFA53_specialS);
    static final short[][] DFA53_transition;

    static {
        int numStates = DFA53_transitionS.length;
        DFA53_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA53_transition[i] = DFA.unpackEncodedString(DFA53_transitionS[i]);
        }
    }

    class DFA53 extends DFA {

        public DFA53(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 53;
            this.eot = DFA53_eot;
            this.eof = DFA53_eof;
            this.min = DFA53_min;
            this.max = DFA53_max;
            this.accept = DFA53_accept;
            this.special = DFA53_special;
            this.transition = DFA53_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( ABSTRACT | AFTER | AND | AS | ASSERT | AT | ATTRIBUTE | BEFORE | BIND | BOUND | BREAK | CATCH | CLASS | CONTINUE | DEF | DELETE | ELSE | EXCLUSIVE | EXTENDS | FALSE | FINALLY | FIRST | FOR | FROM | FUNCTION | IF | IMPORT | INDEXOF | IN | INIT | INSERT | INSTANCEOF | INTO | INVERSE | LAST | LAZY | MOD | NEW | NON_WRITABLE | NOT | NULL | ON | OR | OVERRIDE | PACKAGE | POSTINIT | PRIVATE | PROTECTED | PUBLIC_INIT | PUBLIC | PUBLIC_READABLE | PUBLIC_READ | READABLE | REPLACE | RETURN | REVERSE | SIZEOF | STATIC | STEP | SUPER | THEN | THIS | THROW | TRIGGER | TRUE | TRY | TWEEN | TYPEOF | VAR | WHERE | WHILE | WITH | LBRACKET | LPAREN | POUND | PIPE | PLUSPLUS | DOTDOT | RPAREN | RBRACKET | SEMI | COMMA | DOT | EQEQ | EQ | GT | LT | LTGT | LTEQ | GTEQ | PLUS | SUB | STAR | SLASH | PERCENT | PLUSEQ | SUBEQ | STAREQ | SLASHEQ | PERCENTEQ | NOTEQ | COLON | QUES | SUCHTHAT | SUBSUB | STRING_LITERAL | QUOTE_LBRACE_STRING_LITERAL | LBRACE | RBRACE_QUOTE_STRING_LITERAL | RBRACE_LBRACE_STRING_LITERAL | RBRACE | FORMAT_STRING_LITERAL | TRANSLATION_KEY | FLOATING_POINT_LITERAL | IDENTIFIER | WS | COMMENT | LINE_COMMENT | LAST_TOKEN | INVALIDC );";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            IntStream input = _input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA53_221 = input.LA(1);

                         
                        int index53_221 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA53_221=='\"') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 144;}

                        else if ( ((LA53_221>='\u0000' && LA53_221<='!')||(LA53_221>='#' && LA53_221<='&')||(LA53_221>='(' && LA53_221<='[')||(LA53_221>=']' && LA53_221<='z')||(LA53_221>='|' && LA53_221<='\uFFFE')) && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 141;}

                        else if ( (LA53_221=='\\') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 142;}

                        else if ( (LA53_221=='{') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 143;}

                        else if ( (LA53_221=='\'') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 145;}

                         
                        input.seek(index53_221);
                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA53_145 = input.LA(1);

                         
                        int index53_145 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA53_145=='\"') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 227;}

                        else if ( ((LA53_145>='\u0000' && LA53_145<='!')||(LA53_145>='#' && LA53_145<='[')||(LA53_145>=']' && LA53_145<='z')||(LA53_145>='|' && LA53_145<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 228;}

                        else if ( (LA53_145=='\\') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 229;}

                        else if ( (LA53_145=='{') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 230;}

                        else s = 226;

                         
                        input.seek(index53_145);
                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA53_225 = input.LA(1);

                         
                        int index53_225 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA53_225>='\u0000' && LA53_225<='\uFFFE')) && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 295;}

                         
                        input.seek(index53_225);
                        if ( s>=0 ) return s;
                        break;
                    case 3 : 
                        int LA53_127 = input.LA(1);

                         
                        int index53_127 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA53_127>='\u0000' && LA53_127<='\u001F')||(LA53_127>='!' && LA53_127<='\uFFFE')) && (( percentIsFormat() ))) {s = 129;}

                        else s = 217;

                         
                        input.seek(index53_127);
                        if ( s>=0 ) return s;
                        break;
                    case 4 : 
                        int LA53_141 = input.LA(1);

                         
                        int index53_141 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA53_141=='{') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 143;}

                        else if ( (LA53_141=='\"') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 144;}

                        else if ( (LA53_141=='\\') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 142;}

                        else if ( (LA53_141=='\'') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 145;}

                        else if ( ((LA53_141>='\u0000' && LA53_141<='!')||(LA53_141>='#' && LA53_141<='&')||(LA53_141>='(' && LA53_141<='[')||(LA53_141>=']' && LA53_141<='z')||(LA53_141>='|' && LA53_141<='\uFFFE')) && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 141;}

                         
                        input.seek(index53_141);
                        if ( s>=0 ) return s;
                        break;
                    case 5 : 
                        int LA53_229 = input.LA(1);

                         
                        int index53_229 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA53_229>='\u0000' && LA53_229<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 296;}

                         
                        input.seek(index53_229);
                        if ( s>=0 ) return s;
                        break;
                    case 6 : 
                        int LA53_144 = input.LA(1);

                         
                        int index53_144 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA53_144=='{') && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 223;}

                        else if ( ((LA53_144>='\u0000' && LA53_144<='&')||(LA53_144>='(' && LA53_144<='[')||(LA53_144>=']' && LA53_144<='z')||(LA53_144>='|' && LA53_144<='\uFFFE')) && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 224;}

                        else if ( (LA53_144=='\\') && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 225;}

                        else if ( (LA53_144=='\'') && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 226;}

                        else s = 222;

                         
                        input.seek(index53_144);
                        if ( s>=0 ) return s;
                        break;
                    case 7 : 
                        int LA53_142 = input.LA(1);

                         
                        int index53_142 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA53_142>='\u0000' && LA53_142<='\uFFFE')) && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 221;}

                         
                        input.seek(index53_142);
                        if ( s>=0 ) return s;
                        break;
                    case 8 : 
                        int LA53_140 = input.LA(1);

                         
                        int index53_140 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (( !rightBraceLikeQuote(CUR_QUOTE_CTX) )) ) {s = 220;}

                        else if ( (true) ) {s = 47;}

                         
                        input.seek(index53_140);
                        if ( s>=0 ) return s;
                        break;
                    case 9 : 
                        int LA53_224 = input.LA(1);

                         
                        int index53_224 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA53_224=='{') && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 223;}

                        else if ( ((LA53_224>='\u0000' && LA53_224<='&')||(LA53_224>='(' && LA53_224<='[')||(LA53_224>=']' && LA53_224<='z')||(LA53_224>='|' && LA53_224<='\uFFFE')) && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 224;}

                        else if ( (LA53_224=='\\') && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 225;}

                        else if ( (LA53_224=='\'') && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 226;}

                         
                        input.seek(index53_224);
                        if ( s>=0 ) return s;
                        break;
                    case 10 : 
                        int LA53_217 = input.LA(1);

                         
                        int index53_217 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (!((( percentIsFormat() )))) ) {s = 294;}

                        else if ( (( percentIsFormat() )) ) {s = 129;}

                         
                        input.seek(index53_217);
                        if ( s>=0 ) return s;
                        break;
                    case 11 : 
                        int LA53_296 = input.LA(1);

                         
                        int index53_296 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA53_296=='\"') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 227;}

                        else if ( ((LA53_296>='\u0000' && LA53_296<='!')||(LA53_296>='#' && LA53_296<='[')||(LA53_296>=']' && LA53_296<='z')||(LA53_296>='|' && LA53_296<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 228;}

                        else if ( (LA53_296=='\\') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 229;}

                        else if ( (LA53_296=='{') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 230;}

                         
                        input.seek(index53_296);
                        if ( s>=0 ) return s;
                        break;
                    case 12 : 
                        int LA53_295 = input.LA(1);

                         
                        int index53_295 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA53_295=='{') && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 223;}

                        else if ( ((LA53_295>='\u0000' && LA53_295<='&')||(LA53_295>='(' && LA53_295<='[')||(LA53_295>=']' && LA53_295<='z')||(LA53_295>='|' && LA53_295<='\uFFFE')) && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 224;}

                        else if ( (LA53_295=='\\') && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 225;}

                        else if ( (LA53_295=='\'') && (( rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 226;}

                         
                        input.seek(index53_295);
                        if ( s>=0 ) return s;
                        break;
                    case 13 : 
                        int LA53_34 = input.LA(1);

                         
                        int index53_34 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA53_34=='=') ) {s = 127;}

                        else if ( ((LA53_34>='\u0000' && LA53_34<='\u001F')||(LA53_34>='!' && LA53_34<='<')||(LA53_34>='>' && LA53_34<='\uFFFE')) && (( percentIsFormat() ))) {s = 129;}

                        else s = 128;

                         
                        input.seek(index53_34);
                        if ( s>=0 ) return s;
                        break;
                    case 14 : 
                        int LA53_41 = input.LA(1);

                         
                        int index53_41 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA53_41>='\u0000' && LA53_41<='!')||(LA53_41>='#' && LA53_41<='&')||(LA53_41>='(' && LA53_41<='[')||(LA53_41>=']' && LA53_41<='z')||(LA53_41>='|' && LA53_41<='\uFFFE')) && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 141;}

                        else if ( (LA53_41=='\\') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 142;}

                        else if ( (LA53_41=='{') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 143;}

                        else if ( (LA53_41=='\"') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 144;}

                        else if ( (LA53_41=='\'') && ((( rightBraceLikeQuote(DBL_QUOTE_CTX) )||( rightBraceLikeQuote(SNG_QUOTE_CTX) )))) {s = 145;}

                        else s = 140;

                         
                        input.seek(index53_41);
                        if ( s>=0 ) return s;
                        break;
                    case 15 : 
                        int LA53_228 = input.LA(1);

                         
                        int index53_228 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA53_228=='\"') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 227;}

                        else if ( ((LA53_228>='\u0000' && LA53_228<='!')||(LA53_228>='#' && LA53_228<='[')||(LA53_228>=']' && LA53_228<='z')||(LA53_228>='|' && LA53_228<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 228;}

                        else if ( (LA53_228=='\\') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 229;}

                        else if ( (LA53_228=='{') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) ))) {s = 230;}

                         
                        input.seek(index53_228);
                        if ( s>=0 ) return s;
                        break;
            }
            if (state.backtracking>0) {state.failed=true; return -1;}
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 53, _s, input);
            error(nvae);
            throw nvae;
        }
    }
 

}