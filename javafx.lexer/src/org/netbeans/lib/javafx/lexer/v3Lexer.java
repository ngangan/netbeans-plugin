// $ANTLR 3.1b1 E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g 2008-07-11 11:41:08

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
public class v3Lexer extends Lexer {
    public static final int COMMA=83;
    public static final int LAZY=69;
    public static final int EXPR_LIST=120;
    public static final int SEQ_INDEX=125;
    public static final int AS=55;
    public static final int HexDigit=158;
    public static final int SEQ_SLICE_EXCLUSIVE=127;
    public static final int NOTEQ=102;
    public static final int INTO=66;
    public static final int TranslationKeyBody=151;
    public static final int FALSE=15;
    public static final int ABSTRACT=5;
    public static final int THEN=75;
    public static final int STEP=74;
    public static final int PLUSPLUS=49;
    public static final int IMPORT=19;
    public static final int PACKAGE=28;
    public static final int PIPE=51;
    public static final int SIZEOF=37;
    public static final int ON=71;
    public static final int CONTINUE=13;
    public static final int DOT=84;
    public static final int SingleQuoteBody=142;
    public static final int PRIVATE=30;
    public static final int Letter=162;
    public static final int AND=54;
    public static final int EXPRESSION=114;
    public static final int TYPED_ARG_LIST=138;
    public static final int FUNCTION=17;
    public static final int TRIGGER=76;
    public static final int EMPTY_MODULE_ITEM=108;
    public static final int STRING_LITERAL=143;
    public static final int RBRACKET=81;
    public static final int MODULE=107;
    public static final int RPAREN=80;
    public static final int SEMI_INSERT_START=4;
    public static final int ASSERT=6;
    public static final int RBRACE_LBRACE_STRING_LITERAL=148;
    public static final int PLUS=92;
    public static final int OBJECT_LIT=128;
    public static final int ON_REPLACE=119;
    public static final int FINALLY=61;
    public static final int EXTENDS=60;
    public static final int AT=7;
    public static final int TIME_LITERAL=156;
    public static final int SUPER=36;
    public static final int DECIMAL_LITERAL=153;
    public static final int SLICE_CLAUSE=117;
    public static final int WS=165;
    public static final int NEW=24;
    public static final int SUBSUB=50;
    public static final int EQ=86;
    public static final int FUNC_EXPR=112;
    public static final int EXCLUSIVE=59;
    public static final int LT=88;
    public static final int BOUND=10;
    public static final int LINE_COMMENT=167;
    public static final int RangeDots=160;
    public static final int NEGATIVE=122;
    public static final int EQEQ=85;
    public static final int QUOTE_LBRACE_STRING_LITERAL=145;
    public static final int FLOATING_POINT_LITERAL=161;
    public static final int TYPE_ANY=135;
    public static final int STATIC=38;
    public static final int CATCH=57;
    public static final int SEMI=82;
    public static final int ELSE=58;
    public static final int INDEXOF=20;
    public static final int FORMAT_STRING_LITERAL=150;
    public static final int LTEQ=90;
    public static final int BREAK=11;
    public static final int FIRST=62;
    public static final int NULL=26;
    public static final int QUES=104;
    public static final int COLON=103;
    public static final int DOTDOT=79;
    public static final int IDENTIFIER=164;
    public static final int NextIsPercent=144;
    public static final int TYPE_UNKNOWN=136;
    public static final int INSERT=22;
    public static final int TRUE=42;
    public static final int DOC_COMMENT=139;
    public static final int POUND=46;
    public static final int THROW=40;
    public static final int POSTINIT=29;
    public static final int WHERE=78;
    public static final int POSTINCR=123;
    public static final int PUBLIC=32;
    public static final int OBJECT_LIT_PART=129;
    public static final int LTGT=89;
    public static final int STATEMENT=113;
    public static final int TYPEOF=43;
    public static final int PERCENT=96;
    public static final int LAST=68;
    public static final int SEQ_EMPTY=130;
    public static final int READONLY=33;
    public static final int LBRACKET=48;
    public static final int MOD=70;
    public static final int INIT=21;
    public static final int OCTAL_LITERAL=157;
    public static final int SEQ_SLICE=126;
    public static final int FUNC_APPLY=121;
    public static final int HEX_LITERAL=159;
    public static final int OR=72;
    public static final int AFTER=53;
    public static final int LBRACE=146;
    public static final int BLOCK=115;
    public static final int RBRACE=149;
    public static final int PROTECTED=31;
    public static final int EMPTY_FORMAT_STRING=132;
    public static final int INVERSE=67;
    public static final int SUBEQ=98;
    public static final int TYPE_NAMED=133;
    public static final int INSTANCEOF=65;
    public static final int POSTDECR=124;
    public static final int TRANSLATION_KEY=152;
    public static final int PARAM=111;
    public static final int ON_REPLACE_SLICE=118;
    public static final int LPAREN=47;
    public static final int SLASHEQ=100;
    public static final int DoubleQuoteBody=141;
    public static final int FROM=63;
    public static final int DELETE=14;
    public static final int PERCENTEQ=101;
    public static final int Exponent=155;
    public static final int SLASH=95;
    public static final int WHILE=45;
    public static final int STAREQ=99;
    public static final int CLASS_MEMBERS=110;
    public static final int PLUSEQ=97;
    public static final int REPLACE=73;
    public static final int GT=87;
    public static final int COMMENT=166;
    public static final int OVERRIDE=27;
    public static final int GTEQ=91;
    public static final int THIS=39;
    public static final int SEQ_EXPLICIT=131;
    public static final int WITH=77;
    public static final int IN=64;
    public static final int REVERSE=35;
    public static final int VAR=44;
    public static final int JavaIDDigit=163;
    public static final int CLASS=12;
    public static final int TWEEN=105;
    public static final int RETURN=34;
    public static final int IF=18;
    public static final int LET=23;
    public static final int SEMI_INSERT_END=52;
    public static final int SUCHTHAT=106;
    public static final int EOF=-1;
    public static final int TYPE_FUNCTION=134;
    public static final int FOR=16;
    public static final int LAST_TOKEN=168;
    public static final int BEFORE=56;
    public static final int STAR=94;
    public static final int MISSING_NAME=116;
    public static final int ATTR_INTERPOLATE=140;
    public static final int ATTRIBUTE=8;
    public static final int BIND=9;
    public static final int SUB=93;
    public static final int MODIFIER=109;
    public static final int NOT=25;
    public static final int TRY=41;
    public static final int TYPE_ARG=137;
    public static final int Digits=154;
    public static final int RBRACE_QUOTE_STRING_LITERAL=147;

        /** The log to be used for error diagnostics.
         */
    //    private Log log;
        
        static final byte NO_INSERT_SEMI = 0; // default
        static final byte INSERT_SEMI = 1; 
        static final byte IGNORE_FOR_SEMI = 2; 
        static final byte[] semiKind = new byte[LAST_TOKEN];
        { 
          for (int i = SEMI_INSERT_START; i < SEMI_INSERT_END; ++i) {
              semiKind[i] = INSERT_SEMI;
          }
          semiKind[RBRACE] = INSERT_SEMI;
          semiKind[STRING_LITERAL] = INSERT_SEMI;
          semiKind[QUOTE_LBRACE_STRING_LITERAL] = INSERT_SEMI;
          semiKind[DECIMAL_LITERAL] = INSERT_SEMI;
          semiKind[OCTAL_LITERAL] = INSERT_SEMI;
          semiKind[HEX_LITERAL] = INSERT_SEMI;
          semiKind[TIME_LITERAL] = INSERT_SEMI;
          semiKind[FLOATING_POINT_LITERAL] = INSERT_SEMI;
          semiKind[IDENTIFIER] = INSERT_SEMI;
          
          semiKind[WS] = IGNORE_FOR_SEMI;
          semiKind[COMMENT] = IGNORE_FOR_SEMI;
          semiKind[LINE_COMMENT] = IGNORE_FOR_SEMI;
        }
          
        public v3Lexer(Context context, CharStream input) {
        	this(input);
            this.log = Log.instance(context);
        }

        protected int getSyntheticSemiType() {
            return SEMI;
        }

        protected boolean verifyCurrentType(int ttype) {
            return ttype != EOF && semiKind[ttype] != IGNORE_FOR_SEMI;
        }

        protected boolean verifyPreviousType(int ttype, int previousTokenType) {
            return previousTokenType == RBRACE && (ttype == EOF || semiKind[ttype] == INSERT_SEMI);
        }


    // quote context --
        static final int CUR_QUOTE_CTX	= 0;	// 0 = use current quote context
        static final int SNG_QUOTE_CTX	= 1;	// 1 = single quote quote context
        static final int DBL_QUOTE_CTX	= 2;	// 2 = double quote quote context
                

    // delegates
    // delegators

    public v3Lexer() {;} 
    public v3Lexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public v3Lexer(CharStream input, RecognizerSharedState state) {
        super(input,state);

    }
    public String getGrammarFileName() { return "E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g"; }

    // $ANTLR start ABSTRACT
    public final void mABSTRACT() throws RecognitionException {
        try {
            int _type = ABSTRACT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:79:10: ( 'abstract' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:79:12: 'abstract'
            {
            match("abstract"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end ABSTRACT

    // $ANTLR start ASSERT
    public final void mASSERT() throws RecognitionException {
        try {
            int _type = ASSERT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:80:8: ( 'assert' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:80:10: 'assert'
            {
            match("assert"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end ASSERT

    // $ANTLR start AT
    public final void mAT() throws RecognitionException {
        try {
            int _type = AT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:81:4: ( 'at' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:81:6: 'at'
            {
            match("at"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end AT

    // $ANTLR start ATTRIBUTE
    public final void mATTRIBUTE() throws RecognitionException {
        try {
            int _type = ATTRIBUTE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:82:11: ( 'attribute' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:82:13: 'attribute'
            {
            match("attribute"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end ATTRIBUTE

    // $ANTLR start BIND
    public final void mBIND() throws RecognitionException {
        try {
            int _type = BIND;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:83:6: ( 'bind' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:83:8: 'bind'
            {
            match("bind"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end BIND

    // $ANTLR start BOUND
    public final void mBOUND() throws RecognitionException {
        try {
            int _type = BOUND;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:84:7: ( 'bound' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:84:9: 'bound'
            {
            match("bound"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end BOUND

    // $ANTLR start BREAK
    public final void mBREAK() throws RecognitionException {
        try {
            int _type = BREAK;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:85:7: ( 'break' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:85:9: 'break'
            {
            match("break"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end BREAK

    // $ANTLR start CLASS
    public final void mCLASS() throws RecognitionException {
        try {
            int _type = CLASS;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:86:7: ( 'class' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:86:9: 'class'
            {
            match("class"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end CLASS

    // $ANTLR start CONTINUE
    public final void mCONTINUE() throws RecognitionException {
        try {
            int _type = CONTINUE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:87:10: ( 'continue' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:87:12: 'continue'
            {
            match("continue"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end CONTINUE

    // $ANTLR start DELETE
    public final void mDELETE() throws RecognitionException {
        try {
            int _type = DELETE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:88:8: ( 'delete' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:88:10: 'delete'
            {
            match("delete"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end DELETE

    // $ANTLR start FALSE
    public final void mFALSE() throws RecognitionException {
        try {
            int _type = FALSE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:89:7: ( 'false' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:89:9: 'false'
            {
            match("false"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end FALSE

    // $ANTLR start FOR
    public final void mFOR() throws RecognitionException {
        try {
            int _type = FOR;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:90:5: ( 'for' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:90:7: 'for'
            {
            match("for"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end FOR

    // $ANTLR start FUNCTION
    public final void mFUNCTION() throws RecognitionException {
        try {
            int _type = FUNCTION;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:91:10: ( 'function' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:91:12: 'function'
            {
            match("function"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end FUNCTION

    // $ANTLR start IF
    public final void mIF() throws RecognitionException {
        try {
            int _type = IF;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:92:4: ( 'if' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:92:6: 'if'
            {
            match("if"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end IF

    // $ANTLR start IMPORT
    public final void mIMPORT() throws RecognitionException {
        try {
            int _type = IMPORT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:93:8: ( 'import' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:93:10: 'import'
            {
            match("import"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end IMPORT

    // $ANTLR start INDEXOF
    public final void mINDEXOF() throws RecognitionException {
        try {
            int _type = INDEXOF;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:94:9: ( 'indexof' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:94:11: 'indexof'
            {
            match("indexof"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end INDEXOF

    // $ANTLR start INIT
    public final void mINIT() throws RecognitionException {
        try {
            int _type = INIT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:95:6: ( 'init' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:95:8: 'init'
            {
            match("init"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end INIT

    // $ANTLR start INSERT
    public final void mINSERT() throws RecognitionException {
        try {
            int _type = INSERT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:96:8: ( 'insert' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:96:10: 'insert'
            {
            match("insert"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end INSERT

    // $ANTLR start LET
    public final void mLET() throws RecognitionException {
        try {
            int _type = LET;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:97:5: ( 'let' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:97:7: 'let'
            {
            match("let"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end LET

    // $ANTLR start NEW
    public final void mNEW() throws RecognitionException {
        try {
            int _type = NEW;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:98:5: ( 'new' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:98:7: 'new'
            {
            match("new"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end NEW

    // $ANTLR start NOT
    public final void mNOT() throws RecognitionException {
        try {
            int _type = NOT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:99:5: ( 'not' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:99:7: 'not'
            {
            match("not"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end NOT

    // $ANTLR start NULL
    public final void mNULL() throws RecognitionException {
        try {
            int _type = NULL;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:100:6: ( 'null' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:100:8: 'null'
            {
            match("null"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end NULL

    // $ANTLR start OVERRIDE
    public final void mOVERRIDE() throws RecognitionException {
        try {
            int _type = OVERRIDE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:101:10: ( 'override' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:101:12: 'override'
            {
            match("override"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end OVERRIDE

    // $ANTLR start PACKAGE
    public final void mPACKAGE() throws RecognitionException {
        try {
            int _type = PACKAGE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:102:9: ( 'package' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:102:11: 'package'
            {
            match("package"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end PACKAGE

    // $ANTLR start POSTINIT
    public final void mPOSTINIT() throws RecognitionException {
        try {
            int _type = POSTINIT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:103:10: ( 'postinit' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:103:12: 'postinit'
            {
            match("postinit"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end POSTINIT

    // $ANTLR start PRIVATE
    public final void mPRIVATE() throws RecognitionException {
        try {
            int _type = PRIVATE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:104:9: ( 'private' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:104:11: 'private'
            {
            match("private"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end PRIVATE

    // $ANTLR start PROTECTED
    public final void mPROTECTED() throws RecognitionException {
        try {
            int _type = PROTECTED;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:105:11: ( 'protected' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:105:13: 'protected'
            {
            match("protected"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end PROTECTED

    // $ANTLR start PUBLIC
    public final void mPUBLIC() throws RecognitionException {
        try {
            int _type = PUBLIC;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:106:8: ( 'public' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:106:10: 'public'
            {
            match("public"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end PUBLIC

    // $ANTLR start READONLY
    public final void mREADONLY() throws RecognitionException {
        try {
            int _type = READONLY;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:107:10: ( 'readonly' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:107:12: 'readonly'
            {
            match("readonly"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end READONLY

    // $ANTLR start RETURN
    public final void mRETURN() throws RecognitionException {
        try {
            int _type = RETURN;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:108:8: ( 'return' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:108:10: 'return'
            {
            match("return"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end RETURN

    // $ANTLR start REVERSE
    public final void mREVERSE() throws RecognitionException {
        try {
            int _type = REVERSE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:109:9: ( 'reverse' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:109:11: 'reverse'
            {
            match("reverse"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end REVERSE

    // $ANTLR start SUPER
    public final void mSUPER() throws RecognitionException {
        try {
            int _type = SUPER;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:110:7: ( 'super' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:110:9: 'super'
            {
            match("super"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end SUPER

    // $ANTLR start SIZEOF
    public final void mSIZEOF() throws RecognitionException {
        try {
            int _type = SIZEOF;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:111:8: ( 'sizeof' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:111:10: 'sizeof'
            {
            match("sizeof"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end SIZEOF

    // $ANTLR start STATIC
    public final void mSTATIC() throws RecognitionException {
        try {
            int _type = STATIC;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:112:8: ( 'static' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:112:10: 'static'
            {
            match("static"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end STATIC

    // $ANTLR start THIS
    public final void mTHIS() throws RecognitionException {
        try {
            int _type = THIS;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:113:6: ( 'this' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:113:8: 'this'
            {
            match("this"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end THIS

    // $ANTLR start THROW
    public final void mTHROW() throws RecognitionException {
        try {
            int _type = THROW;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:114:7: ( 'throw' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:114:9: 'throw'
            {
            match("throw"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end THROW

    // $ANTLR start TRY
    public final void mTRY() throws RecognitionException {
        try {
            int _type = TRY;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:115:5: ( 'try' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:115:7: 'try'
            {
            match("try"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end TRY

    // $ANTLR start TRUE
    public final void mTRUE() throws RecognitionException {
        try {
            int _type = TRUE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:116:6: ( 'true' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:116:8: 'true'
            {
            match("true"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end TRUE

    // $ANTLR start TYPEOF
    public final void mTYPEOF() throws RecognitionException {
        try {
            int _type = TYPEOF;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:117:8: ( 'typeof' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:117:10: 'typeof'
            {
            match("typeof"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end TYPEOF

    // $ANTLR start VAR
    public final void mVAR() throws RecognitionException {
        try {
            int _type = VAR;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:118:5: ( 'var' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:118:7: 'var'
            {
            match("var"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end VAR

    // $ANTLR start WHILE
    public final void mWHILE() throws RecognitionException {
        try {
            int _type = WHILE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:119:7: ( 'while' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:119:9: 'while'
            {
            match("while"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end WHILE

    // $ANTLR start POUND
    public final void mPOUND() throws RecognitionException {
        try {
            int _type = POUND;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:120:7: ( '#' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:120:9: '#'
            {
            match('#'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end POUND

    // $ANTLR start LPAREN
    public final void mLPAREN() throws RecognitionException {
        try {
            int _type = LPAREN;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:121:8: ( '(' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:121:10: '('
            {
            match('('); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end LPAREN

    // $ANTLR start LBRACKET
    public final void mLBRACKET() throws RecognitionException {
        try {
            int _type = LBRACKET;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:122:10: ( '[' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:122:12: '['
            {
            match('['); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end LBRACKET

    // $ANTLR start PLUSPLUS
    public final void mPLUSPLUS() throws RecognitionException {
        try {
            int _type = PLUSPLUS;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:123:10: ( '++' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:123:12: '++'
            {
            match("++"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end PLUSPLUS

    // $ANTLR start SUBSUB
    public final void mSUBSUB() throws RecognitionException {
        try {
            int _type = SUBSUB;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:124:8: ( '--' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:124:10: '--'
            {
            match("--"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end SUBSUB

    // $ANTLR start PIPE
    public final void mPIPE() throws RecognitionException {
        try {
            int _type = PIPE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:125:6: ( '|' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:125:8: '|'
            {
            match('|'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end PIPE

    // $ANTLR start AFTER
    public final void mAFTER() throws RecognitionException {
        try {
            int _type = AFTER;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:126:7: ( 'after' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:126:9: 'after'
            {
            match("after"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end AFTER

    // $ANTLR start AND
    public final void mAND() throws RecognitionException {
        try {
            int _type = AND;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:127:5: ( 'and' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:127:7: 'and'
            {
            match("and"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end AND

    // $ANTLR start AS
    public final void mAS() throws RecognitionException {
        try {
            int _type = AS;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:128:4: ( 'as' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:128:6: 'as'
            {
            match("as"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end AS

    // $ANTLR start BEFORE
    public final void mBEFORE() throws RecognitionException {
        try {
            int _type = BEFORE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:129:8: ( 'before' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:129:10: 'before'
            {
            match("before"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end BEFORE

    // $ANTLR start CATCH
    public final void mCATCH() throws RecognitionException {
        try {
            int _type = CATCH;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:130:7: ( 'catch' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:130:9: 'catch'
            {
            match("catch"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end CATCH

    // $ANTLR start ELSE
    public final void mELSE() throws RecognitionException {
        try {
            int _type = ELSE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:131:6: ( 'else' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:131:8: 'else'
            {
            match("else"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end ELSE

    // $ANTLR start EXCLUSIVE
    public final void mEXCLUSIVE() throws RecognitionException {
        try {
            int _type = EXCLUSIVE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:132:11: ( 'exclusive' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:132:13: 'exclusive'
            {
            match("exclusive"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end EXCLUSIVE

    // $ANTLR start EXTENDS
    public final void mEXTENDS() throws RecognitionException {
        try {
            int _type = EXTENDS;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:133:9: ( 'extends' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:133:11: 'extends'
            {
            match("extends"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end EXTENDS

    // $ANTLR start FINALLY
    public final void mFINALLY() throws RecognitionException {
        try {
            int _type = FINALLY;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:134:9: ( 'finally' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:134:11: 'finally'
            {
            match("finally"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end FINALLY

    // $ANTLR start FIRST
    public final void mFIRST() throws RecognitionException {
        try {
            int _type = FIRST;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:135:7: ( 'first' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:135:9: 'first'
            {
            match("first"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end FIRST

    // $ANTLR start FROM
    public final void mFROM() throws RecognitionException {
        try {
            int _type = FROM;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:136:6: ( 'from' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:136:8: 'from'
            {
            match("from"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end FROM

    // $ANTLR start IN
    public final void mIN() throws RecognitionException {
        try {
            int _type = IN;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:137:4: ( 'in' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:137:6: 'in'
            {
            match("in"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end IN

    // $ANTLR start INSTANCEOF
    public final void mINSTANCEOF() throws RecognitionException {
        try {
            int _type = INSTANCEOF;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:138:12: ( 'instanceof' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:138:14: 'instanceof'
            {
            match("instanceof"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end INSTANCEOF

    // $ANTLR start INTO
    public final void mINTO() throws RecognitionException {
        try {
            int _type = INTO;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:139:6: ( 'into' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:139:8: 'into'
            {
            match("into"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end INTO

    // $ANTLR start INVERSE
    public final void mINVERSE() throws RecognitionException {
        try {
            int _type = INVERSE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:140:9: ( 'inverse' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:140:11: 'inverse'
            {
            match("inverse"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end INVERSE

    // $ANTLR start LAST
    public final void mLAST() throws RecognitionException {
        try {
            int _type = LAST;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:141:6: ( 'last' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:141:8: 'last'
            {
            match("last"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end LAST

    // $ANTLR start LAZY
    public final void mLAZY() throws RecognitionException {
        try {
            int _type = LAZY;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:142:6: ( 'lazy' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:142:8: 'lazy'
            {
            match("lazy"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end LAZY

    // $ANTLR start MOD
    public final void mMOD() throws RecognitionException {
        try {
            int _type = MOD;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:143:5: ( 'mod' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:143:7: 'mod'
            {
            match("mod"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end MOD

    // $ANTLR start ON
    public final void mON() throws RecognitionException {
        try {
            int _type = ON;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:144:4: ( 'on' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:144:6: 'on'
            {
            match("on"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end ON

    // $ANTLR start OR
    public final void mOR() throws RecognitionException {
        try {
            int _type = OR;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:145:4: ( 'or' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:145:6: 'or'
            {
            match("or"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end OR

    // $ANTLR start REPLACE
    public final void mREPLACE() throws RecognitionException {
        try {
            int _type = REPLACE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:146:9: ( 'replace' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:146:11: 'replace'
            {
            match("replace"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end REPLACE

    // $ANTLR start STEP
    public final void mSTEP() throws RecognitionException {
        try {
            int _type = STEP;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:147:6: ( 'step' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:147:8: 'step'
            {
            match("step"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end STEP

    // $ANTLR start THEN
    public final void mTHEN() throws RecognitionException {
        try {
            int _type = THEN;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:148:6: ( 'then' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:148:8: 'then'
            {
            match("then"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end THEN

    // $ANTLR start TRIGGER
    public final void mTRIGGER() throws RecognitionException {
        try {
            int _type = TRIGGER;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:149:9: ( 'trigger' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:149:11: 'trigger'
            {
            match("trigger"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end TRIGGER

    // $ANTLR start WITH
    public final void mWITH() throws RecognitionException {
        try {
            int _type = WITH;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:150:6: ( 'with' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:150:8: 'with'
            {
            match("with"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end WITH

    // $ANTLR start WHERE
    public final void mWHERE() throws RecognitionException {
        try {
            int _type = WHERE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:151:7: ( 'where' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:151:9: 'where'
            {
            match("where"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end WHERE

    // $ANTLR start DOTDOT
    public final void mDOTDOT() throws RecognitionException {
        try {
            int _type = DOTDOT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:152:8: ( '..' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:152:10: '..'
            {
            match(".."); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end DOTDOT

    // $ANTLR start RPAREN
    public final void mRPAREN() throws RecognitionException {
        try {
            int _type = RPAREN;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:153:8: ( ')' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:153:10: ')'
            {
            match(')'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end RPAREN

    // $ANTLR start RBRACKET
    public final void mRBRACKET() throws RecognitionException {
        try {
            int _type = RBRACKET;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:154:10: ( ']' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:154:12: ']'
            {
            match(']'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end RBRACKET

    // $ANTLR start SEMI
    public final void mSEMI() throws RecognitionException {
        try {
            int _type = SEMI;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:155:6: ( ';' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:155:8: ';'
            {
            match(';'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end SEMI

    // $ANTLR start COMMA
    public final void mCOMMA() throws RecognitionException {
        try {
            int _type = COMMA;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:156:7: ( ',' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:156:9: ','
            {
            match(','); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end COMMA

    // $ANTLR start DOT
    public final void mDOT() throws RecognitionException {
        try {
            int _type = DOT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:157:5: ( '.' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:157:7: '.'
            {
            match('.'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end DOT

    // $ANTLR start EQEQ
    public final void mEQEQ() throws RecognitionException {
        try {
            int _type = EQEQ;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:158:6: ( '==' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:158:8: '=='
            {
            match("=="); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end EQEQ

    // $ANTLR start EQ
    public final void mEQ() throws RecognitionException {
        try {
            int _type = EQ;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:159:4: ( '=' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:159:6: '='
            {
            match('='); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end EQ

    // $ANTLR start GT
    public final void mGT() throws RecognitionException {
        try {
            int _type = GT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:160:4: ( '>' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:160:6: '>'
            {
            match('>'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end GT

    // $ANTLR start LT
    public final void mLT() throws RecognitionException {
        try {
            int _type = LT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:161:4: ( '<' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:161:6: '<'
            {
            match('<'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end LT

    // $ANTLR start LTGT
    public final void mLTGT() throws RecognitionException {
        try {
            int _type = LTGT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:162:6: ( '<>' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:162:8: '<>'
            {
            match("<>"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end LTGT

    // $ANTLR start LTEQ
    public final void mLTEQ() throws RecognitionException {
        try {
            int _type = LTEQ;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:163:6: ( '<=' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:163:8: '<='
            {
            match("<="); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end LTEQ

    // $ANTLR start GTEQ
    public final void mGTEQ() throws RecognitionException {
        try {
            int _type = GTEQ;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:164:6: ( '>=' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:164:8: '>='
            {
            match(">="); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end GTEQ

    // $ANTLR start PLUS
    public final void mPLUS() throws RecognitionException {
        try {
            int _type = PLUS;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:165:6: ( '+' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:165:8: '+'
            {
            match('+'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end PLUS

    // $ANTLR start SUB
    public final void mSUB() throws RecognitionException {
        try {
            int _type = SUB;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:166:5: ( '-' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:166:7: '-'
            {
            match('-'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end SUB

    // $ANTLR start STAR
    public final void mSTAR() throws RecognitionException {
        try {
            int _type = STAR;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:167:6: ( '*' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:167:8: '*'
            {
            match('*'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end STAR

    // $ANTLR start SLASH
    public final void mSLASH() throws RecognitionException {
        try {
            int _type = SLASH;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:168:7: ( '/' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:168:9: '/'
            {
            match('/'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end SLASH

    // $ANTLR start PERCENT
    public final void mPERCENT() throws RecognitionException {
        try {
            int _type = PERCENT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:169:9: ( '%' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:169:11: '%'
            {
            match('%'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end PERCENT

    // $ANTLR start PLUSEQ
    public final void mPLUSEQ() throws RecognitionException {
        try {
            int _type = PLUSEQ;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:170:8: ( '+=' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:170:10: '+='
            {
            match("+="); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end PLUSEQ

    // $ANTLR start SUBEQ
    public final void mSUBEQ() throws RecognitionException {
        try {
            int _type = SUBEQ;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:171:7: ( '-=' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:171:9: '-='
            {
            match("-="); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end SUBEQ

    // $ANTLR start STAREQ
    public final void mSTAREQ() throws RecognitionException {
        try {
            int _type = STAREQ;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:172:8: ( '*=' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:172:10: '*='
            {
            match("*="); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end STAREQ

    // $ANTLR start SLASHEQ
    public final void mSLASHEQ() throws RecognitionException {
        try {
            int _type = SLASHEQ;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:173:9: ( '/=' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:173:11: '/='
            {
            match("/="); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end SLASHEQ

    // $ANTLR start PERCENTEQ
    public final void mPERCENTEQ() throws RecognitionException {
        try {
            int _type = PERCENTEQ;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:174:11: ( '%=' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:174:13: '%='
            {
            match("%="); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end PERCENTEQ

    // $ANTLR start NOTEQ
    public final void mNOTEQ() throws RecognitionException {
        try {
            int _type = NOTEQ;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:175:7: ( '!=' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:175:9: '!='
            {
            match("!="); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end NOTEQ

    // $ANTLR start COLON
    public final void mCOLON() throws RecognitionException {
        try {
            int _type = COLON;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:176:7: ( ':' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:176:9: ':'
            {
            match(':'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end COLON

    // $ANTLR start QUES
    public final void mQUES() throws RecognitionException {
        try {
            int _type = QUES;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:177:6: ( '?' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:177:8: '?'
            {
            match('?'); if (state.failed) return ;


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end QUES

    // $ANTLR start TWEEN
    public final void mTWEEN() throws RecognitionException {
        try {
            int _type = TWEEN;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:178:7: ( 'tween' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:178:9: 'tween'
            {
            match("tween"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end TWEEN

    // $ANTLR start SUCHTHAT
    public final void mSUCHTHAT() throws RecognitionException {
        try {
            int _type = SUCHTHAT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:179:10: ( '=>' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:179:12: '=>'
            {
            match("=>"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end SUCHTHAT

    // $ANTLR start STRING_LITERAL
    public final void mSTRING_LITERAL() throws RecognitionException {
        try {
            int _type = STRING_LITERAL;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:293:19: ( '\"' DoubleQuoteBody '\"' | '\\'' SingleQuoteBody '\\'' )
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
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:293:21: '\"' DoubleQuoteBody '\"'
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
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:294:7: '\\'' SingleQuoteBody '\\''
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
        }
        finally {
        }
    }
    // $ANTLR end STRING_LITERAL

    // $ANTLR start QUOTE_LBRACE_STRING_LITERAL
    public final void mQUOTE_LBRACE_STRING_LITERAL() throws RecognitionException {
        try {
            int _type = QUOTE_LBRACE_STRING_LITERAL;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:297:30: ( '\"' DoubleQuoteBody '{' NextIsPercent[DBL_QUOTE_CTX] | '\\'' SingleQuoteBody '{' NextIsPercent[SNG_QUOTE_CTX] )
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
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:297:32: '\"' DoubleQuoteBody '{' NextIsPercent[DBL_QUOTE_CTX]
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
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:299:7: '\\'' SingleQuoteBody '{' NextIsPercent[SNG_QUOTE_CTX]
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
        }
        finally {
        }
    }
    // $ANTLR end QUOTE_LBRACE_STRING_LITERAL

    // $ANTLR start LBRACE
    public final void mLBRACE() throws RecognitionException {
        try {
            int _type = LBRACE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:302:11: ( '{' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:302:13: '{'
            {
            match('{'); if (state.failed) return ;
            if ( state.backtracking==0 ) {
               enterBrace(0, false); 
            }


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end LBRACE

    // $ANTLR start RBRACE_QUOTE_STRING_LITERAL
    public final void mRBRACE_QUOTE_STRING_LITERAL() throws RecognitionException {
        try {
            int _type = RBRACE_QUOTE_STRING_LITERAL;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:304:30: ({...}? => '}' DoubleQuoteBody '\"' | {...}? => '}' SingleQuoteBody '\\'' )
            int alt3=2;
            alt3 = dfa3.predict(input);
            switch (alt3) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:304:35: {...}? => '}' DoubleQuoteBody '\"'
                    {
                    if ( !( rightBraceLikeQuote(DBL_QUOTE_CTX) ) ) {
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
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:308:10: {...}? => '}' SingleQuoteBody '\\''
                    {
                    if ( !( rightBraceLikeQuote(SNG_QUOTE_CTX) ) ) {
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
        }
        finally {
        }
    }
    // $ANTLR end RBRACE_QUOTE_STRING_LITERAL

    // $ANTLR start RBRACE_LBRACE_STRING_LITERAL
    public final void mRBRACE_LBRACE_STRING_LITERAL() throws RecognitionException {
        try {
            int _type = RBRACE_LBRACE_STRING_LITERAL;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:313:31: ({...}? => '}' DoubleQuoteBody '{' NextIsPercent[CUR_QUOTE_CTX] | {...}? => '}' SingleQuoteBody '{' NextIsPercent[CUR_QUOTE_CTX] )
            int alt4=2;
            alt4 = dfa4.predict(input);
            switch (alt4) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:313:36: {...}? => '}' DoubleQuoteBody '{' NextIsPercent[CUR_QUOTE_CTX]
                    {
                    if ( !( rightBraceLikeQuote(DBL_QUOTE_CTX) ) ) {
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
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:317:10: {...}? => '}' SingleQuoteBody '{' NextIsPercent[CUR_QUOTE_CTX]
                    {
                    if ( !( rightBraceLikeQuote(SNG_QUOTE_CTX) ) ) {
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
        }
        finally {
        }
    }
    // $ANTLR end RBRACE_LBRACE_STRING_LITERAL

    // $ANTLR start RBRACE
    public final void mRBRACE() throws RecognitionException {
        try {
            int _type = RBRACE;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:322:11: ({...}? => '}' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:322:16: {...}? => '}'
            {
            if ( !( !rightBraceLikeQuote(CUR_QUOTE_CTX) ) ) {
                if (state.backtracking>0) {state.failed=true; return ;}
                throw new FailedPredicateException(input, "RBRACE", " !rightBraceLikeQuote(CUR_QUOTE_CTX) ");
            }
            match('}'); if (state.failed) return ;
            if ( state.backtracking==0 ) {
               leaveBrace(); 
            }


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end RBRACE

    // $ANTLR start DoubleQuoteBody
    public final void mDoubleQuoteBody() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:326:18: ( (~ ( '{' | '\"' | '\\\\' ) | '\\\\' . )* )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:326:21: (~ ( '{' | '\"' | '\\\\' ) | '\\\\' . )*
            {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:326:21: (~ ( '{' | '\"' | '\\\\' ) | '\\\\' . )*
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
            	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:326:22: ~ ( '{' | '\"' | '\\\\' )
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
            	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:326:39: '\\\\' .
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
    // $ANTLR end DoubleQuoteBody

    // $ANTLR start SingleQuoteBody
    public final void mSingleQuoteBody() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:329:18: ( (~ ( '{' | '\\'' | '\\\\' ) | '\\\\' . )* )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:329:21: (~ ( '{' | '\\'' | '\\\\' ) | '\\\\' . )*
            {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:329:21: (~ ( '{' | '\\'' | '\\\\' ) | '\\\\' . )*
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
            	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:329:22: ~ ( '{' | '\\'' | '\\\\' )
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
            	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:329:40: '\\\\' .
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
    // $ANTLR end SingleQuoteBody

    // $ANTLR start NextIsPercent
    public final void mNextIsPercent(int quoteContext) throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:333:6: ( ( ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )* '%' )=> | )
            int alt7=2;
            int LA7_0 = input.LA(1);

            if ( (synpred1_v3()) ) {
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
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:333:8: ( ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )* '%' )=>
                    {
                    if ( state.backtracking==0 ) {
                       enterBrace(quoteContext, true); 
                    }


                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:335:10: 
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
    // $ANTLR end NextIsPercent

    // $ANTLR start FORMAT_STRING_LITERAL
    public final void mFORMAT_STRING_LITERAL() throws RecognitionException {
        try {
            int _type = FORMAT_STRING_LITERAL;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:337:24: ({...}? => '%' (~ ' ' )* )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:337:30: {...}? => '%' (~ ' ' )*
            {
            if ( !( percentIsFormat() ) ) {
                if (state.backtracking>0) {state.failed=true; return ;}
                throw new FailedPredicateException(input, "FORMAT_STRING_LITERAL", " percentIsFormat() ");
            }
            match('%'); if (state.failed) return ;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:338:11: (~ ' ' )*
            loop8:
            do {
                int alt8=2;
                int LA8_0 = input.LA(1);

                if ( ((LA8_0>='\u0000' && LA8_0<='\u001F')||(LA8_0>='!' && LA8_0<='\uFFFE')) ) {
                    alt8=1;
                }


                switch (alt8) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:338:12: ~ ' '
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
        }
        finally {
        }
    }
    // $ANTLR end FORMAT_STRING_LITERAL

    // $ANTLR start TRANSLATION_KEY
    public final void mTRANSLATION_KEY() throws RecognitionException {
        try {
            int _type = TRANSLATION_KEY;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:341:33: ( '##' ( '[' TranslationKeyBody ']' )? )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:341:35: '##' ( '[' TranslationKeyBody ']' )?
            {
            match("##"); if (state.failed) return ;

            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:342:35: ( '[' TranslationKeyBody ']' )?
            int alt9=2;
            int LA9_0 = input.LA(1);

            if ( (LA9_0=='[') ) {
                alt9=1;
            }
            switch (alt9) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:343:37: '[' TranslationKeyBody ']'
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
        }
        finally {
        }
    }
    // $ANTLR end TRANSLATION_KEY

    // $ANTLR start TranslationKeyBody
    public final void mTranslationKeyBody() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:348:33: ( (~ ( '[' | ']' | '\\\\' ) | '\\\\' . )+ )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:348:35: (~ ( '[' | ']' | '\\\\' ) | '\\\\' . )+
            {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:348:35: (~ ( '[' | ']' | '\\\\' ) | '\\\\' . )+
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
            	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:348:36: ~ ( '[' | ']' | '\\\\' )
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
            	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:348:56: '\\\\' .
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
    // $ANTLR end TranslationKeyBody

    // $ANTLR start TIME_LITERAL
    public final void mTIME_LITERAL() throws RecognitionException {
        try {
            int _type = TIME_LITERAL;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:14: ( ( DECIMAL_LITERAL | Digits '.' ( Digits )? ( Exponent )? | '.' Digits ( Exponent )? ) ( 'ms' | 'm' | 's' | 'h' ) )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:16: ( DECIMAL_LITERAL | Digits '.' ( Digits )? ( Exponent )? | '.' Digits ( Exponent )? ) ( 'ms' | 'm' | 's' | 'h' )
            {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:16: ( DECIMAL_LITERAL | Digits '.' ( Digits )? ( Exponent )? | '.' Digits ( Exponent )? )
            int alt14=3;
            alt14 = dfa14.predict(input);
            switch (alt14) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:17: DECIMAL_LITERAL
                    {
                    mDECIMAL_LITERAL(); if (state.failed) return ;


                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:35: Digits '.' ( Digits )? ( Exponent )?
                    {
                    mDigits(); if (state.failed) return ;
                    match('.'); if (state.failed) return ;
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:46: ( Digits )?
                    int alt11=2;
                    int LA11_0 = input.LA(1);

                    if ( ((LA11_0>='0' && LA11_0<='9')) ) {
                        alt11=1;
                    }
                    switch (alt11) {
                        case 1 :
                            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:47: Digits
                            {
                            mDigits(); if (state.failed) return ;


                            }
                            break;

                    }

                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:56: ( Exponent )?
                    int alt12=2;
                    int LA12_0 = input.LA(1);

                    if ( (LA12_0=='E'||LA12_0=='e') ) {
                        alt12=1;
                    }
                    switch (alt12) {
                        case 1 :
                            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:57: Exponent
                            {
                            mExponent(); if (state.failed) return ;


                            }
                            break;

                    }



                    }
                    break;
                case 3 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:70: '.' Digits ( Exponent )?
                    {
                    match('.'); if (state.failed) return ;
                    mDigits(); if (state.failed) return ;
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:81: ( Exponent )?
                    int alt13=2;
                    int LA13_0 = input.LA(1);

                    if ( (LA13_0=='E'||LA13_0=='e') ) {
                        alt13=1;
                    }
                    switch (alt13) {
                        case 1 :
                            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:82: Exponent
                            {
                            mExponent(); if (state.failed) return ;


                            }
                            break;

                    }



                    }
                    break;

            }

            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:94: ( 'ms' | 'm' | 's' | 'h' )
            int alt15=4;
            switch ( input.LA(1) ) {
            case 'm':
                {
                int LA15_1 = input.LA(2);

                if ( (LA15_1=='s') ) {
                    alt15=1;
                }
                else {
                    alt15=2;}
                }
                break;
            case 's':
                {
                alt15=3;
                }
                break;
            case 'h':
                {
                alt15=4;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return ;}
                NoViableAltException nvae =
                    new NoViableAltException("", 15, 0, input);

                throw nvae;
            }

            switch (alt15) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:96: 'ms'
                    {
                    match("ms"); if (state.failed) return ;



                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:103: 'm'
                    {
                    match('m'); if (state.failed) return ;


                    }
                    break;
                case 3 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:109: 's'
                    {
                    match('s'); if (state.failed) return ;


                    }
                    break;
                case 4 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:351:115: 'h'
                    {
                    match('h'); if (state.failed) return ;


                    }
                    break;

            }



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end TIME_LITERAL

    // $ANTLR start DECIMAL_LITERAL
    public final void mDECIMAL_LITERAL() throws RecognitionException {
        try {
            int _type = DECIMAL_LITERAL;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:353:17: ( ( '0' | '1' .. '9' ( '0' .. '9' )* ) )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:353:19: ( '0' | '1' .. '9' ( '0' .. '9' )* )
            {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:353:19: ( '0' | '1' .. '9' ( '0' .. '9' )* )
            int alt17=2;
            int LA17_0 = input.LA(1);

            if ( (LA17_0=='0') ) {
                alt17=1;
            }
            else if ( ((LA17_0>='1' && LA17_0<='9')) ) {
                alt17=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                NoViableAltException nvae =
                    new NoViableAltException("", 17, 0, input);

                throw nvae;
            }
            switch (alt17) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:353:20: '0'
                    {
                    match('0'); if (state.failed) return ;


                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:353:26: '1' .. '9' ( '0' .. '9' )*
                    {
                    matchRange('1','9'); if (state.failed) return ;
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:353:35: ( '0' .. '9' )*
                    loop16:
                    do {
                        int alt16=2;
                        int LA16_0 = input.LA(1);

                        if ( ((LA16_0>='0' && LA16_0<='9')) ) {
                            alt16=1;
                        }


                        switch (alt16) {
                    	case 1 :
                    	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:353:35: '0' .. '9'
                    	    {
                    	    matchRange('0','9'); if (state.failed) return ;


                    	    }
                    	    break;

                    	default :
                    	    break loop16;
                        }
                    } while (true);



                    }
                    break;

            }

            if ( state.backtracking==0 ) {
               checkIntLiteralRange(getText(), getCharIndex(), 10); 
            }


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end DECIMAL_LITERAL

    // $ANTLR start OCTAL_LITERAL
    public final void mOCTAL_LITERAL() throws RecognitionException {
        try {
            int _type = OCTAL_LITERAL;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:355:15: ( '0' ( '0' .. '7' )+ )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:355:17: '0' ( '0' .. '7' )+
            {
            match('0'); if (state.failed) return ;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:355:21: ( '0' .. '7' )+
            int cnt18=0;
            loop18:
            do {
                int alt18=2;
                int LA18_0 = input.LA(1);

                if ( ((LA18_0>='0' && LA18_0<='7')) ) {
                    alt18=1;
                }


                switch (alt18) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:355:22: '0' .. '7'
            	    {
            	    matchRange('0','7'); if (state.failed) return ;


            	    }
            	    break;

            	default :
            	    if ( cnt18 >= 1 ) break loop18;
            	    if (state.backtracking>0) {state.failed=true; return ;}
                        EarlyExitException eee =
                            new EarlyExitException(18, input);
                        throw eee;
                }
                cnt18++;
            } while (true);

            if ( state.backtracking==0 ) {
               checkIntLiteralRange(getText(), getCharIndex(), 8); 
            }


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end OCTAL_LITERAL

    // $ANTLR start HEX_LITERAL
    public final void mHEX_LITERAL() throws RecognitionException {
        try {
            int _type = HEX_LITERAL;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:357:13: ( '0' ( 'x' | 'X' ) ( HexDigit )+ )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:357:15: '0' ( 'x' | 'X' ) ( HexDigit )+
            {
            match('0'); if (state.failed) return ;
            if ( input.LA(1)=='X'||input.LA(1)=='x' ) {
                input.consume();
            state.failed=false;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:357:29: ( HexDigit )+
            int cnt19=0;
            loop19:
            do {
                int alt19=2;
                int LA19_0 = input.LA(1);

                if ( ((LA19_0>='0' && LA19_0<='9')||(LA19_0>='A' && LA19_0<='F')||(LA19_0>='a' && LA19_0<='f')) ) {
                    alt19=1;
                }


                switch (alt19) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:357:29: HexDigit
            	    {
            	    mHexDigit(); if (state.failed) return ;


            	    }
            	    break;

            	default :
            	    if ( cnt19 >= 1 ) break loop19;
            	    if (state.backtracking>0) {state.failed=true; return ;}
                        EarlyExitException eee =
                            new EarlyExitException(19, input);
                        throw eee;
                }
                cnt19++;
            } while (true);

            if ( state.backtracking==0 ) {
               setText(getText().substring(2, getText().length()));
                                                                                checkIntLiteralRange(getText(), getCharIndex(), 16); 
            }


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end HEX_LITERAL

    // $ANTLR start HexDigit
    public final void mHexDigit() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:361:10: ( ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ) )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:361:12: ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' )
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

        }
        finally {
        }
    }
    // $ANTLR end HexDigit

    // $ANTLR start FLOATING_POINT_LITERAL
    public final void mFLOATING_POINT_LITERAL() throws RecognitionException {
        try {
            int _type = FLOATING_POINT_LITERAL;
            Token d=null;
            Token RangeDots1=null;
            Token RangeDots2=null;

            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:364:5: (d= DECIMAL_LITERAL RangeDots | d= OCTAL_LITERAL RangeDots | Digits '.' ( Digits )? ( Exponent )? | '.' Digits ( Exponent )? | Digits Exponent )
            int alt23=5;
            alt23 = dfa23.predict(input);
            switch (alt23) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:364:11: d= DECIMAL_LITERAL RangeDots
                    {
                    int dStart1696 = getCharIndex();
                    mDECIMAL_LITERAL(); if (state.failed) return ;
                    d = new CommonToken(input, Token.INVALID_TOKEN_TYPE, Token.DEFAULT_CHANNEL, dStart1696, getCharIndex()-1);
                    int RangeDots1Start1698 = getCharIndex();
                    mRangeDots(); if (state.failed) return ;
                    RangeDots1 = new CommonToken(input, Token.INVALID_TOKEN_TYPE, Token.DEFAULT_CHANNEL, RangeDots1Start1698, getCharIndex()-1);
                    if ( state.backtracking==0 ) {

                          	  		d.setType(DECIMAL_LITERAL);
                          	  		emit(d);
                                		RangeDots1.setType(DOTDOT);
                          	  		emit(RangeDots1);
                          	  	
                    }


                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:371:11: d= OCTAL_LITERAL RangeDots
                    {
                    int dStart1722 = getCharIndex();
                    mOCTAL_LITERAL(); if (state.failed) return ;
                    d = new CommonToken(input, Token.INVALID_TOKEN_TYPE, Token.DEFAULT_CHANNEL, dStart1722, getCharIndex()-1);
                    int RangeDots2Start1724 = getCharIndex();
                    mRangeDots(); if (state.failed) return ;
                    RangeDots2 = new CommonToken(input, Token.INVALID_TOKEN_TYPE, Token.DEFAULT_CHANNEL, RangeDots2Start1724, getCharIndex()-1);
                    if ( state.backtracking==0 ) {

                          	  		d.setType(OCTAL_LITERAL);
                          	  		emit(d);
                                		RangeDots2.setType(DOTDOT);
                          	  		emit(RangeDots2);
                          	  	
                    }


                    }
                    break;
                case 3 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:378:9: Digits '.' ( Digits )? ( Exponent )?
                    {
                    mDigits(); if (state.failed) return ;
                    match('.'); if (state.failed) return ;
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:378:20: ( Digits )?
                    int alt20=2;
                    int LA20_0 = input.LA(1);

                    if ( ((LA20_0>='0' && LA20_0<='9')) ) {
                        alt20=1;
                    }
                    switch (alt20) {
                        case 1 :
                            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:378:21: Digits
                            {
                            mDigits(); if (state.failed) return ;


                            }
                            break;

                    }

                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:378:30: ( Exponent )?
                    int alt21=2;
                    int LA21_0 = input.LA(1);

                    if ( (LA21_0=='E'||LA21_0=='e') ) {
                        alt21=1;
                    }
                    switch (alt21) {
                        case 1 :
                            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:378:31: Exponent
                            {
                            mExponent(); if (state.failed) return ;


                            }
                            break;

                    }



                    }
                    break;
                case 4 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:379:7: '.' Digits ( Exponent )?
                    {
                    match('.'); if (state.failed) return ;
                    mDigits(); if (state.failed) return ;
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:379:18: ( Exponent )?
                    int alt22=2;
                    int LA22_0 = input.LA(1);

                    if ( (LA22_0=='E'||LA22_0=='e') ) {
                        alt22=1;
                    }
                    switch (alt22) {
                        case 1 :
                            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:379:19: Exponent
                            {
                            mExponent(); if (state.failed) return ;


                            }
                            break;

                    }



                    }
                    break;
                case 5 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:380:11: Digits Exponent
                    {
                    mDigits(); if (state.failed) return ;
                    mExponent(); if (state.failed) return ;


                    }
                    break;

            }
            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end FLOATING_POINT_LITERAL

    // $ANTLR start RangeDots
    public final void mRangeDots() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:385:2: ( DOTDOT )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:385:4: DOTDOT
            {
            mDOTDOT(); if (state.failed) return ;


            }

        }
        finally {
        }
    }
    // $ANTLR end RangeDots

    // $ANTLR start Digits
    public final void mDigits() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:388:8: ( ( '0' .. '9' )+ )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:388:10: ( '0' .. '9' )+
            {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:388:10: ( '0' .. '9' )+
            int cnt24=0;
            loop24:
            do {
                int alt24=2;
                int LA24_0 = input.LA(1);

                if ( ((LA24_0>='0' && LA24_0<='9')) ) {
                    alt24=1;
                }


                switch (alt24) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:388:11: '0' .. '9'
            	    {
            	    matchRange('0','9'); if (state.failed) return ;


            	    }
            	    break;

            	default :
            	    if ( cnt24 >= 1 ) break loop24;
            	    if (state.backtracking>0) {state.failed=true; return ;}
                        EarlyExitException eee =
                            new EarlyExitException(24, input);
                        throw eee;
                }
                cnt24++;
            } while (true);



            }

        }
        finally {
        }
    }
    // $ANTLR end Digits

    // $ANTLR start Exponent
    public final void mExponent() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:391:10: ( ( 'e' | 'E' ) ( '+' | '-' )? Digits )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:391:13: ( 'e' | 'E' ) ( '+' | '-' )? Digits
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

            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:391:23: ( '+' | '-' )?
            int alt25=2;
            int LA25_0 = input.LA(1);

            if ( (LA25_0=='+'||LA25_0=='-') ) {
                alt25=1;
            }
            switch (alt25) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:
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

            mDigits(); if (state.failed) return ;


            }

        }
        finally {
        }
    }
    // $ANTLR end Exponent

    // $ANTLR start IDENTIFIER
    public final void mIDENTIFIER() throws RecognitionException {
        try {
            int _type = IDENTIFIER;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:395:2: ( Letter ( Letter | JavaIDDigit )* | '<<' (~ '>' | '>' ~ '>' )* ( '>' )* '>>' )
            int alt29=2;
            int LA29_0 = input.LA(1);

            if ( (LA29_0=='$'||(LA29_0>='A' && LA29_0<='Z')||LA29_0=='_'||(LA29_0>='a' && LA29_0<='z')||(LA29_0>='\u00C0' && LA29_0<='\u00D6')||(LA29_0>='\u00D8' && LA29_0<='\u00F6')||(LA29_0>='\u00F8' && LA29_0<='\u1FFF')||(LA29_0>='\u3040' && LA29_0<='\u318F')||(LA29_0>='\u3300' && LA29_0<='\u337F')||(LA29_0>='\u3400' && LA29_0<='\u3D2D')||(LA29_0>='\u4E00' && LA29_0<='\u9FFF')||(LA29_0>='\uF900' && LA29_0<='\uFAFF')) ) {
                alt29=1;
            }
            else if ( (LA29_0=='<') ) {
                alt29=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                NoViableAltException nvae =
                    new NoViableAltException("", 29, 0, input);

                throw nvae;
            }
            switch (alt29) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:395:4: Letter ( Letter | JavaIDDigit )*
                    {
                    mLetter(); if (state.failed) return ;
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:395:11: ( Letter | JavaIDDigit )*
                    loop26:
                    do {
                        int alt26=2;
                        int LA26_0 = input.LA(1);

                        if ( (LA26_0=='$'||(LA26_0>='0' && LA26_0<='9')||(LA26_0>='A' && LA26_0<='Z')||LA26_0=='_'||(LA26_0>='a' && LA26_0<='z')||(LA26_0>='\u00C0' && LA26_0<='\u00D6')||(LA26_0>='\u00D8' && LA26_0<='\u00F6')||(LA26_0>='\u00F8' && LA26_0<='\u1FFF')||(LA26_0>='\u3040' && LA26_0<='\u318F')||(LA26_0>='\u3300' && LA26_0<='\u337F')||(LA26_0>='\u3400' && LA26_0<='\u3D2D')||(LA26_0>='\u4E00' && LA26_0<='\u9FFF')||(LA26_0>='\uF900' && LA26_0<='\uFAFF')) ) {
                            alt26=1;
                        }


                        switch (alt26) {
                    	case 1 :
                    	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:
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
                    	    break loop26;
                        }
                    } while (true);



                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:396:4: '<<' (~ '>' | '>' ~ '>' )* ( '>' )* '>>'
                    {
                    match("<<"); if (state.failed) return ;

                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:396:9: (~ '>' | '>' ~ '>' )*
                    loop27:
                    do {
                        int alt27=3;
                        int LA27_0 = input.LA(1);

                        if ( (LA27_0=='>') ) {
                            int LA27_1 = input.LA(2);

                            if ( ((LA27_1>='\u0000' && LA27_1<='=')||(LA27_1>='?' && LA27_1<='\uFFFE')) ) {
                                alt27=2;
                            }


                        }
                        else if ( ((LA27_0>='\u0000' && LA27_0<='=')||(LA27_0>='?' && LA27_0<='\uFFFE')) ) {
                            alt27=1;
                        }


                        switch (alt27) {
                    	case 1 :
                    	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:396:10: ~ '>'
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
                    	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:396:16: '>' ~ '>'
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
                    	    break loop27;
                        }
                    } while (true);

                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:396:27: ( '>' )*
                    loop28:
                    do {
                        int alt28=2;
                        int LA28_0 = input.LA(1);

                        if ( (LA28_0=='>') ) {
                            int LA28_1 = input.LA(2);

                            if ( (LA28_1=='>') ) {
                                int LA28_2 = input.LA(3);

                                if ( (LA28_2=='>') ) {
                                    alt28=1;
                                }


                            }


                        }


                        switch (alt28) {
                    	case 1 :
                    	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:396:27: '>'
                    	    {
                    	    match('>'); if (state.failed) return ;


                    	    }
                    	    break;

                    	default :
                    	    break loop28;
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
        }
        finally {
        }
    }
    // $ANTLR end IDENTIFIER

    // $ANTLR start Letter
    public final void mLetter() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:401:5: ( '\\u0024' | '\\u0041' .. '\\u005a' | '\\u005f' | '\\u0061' .. '\\u007a' | '\\u00c0' .. '\\u00d6' | '\\u00d8' .. '\\u00f6' | '\\u00f8' .. '\\u00ff' | '\\u0100' .. '\\u1fff' | '\\u3040' .. '\\u318f' | '\\u3300' .. '\\u337f' | '\\u3400' .. '\\u3d2d' | '\\u4e00' .. '\\u9fff' | '\\uf900' .. '\\ufaff' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:
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
    // $ANTLR end Letter

    // $ANTLR start JavaIDDigit
    public final void mJavaIDDigit() throws RecognitionException {
        try {
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:418:5: ( '\\u0030' .. '\\u0039' | '\\u0660' .. '\\u0669' | '\\u06f0' .. '\\u06f9' | '\\u0966' .. '\\u096f' | '\\u09e6' .. '\\u09ef' | '\\u0a66' .. '\\u0a6f' | '\\u0ae6' .. '\\u0aef' | '\\u0b66' .. '\\u0b6f' | '\\u0be7' .. '\\u0bef' | '\\u0c66' .. '\\u0c6f' | '\\u0ce6' .. '\\u0cef' | '\\u0d66' .. '\\u0d6f' | '\\u0e50' .. '\\u0e59' | '\\u0ed0' .. '\\u0ed9' | '\\u1040' .. '\\u1049' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:
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
    // $ANTLR end JavaIDDigit

    // $ANTLR start WS
    public final void mWS() throws RecognitionException {
        try {
            int _type = WS;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:435:5: ( ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' ) )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:435:8: ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )
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
              state.channel=HIDDEN;
            }


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end WS

    // $ANTLR start COMMENT
    public final void mCOMMENT() throws RecognitionException {
        try {
            int _type = COMMENT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:439:5: ( '/*' ( options {greedy=false; } : . )* '*/' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:439:9: '/*' ( options {greedy=false; } : . )* '*/'
            {
            match("/*"); if (state.failed) return ;

            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:439:14: ( options {greedy=false; } : . )*
            loop30:
            do {
                int alt30=2;
                int LA30_0 = input.LA(1);

                if ( (LA30_0=='*') ) {
                    int LA30_1 = input.LA(2);

                    if ( (LA30_1=='/') ) {
                        alt30=2;
                    }
                    else if ( ((LA30_1>='\u0000' && LA30_1<='.')||(LA30_1>='0' && LA30_1<='\uFFFE')) ) {
                        alt30=1;
                    }


                }
                else if ( ((LA30_0>='\u0000' && LA30_0<=')')||(LA30_0>='+' && LA30_0<='\uFFFE')) ) {
                    alt30=1;
                }


                switch (alt30) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:439:42: .
            	    {
            	    matchAny(); if (state.failed) return ;


            	    }
            	    break;

            	default :
            	    break loop30;
                }
            } while (true);

            match("*/"); if (state.failed) return ;

            if ( state.backtracking==0 ) {
              state.channel=HIDDEN;
            }


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end COMMENT

    // $ANTLR start LINE_COMMENT
    public final void mLINE_COMMENT() throws RecognitionException {
        try {
            int _type = LINE_COMMENT;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:443:5: ( '//' (~ ( '\\n' | '\\r' ) )* ( '\\r' )? ( '\\n' | EOF ) )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:443:7: '//' (~ ( '\\n' | '\\r' ) )* ( '\\r' )? ( '\\n' | EOF )
            {
            match("//"); if (state.failed) return ;

            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:443:12: (~ ( '\\n' | '\\r' ) )*
            loop31:
            do {
                int alt31=2;
                int LA31_0 = input.LA(1);

                if ( ((LA31_0>='\u0000' && LA31_0<='\t')||(LA31_0>='\u000B' && LA31_0<='\f')||(LA31_0>='\u000E' && LA31_0<='\uFFFE')) ) {
                    alt31=1;
                }


                switch (alt31) {
            	case 1 :
            	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:443:12: ~ ( '\\n' | '\\r' )
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
            	    break loop31;
                }
            } while (true);

            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:443:26: ( '\\r' )?
            int alt32=2;
            int LA32_0 = input.LA(1);

            if ( (LA32_0=='\r') ) {
                alt32=1;
            }
            switch (alt32) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:443:26: '\\r'
                    {
                    match('\r'); if (state.failed) return ;


                    }
                    break;

            }

            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:443:32: ( '\\n' | EOF )
            int alt33=2;
            int LA33_0 = input.LA(1);

            if ( (LA33_0=='\n') ) {
                alt33=1;
            }
            else {
                alt33=2;}
            switch (alt33) {
                case 1 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:443:33: '\\n'
                    {
                    match('\n'); if (state.failed) return ;


                    }
                    break;
                case 2 :
                    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:443:38: EOF
                    {
                    match(EOF); if (state.failed) return ;


                    }
                    break;

            }

            if ( state.backtracking==0 ) {
              state.channel=HIDDEN;
            }


            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end LINE_COMMENT

    // $ANTLR start LAST_TOKEN
    public final void mLAST_TOKEN() throws RecognitionException {
        try {
            int _type = LAST_TOKEN;
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:447:5: ( '~~~~~~~~' {...}? '~~~~~~~~' )
            // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:447:7: '~~~~~~~~' {...}? '~~~~~~~~'
            {
            match("~~~~~~~~"); if (state.failed) return ;

            if ( !(false) ) {
                if (state.backtracking>0) {state.failed=true; return ;}
                throw new FailedPredicateException(input, "LAST_TOKEN", "false");
            }
            match("~~~~~~~~"); if (state.failed) return ;



            }

            state.type = _type;
        }
        finally {
        }
    }
    // $ANTLR end LAST_TOKEN

    public void mTokens() throws RecognitionException {
        // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:8: ( ABSTRACT | ASSERT | AT | ATTRIBUTE | BIND | BOUND | BREAK | CLASS | CONTINUE | DELETE | FALSE | FOR | FUNCTION | IF | IMPORT | INDEXOF | INIT | INSERT | LET | NEW | NOT | NULL | OVERRIDE | PACKAGE | POSTINIT | PRIVATE | PROTECTED | PUBLIC | READONLY | RETURN | REVERSE | SUPER | SIZEOF | STATIC | THIS | THROW | TRY | TRUE | TYPEOF | VAR | WHILE | POUND | LPAREN | LBRACKET | PLUSPLUS | SUBSUB | PIPE | AFTER | AND | AS | BEFORE | CATCH | ELSE | EXCLUSIVE | EXTENDS | FINALLY | FIRST | FROM | IN | INSTANCEOF | INTO | INVERSE | LAST | LAZY | MOD | ON | OR | REPLACE | STEP | THEN | TRIGGER | WITH | WHERE | DOTDOT | RPAREN | RBRACKET | SEMI | COMMA | DOT | EQEQ | EQ | GT | LT | LTGT | LTEQ | GTEQ | PLUS | SUB | STAR | SLASH | PERCENT | PLUSEQ | SUBEQ | STAREQ | SLASHEQ | PERCENTEQ | NOTEQ | COLON | QUES | TWEEN | SUCHTHAT | STRING_LITERAL | QUOTE_LBRACE_STRING_LITERAL | LBRACE | RBRACE_QUOTE_STRING_LITERAL | RBRACE_LBRACE_STRING_LITERAL | RBRACE | FORMAT_STRING_LITERAL | TRANSLATION_KEY | TIME_LITERAL | DECIMAL_LITERAL | OCTAL_LITERAL | HEX_LITERAL | FLOATING_POINT_LITERAL | IDENTIFIER | WS | COMMENT | LINE_COMMENT | LAST_TOKEN )
        int alt34=119;
        alt34 = dfa34.predict(input);
        switch (alt34) {
            case 1 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:10: ABSTRACT
                {
                mABSTRACT(); if (state.failed) return ;


                }
                break;
            case 2 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:19: ASSERT
                {
                mASSERT(); if (state.failed) return ;


                }
                break;
            case 3 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:26: AT
                {
                mAT(); if (state.failed) return ;


                }
                break;
            case 4 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:29: ATTRIBUTE
                {
                mATTRIBUTE(); if (state.failed) return ;


                }
                break;
            case 5 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:39: BIND
                {
                mBIND(); if (state.failed) return ;


                }
                break;
            case 6 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:44: BOUND
                {
                mBOUND(); if (state.failed) return ;


                }
                break;
            case 7 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:50: BREAK
                {
                mBREAK(); if (state.failed) return ;


                }
                break;
            case 8 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:56: CLASS
                {
                mCLASS(); if (state.failed) return ;


                }
                break;
            case 9 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:62: CONTINUE
                {
                mCONTINUE(); if (state.failed) return ;


                }
                break;
            case 10 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:71: DELETE
                {
                mDELETE(); if (state.failed) return ;


                }
                break;
            case 11 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:78: FALSE
                {
                mFALSE(); if (state.failed) return ;


                }
                break;
            case 12 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:84: FOR
                {
                mFOR(); if (state.failed) return ;


                }
                break;
            case 13 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:88: FUNCTION
                {
                mFUNCTION(); if (state.failed) return ;


                }
                break;
            case 14 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:97: IF
                {
                mIF(); if (state.failed) return ;


                }
                break;
            case 15 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:100: IMPORT
                {
                mIMPORT(); if (state.failed) return ;


                }
                break;
            case 16 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:107: INDEXOF
                {
                mINDEXOF(); if (state.failed) return ;


                }
                break;
            case 17 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:115: INIT
                {
                mINIT(); if (state.failed) return ;


                }
                break;
            case 18 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:120: INSERT
                {
                mINSERT(); if (state.failed) return ;


                }
                break;
            case 19 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:127: LET
                {
                mLET(); if (state.failed) return ;


                }
                break;
            case 20 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:131: NEW
                {
                mNEW(); if (state.failed) return ;


                }
                break;
            case 21 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:135: NOT
                {
                mNOT(); if (state.failed) return ;


                }
                break;
            case 22 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:139: NULL
                {
                mNULL(); if (state.failed) return ;


                }
                break;
            case 23 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:144: OVERRIDE
                {
                mOVERRIDE(); if (state.failed) return ;


                }
                break;
            case 24 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:153: PACKAGE
                {
                mPACKAGE(); if (state.failed) return ;


                }
                break;
            case 25 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:161: POSTINIT
                {
                mPOSTINIT(); if (state.failed) return ;


                }
                break;
            case 26 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:170: PRIVATE
                {
                mPRIVATE(); if (state.failed) return ;


                }
                break;
            case 27 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:178: PROTECTED
                {
                mPROTECTED(); if (state.failed) return ;


                }
                break;
            case 28 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:188: PUBLIC
                {
                mPUBLIC(); if (state.failed) return ;


                }
                break;
            case 29 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:195: READONLY
                {
                mREADONLY(); if (state.failed) return ;


                }
                break;
            case 30 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:204: RETURN
                {
                mRETURN(); if (state.failed) return ;


                }
                break;
            case 31 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:211: REVERSE
                {
                mREVERSE(); if (state.failed) return ;


                }
                break;
            case 32 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:219: SUPER
                {
                mSUPER(); if (state.failed) return ;


                }
                break;
            case 33 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:225: SIZEOF
                {
                mSIZEOF(); if (state.failed) return ;


                }
                break;
            case 34 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:232: STATIC
                {
                mSTATIC(); if (state.failed) return ;


                }
                break;
            case 35 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:239: THIS
                {
                mTHIS(); if (state.failed) return ;


                }
                break;
            case 36 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:244: THROW
                {
                mTHROW(); if (state.failed) return ;


                }
                break;
            case 37 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:250: TRY
                {
                mTRY(); if (state.failed) return ;


                }
                break;
            case 38 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:254: TRUE
                {
                mTRUE(); if (state.failed) return ;


                }
                break;
            case 39 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:259: TYPEOF
                {
                mTYPEOF(); if (state.failed) return ;


                }
                break;
            case 40 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:266: VAR
                {
                mVAR(); if (state.failed) return ;


                }
                break;
            case 41 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:270: WHILE
                {
                mWHILE(); if (state.failed) return ;


                }
                break;
            case 42 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:276: POUND
                {
                mPOUND(); if (state.failed) return ;


                }
                break;
            case 43 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:282: LPAREN
                {
                mLPAREN(); if (state.failed) return ;


                }
                break;
            case 44 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:289: LBRACKET
                {
                mLBRACKET(); if (state.failed) return ;


                }
                break;
            case 45 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:298: PLUSPLUS
                {
                mPLUSPLUS(); if (state.failed) return ;


                }
                break;
            case 46 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:307: SUBSUB
                {
                mSUBSUB(); if (state.failed) return ;


                }
                break;
            case 47 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:314: PIPE
                {
                mPIPE(); if (state.failed) return ;


                }
                break;
            case 48 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:319: AFTER
                {
                mAFTER(); if (state.failed) return ;


                }
                break;
            case 49 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:325: AND
                {
                mAND(); if (state.failed) return ;


                }
                break;
            case 50 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:329: AS
                {
                mAS(); if (state.failed) return ;


                }
                break;
            case 51 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:332: BEFORE
                {
                mBEFORE(); if (state.failed) return ;


                }
                break;
            case 52 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:339: CATCH
                {
                mCATCH(); if (state.failed) return ;


                }
                break;
            case 53 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:345: ELSE
                {
                mELSE(); if (state.failed) return ;


                }
                break;
            case 54 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:350: EXCLUSIVE
                {
                mEXCLUSIVE(); if (state.failed) return ;


                }
                break;
            case 55 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:360: EXTENDS
                {
                mEXTENDS(); if (state.failed) return ;


                }
                break;
            case 56 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:368: FINALLY
                {
                mFINALLY(); if (state.failed) return ;


                }
                break;
            case 57 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:376: FIRST
                {
                mFIRST(); if (state.failed) return ;


                }
                break;
            case 58 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:382: FROM
                {
                mFROM(); if (state.failed) return ;


                }
                break;
            case 59 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:387: IN
                {
                mIN(); if (state.failed) return ;


                }
                break;
            case 60 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:390: INSTANCEOF
                {
                mINSTANCEOF(); if (state.failed) return ;


                }
                break;
            case 61 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:401: INTO
                {
                mINTO(); if (state.failed) return ;


                }
                break;
            case 62 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:406: INVERSE
                {
                mINVERSE(); if (state.failed) return ;


                }
                break;
            case 63 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:414: LAST
                {
                mLAST(); if (state.failed) return ;


                }
                break;
            case 64 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:419: LAZY
                {
                mLAZY(); if (state.failed) return ;


                }
                break;
            case 65 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:424: MOD
                {
                mMOD(); if (state.failed) return ;


                }
                break;
            case 66 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:428: ON
                {
                mON(); if (state.failed) return ;


                }
                break;
            case 67 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:431: OR
                {
                mOR(); if (state.failed) return ;


                }
                break;
            case 68 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:434: REPLACE
                {
                mREPLACE(); if (state.failed) return ;


                }
                break;
            case 69 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:442: STEP
                {
                mSTEP(); if (state.failed) return ;


                }
                break;
            case 70 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:447: THEN
                {
                mTHEN(); if (state.failed) return ;


                }
                break;
            case 71 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:452: TRIGGER
                {
                mTRIGGER(); if (state.failed) return ;


                }
                break;
            case 72 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:460: WITH
                {
                mWITH(); if (state.failed) return ;


                }
                break;
            case 73 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:465: WHERE
                {
                mWHERE(); if (state.failed) return ;


                }
                break;
            case 74 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:471: DOTDOT
                {
                mDOTDOT(); if (state.failed) return ;


                }
                break;
            case 75 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:478: RPAREN
                {
                mRPAREN(); if (state.failed) return ;


                }
                break;
            case 76 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:485: RBRACKET
                {
                mRBRACKET(); if (state.failed) return ;


                }
                break;
            case 77 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:494: SEMI
                {
                mSEMI(); if (state.failed) return ;


                }
                break;
            case 78 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:499: COMMA
                {
                mCOMMA(); if (state.failed) return ;


                }
                break;
            case 79 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:505: DOT
                {
                mDOT(); if (state.failed) return ;


                }
                break;
            case 80 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:509: EQEQ
                {
                mEQEQ(); if (state.failed) return ;


                }
                break;
            case 81 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:514: EQ
                {
                mEQ(); if (state.failed) return ;


                }
                break;
            case 82 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:517: GT
                {
                mGT(); if (state.failed) return ;


                }
                break;
            case 83 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:520: LT
                {
                mLT(); if (state.failed) return ;


                }
                break;
            case 84 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:523: LTGT
                {
                mLTGT(); if (state.failed) return ;


                }
                break;
            case 85 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:528: LTEQ
                {
                mLTEQ(); if (state.failed) return ;


                }
                break;
            case 86 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:533: GTEQ
                {
                mGTEQ(); if (state.failed) return ;


                }
                break;
            case 87 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:538: PLUS
                {
                mPLUS(); if (state.failed) return ;


                }
                break;
            case 88 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:543: SUB
                {
                mSUB(); if (state.failed) return ;


                }
                break;
            case 89 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:547: STAR
                {
                mSTAR(); if (state.failed) return ;


                }
                break;
            case 90 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:552: SLASH
                {
                mSLASH(); if (state.failed) return ;


                }
                break;
            case 91 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:558: PERCENT
                {
                mPERCENT(); if (state.failed) return ;


                }
                break;
            case 92 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:566: PLUSEQ
                {
                mPLUSEQ(); if (state.failed) return ;


                }
                break;
            case 93 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:573: SUBEQ
                {
                mSUBEQ(); if (state.failed) return ;


                }
                break;
            case 94 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:579: STAREQ
                {
                mSTAREQ(); if (state.failed) return ;


                }
                break;
            case 95 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:586: SLASHEQ
                {
                mSLASHEQ(); if (state.failed) return ;


                }
                break;
            case 96 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:594: PERCENTEQ
                {
                mPERCENTEQ(); if (state.failed) return ;


                }
                break;
            case 97 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:604: NOTEQ
                {
                mNOTEQ(); if (state.failed) return ;


                }
                break;
            case 98 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:610: COLON
                {
                mCOLON(); if (state.failed) return ;


                }
                break;
            case 99 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:616: QUES
                {
                mQUES(); if (state.failed) return ;


                }
                break;
            case 100 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:621: TWEEN
                {
                mTWEEN(); if (state.failed) return ;


                }
                break;
            case 101 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:627: SUCHTHAT
                {
                mSUCHTHAT(); if (state.failed) return ;


                }
                break;
            case 102 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:636: STRING_LITERAL
                {
                mSTRING_LITERAL(); if (state.failed) return ;


                }
                break;
            case 103 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:651: QUOTE_LBRACE_STRING_LITERAL
                {
                mQUOTE_LBRACE_STRING_LITERAL(); if (state.failed) return ;


                }
                break;
            case 104 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:679: LBRACE
                {
                mLBRACE(); if (state.failed) return ;


                }
                break;
            case 105 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:686: RBRACE_QUOTE_STRING_LITERAL
                {
                mRBRACE_QUOTE_STRING_LITERAL(); if (state.failed) return ;


                }
                break;
            case 106 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:714: RBRACE_LBRACE_STRING_LITERAL
                {
                mRBRACE_LBRACE_STRING_LITERAL(); if (state.failed) return ;


                }
                break;
            case 107 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:743: RBRACE
                {
                mRBRACE(); if (state.failed) return ;


                }
                break;
            case 108 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:750: FORMAT_STRING_LITERAL
                {
                mFORMAT_STRING_LITERAL(); if (state.failed) return ;


                }
                break;
            case 109 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:772: TRANSLATION_KEY
                {
                mTRANSLATION_KEY(); if (state.failed) return ;


                }
                break;
            case 110 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:788: TIME_LITERAL
                {
                mTIME_LITERAL(); if (state.failed) return ;


                }
                break;
            case 111 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:801: DECIMAL_LITERAL
                {
                mDECIMAL_LITERAL(); if (state.failed) return ;


                }
                break;
            case 112 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:817: OCTAL_LITERAL
                {
                mOCTAL_LITERAL(); if (state.failed) return ;


                }
                break;
            case 113 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:831: HEX_LITERAL
                {
                mHEX_LITERAL(); if (state.failed) return ;


                }
                break;
            case 114 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:843: FLOATING_POINT_LITERAL
                {
                mFLOATING_POINT_LITERAL(); if (state.failed) return ;


                }
                break;
            case 115 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:866: IDENTIFIER
                {
                mIDENTIFIER(); if (state.failed) return ;


                }
                break;
            case 116 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:877: WS
                {
                mWS(); if (state.failed) return ;


                }
                break;
            case 117 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:880: COMMENT
                {
                mCOMMENT(); if (state.failed) return ;


                }
                break;
            case 118 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:888: LINE_COMMENT
                {
                mLINE_COMMENT(); if (state.failed) return ;


                }
                break;
            case 119 :
                // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:1:901: LAST_TOKEN
                {
                mLAST_TOKEN(); if (state.failed) return ;


                }
                break;

        }

    }

    // $ANTLR start synpred1_v3
    public final void synpred1_v3_fragment() throws RecognitionException {   
        // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:333:8: ( ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )* '%' )
        // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:333:9: ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )* '%'
        {
        // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:333:9: ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )*
        loop35:
        do {
            int alt35=2;
            int LA35_0 = input.LA(1);

            if ( ((LA35_0>='\t' && LA35_0<='\n')||(LA35_0>='\f' && LA35_0<='\r')||LA35_0==' ') ) {
                alt35=1;
            }


            switch (alt35) {
        	case 1 :
        	    // E:\\SunWork\\nbjfxp\\localrep\\release61\\contrib\\javafx.lexer/src/org/netbeans/lib/javafx/lexer/v3.g:
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
        	    break loop35;
            }
        } while (true);

        match('%'); if (state.failed) return ;


        }
    }
    // $ANTLR end synpred1_v3

    public final boolean synpred1_v3() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred1_v3_fragment(); // can never throw exception
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
    protected DFA14 dfa14 = new DFA14(this);
    protected DFA23 dfa23 = new DFA23(this);
    protected DFA34 dfa34 = new DFA34(this);
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
        "\1\5\1\2\1\4\1\0\1\3\1\1\1\6\2\uffff}>";
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
                        int LA3_3 = input.LA(1);

                         
                        int index3_3 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA3_3>='\u0000' && LA3_3<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 6;}

                         
                        input.seek(index3_3);
                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA3_5 = input.LA(1);

                         
                        int index3_5 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA3_5>='\u0000' && LA3_5<='z')||(LA3_5>='|' && LA3_5<='\uFFFE')) && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 7;}

                        else s = 8;

                         
                        input.seek(index3_5);
                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA3_1 = input.LA(1);

                         
                        int index3_1 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA3_1>='\u0000' && LA3_1<='!')||(LA3_1>='#' && LA3_1<='&')||(LA3_1>='(' && LA3_1<='[')||(LA3_1>=']' && LA3_1<='z')||(LA3_1>='|' && LA3_1<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 2;}

                        else if ( (LA3_1=='\\') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 3;}

                        else if ( (LA3_1=='\"') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 4;}

                        else if ( (LA3_1=='\'') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 5;}

                         
                        input.seek(index3_1);
                        if ( s>=0 ) return s;
                        break;
                    case 3 : 
                        int LA3_4 = input.LA(1);

                         
                        int index3_4 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA3_4>='\u0000' && LA3_4<='z')||(LA3_4>='|' && LA3_4<='\uFFFE')) && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 8;}

                        else s = 7;

                         
                        input.seek(index3_4);
                        if ( s>=0 ) return s;
                        break;
                    case 4 : 
                        int LA3_2 = input.LA(1);

                         
                        int index3_2 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA3_2=='\'') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 5;}

                        else if ( (LA3_2=='\"') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 4;}

                        else if ( (LA3_2=='\\') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 3;}

                        else if ( ((LA3_2>='\u0000' && LA3_2<='!')||(LA3_2>='#' && LA3_2<='&')||(LA3_2>='(' && LA3_2<='[')||(LA3_2>=']' && LA3_2<='z')||(LA3_2>='|' && LA3_2<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 2;}

                         
                        input.seek(index3_2);
                        if ( s>=0 ) return s;
                        break;
                    case 5 : 
                        int LA3_0 = input.LA(1);

                         
                        int index3_0 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA3_0=='}') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 1;}

                         
                        input.seek(index3_0);
                        if ( s>=0 ) return s;
                        break;
                    case 6 : 
                        int LA3_6 = input.LA(1);

                         
                        int index3_6 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA3_6=='\"') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 4;}

                        else if ( (LA3_6=='\'') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 5;}

                        else if ( (LA3_6=='\\') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 3;}

                        else if ( ((LA3_6>='\u0000' && LA3_6<='!')||(LA3_6>='#' && LA3_6<='&')||(LA3_6>='(' && LA3_6<='[')||(LA3_6>=']' && LA3_6<='z')||(LA3_6>='|' && LA3_6<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 2;}

                         
                        input.seek(index3_6);
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
        "\1\2\1\4\1\5\1\0\1\3\2\uffff\1\1}>";
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
            return "313:1: RBRACE_LBRACE_STRING_LITERAL : ({...}? => '}' DoubleQuoteBody '{' NextIsPercent[CUR_QUOTE_CTX] | {...}? => '}' SingleQuoteBody '{' NextIsPercent[CUR_QUOTE_CTX] );";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            IntStream input = _input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA4_3 = input.LA(1);

                         
                        int index4_3 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA4_3>='\u0000' && LA4_3<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 7;}

                         
                        input.seek(index4_3);
                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA4_7 = input.LA(1);

                         
                        int index4_7 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_7=='{') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 4;}

                        else if ( ((LA4_7>='\u0000' && LA4_7<='!')||(LA4_7>='#' && LA4_7<='&')||(LA4_7>='(' && LA4_7<='[')||(LA4_7>=']' && LA4_7<='z')||(LA4_7>='|' && LA4_7<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 2;}

                        else if ( (LA4_7=='\\') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 3;}

                        else if ( (LA4_7=='\'') && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 6;}

                        else if ( (LA4_7=='\"') && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 5;}

                         
                        input.seek(index4_7);
                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA4_0 = input.LA(1);

                         
                        int index4_0 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_0=='}') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 1;}

                         
                        input.seek(index4_0);
                        if ( s>=0 ) return s;
                        break;
                    case 3 : 
                        int LA4_4 = input.LA(1);

                         
                        int index4_4 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ( rightBraceLikeQuote(DBL_QUOTE_CTX) ) ) {s = 6;}

                        else if ( ( rightBraceLikeQuote(SNG_QUOTE_CTX) ) ) {s = 5;}

                         
                        input.seek(index4_4);
                        if ( s>=0 ) return s;
                        break;
                    case 4 : 
                        int LA4_1 = input.LA(1);

                         
                        int index4_1 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA4_1>='\u0000' && LA4_1<='!')||(LA4_1>='#' && LA4_1<='&')||(LA4_1>='(' && LA4_1<='[')||(LA4_1>=']' && LA4_1<='z')||(LA4_1>='|' && LA4_1<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 2;}

                        else if ( (LA4_1=='\\') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 3;}

                        else if ( (LA4_1=='{') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 4;}

                        else if ( (LA4_1=='\"') && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 5;}

                        else if ( (LA4_1=='\'') && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 6;}

                         
                        input.seek(index4_1);
                        if ( s>=0 ) return s;
                        break;
                    case 5 : 
                        int LA4_2 = input.LA(1);

                         
                        int index4_2 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA4_2=='{') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 4;}

                        else if ( ((LA4_2>='\u0000' && LA4_2<='!')||(LA4_2>='#' && LA4_2<='&')||(LA4_2>='(' && LA4_2<='[')||(LA4_2>=']' && LA4_2<='z')||(LA4_2>='|' && LA4_2<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 2;}

                        else if ( (LA4_2=='\\') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 3;}

                        else if ( (LA4_2=='\'') && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 6;}

                        else if ( (LA4_2=='\"') && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 5;}

                         
                        input.seek(index4_2);
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
    static final String DFA14_eotS =
        "\7\uffff";
    static final String DFA14_eofS =
        "\7\uffff";
    static final String DFA14_minS =
        "\3\56\3\uffff\1\56";
    static final String DFA14_maxS =
        "\1\71\2\163\3\uffff\1\163";
    static final String DFA14_acceptS =
        "\3\uffff\1\3\1\1\1\2\1\uffff";
    static final String DFA14_specialS =
        "\7\uffff}>";
    static final String[] DFA14_transitionS = {
            "\1\3\1\uffff\1\1\11\2",
            "\1\5\1\uffff\12\5\56\uffff\1\4\4\uffff\1\4\5\uffff\1\4",
            "\1\5\1\uffff\12\6\56\uffff\1\4\4\uffff\1\4\5\uffff\1\4",
            "",
            "",
            "",
            "\1\5\1\uffff\12\6\56\uffff\1\4\4\uffff\1\4\5\uffff\1\4"
    };

    static final short[] DFA14_eot = DFA.unpackEncodedString(DFA14_eotS);
    static final short[] DFA14_eof = DFA.unpackEncodedString(DFA14_eofS);
    static final char[] DFA14_min = DFA.unpackEncodedStringToUnsignedChars(DFA14_minS);
    static final char[] DFA14_max = DFA.unpackEncodedStringToUnsignedChars(DFA14_maxS);
    static final short[] DFA14_accept = DFA.unpackEncodedString(DFA14_acceptS);
    static final short[] DFA14_special = DFA.unpackEncodedString(DFA14_specialS);
    static final short[][] DFA14_transition;

    static {
        int numStates = DFA14_transitionS.length;
        DFA14_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA14_transition[i] = DFA.unpackEncodedString(DFA14_transitionS[i]);
        }
    }

    class DFA14 extends DFA {

        public DFA14(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 14;
            this.eot = DFA14_eot;
            this.eof = DFA14_eof;
            this.min = DFA14_min;
            this.max = DFA14_max;
            this.accept = DFA14_accept;
            this.special = DFA14_special;
            this.transition = DFA14_transition;
        }
        public String getDescription() {
            return "351:16: ( DECIMAL_LITERAL | Digits '.' ( Digits )? ( Exponent )? | '.' Digits ( Exponent )? )";
        }
    }
    static final String DFA23_eotS =
        "\4\uffff\1\12\6\uffff\1\12\1\uffff";
    static final String DFA23_eofS =
        "\15\uffff";
    static final String DFA23_minS =
        "\3\56\1\uffff\3\56\1\uffff\1\56\2\uffff\1\56\1\uffff";
    static final String DFA23_maxS =
        "\1\71\2\145\1\uffff\1\56\2\145\1\uffff\1\145\2\uffff\1\56\1\uffff";
    static final String DFA23_acceptS =
        "\3\uffff\1\4\3\uffff\1\5\1\uffff\1\1\1\3\1\uffff\1\2";
    static final String DFA23_specialS =
        "\15\uffff}>";
    static final String[] DFA23_transitionS = {
            "\1\3\1\uffff\1\1\11\2",
            "\1\4\1\uffff\10\5\2\6\13\uffff\1\7\37\uffff\1\7",
            "\1\4\1\uffff\12\10\13\uffff\1\7\37\uffff\1\7",
            "",
            "\1\11",
            "\1\13\1\uffff\10\5\2\6\13\uffff\1\7\37\uffff\1\7",
            "\1\12\1\uffff\12\6\13\uffff\1\7\37\uffff\1\7",
            "",
            "\1\4\1\uffff\12\10\13\uffff\1\7\37\uffff\1\7",
            "",
            "",
            "\1\14",
            ""
    };

    static final short[] DFA23_eot = DFA.unpackEncodedString(DFA23_eotS);
    static final short[] DFA23_eof = DFA.unpackEncodedString(DFA23_eofS);
    static final char[] DFA23_min = DFA.unpackEncodedStringToUnsignedChars(DFA23_minS);
    static final char[] DFA23_max = DFA.unpackEncodedStringToUnsignedChars(DFA23_maxS);
    static final short[] DFA23_accept = DFA.unpackEncodedString(DFA23_acceptS);
    static final short[] DFA23_special = DFA.unpackEncodedString(DFA23_specialS);
    static final short[][] DFA23_transition;

    static {
        int numStates = DFA23_transitionS.length;
        DFA23_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA23_transition[i] = DFA.unpackEncodedString(DFA23_transitionS[i]);
        }
    }

    class DFA23 extends DFA {

        public DFA23(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 23;
            this.eot = DFA23_eot;
            this.eof = DFA23_eof;
            this.min = DFA23_min;
            this.max = DFA23_max;
            this.accept = DFA23_accept;
            this.special = DFA23_special;
            this.transition = DFA23_transition;
        }
        public String getDescription() {
            return "363:1: FLOATING_POINT_LITERAL : (d= DECIMAL_LITERAL RangeDots | d= OCTAL_LITERAL RangeDots | Digits '.' ( Digits )? ( Exponent )? | '.' Digits ( Exponent )? | Digits Exponent );";
        }
    }
    static final String DFA34_eotS =
        "\1\uffff\17\54\1\134\2\uffff\1\137\1\142\1\uffff\2\54\1\150\4\uffff"+
        "\1\153\1\155\1\160\1\162\1\166\1\170\6\uffff\1\u0082\2\u0087\3\uffff"+
        "\1\54\1\u0090\1\u0092\17\54\1\u00a3\1\54\1\u00aa\6\54\1\u00b2\1"+
        "\u00b3\17\54\10\uffff\3\54\1\uffff\1\u008c\17\uffff\1\u00d2\14\uffff"+
        "\1\u00d7\1\u00d8\3\uffff\1\u008c\1\u00e2\2\uffff\1\u0087\2\54\1"+
        "\uffff\1\54\1\uffff\1\54\1\u00e9\11\54\1\u00f3\4\54\1\uffff\6\54"+
        "\1\uffff\1\u00ff\2\54\1\u0102\1\u0103\2\54\2\uffff\20\54\1\u0116"+
        "\4\54\1\u011b\6\54\1\u0122\17\uffff\1\u008c\2\uffff\2\u008c\4\54"+
        "\1\uffff\1\u012e\10\54\1\uffff\3\54\1\u013a\2\54\1\u013d\2\54\1"+
        "\u0140\1\54\1\uffff\1\u0142\1\u0143\2\uffff\1\u0144\15\54\1\u0152"+
        "\1\u0153\1\54\1\u0155\1\uffff\1\u0156\3\54\1\uffff\2\54\1\u015c"+
        "\1\u015d\2\54\2\uffff\1\u008c\4\uffff\1\u008c\3\54\1\u0163\1\uffff"+
        "\1\u0164\1\u0165\1\54\1\u0167\1\54\1\u0169\1\54\1\u016b\2\54\1\u016e"+
        "\1\uffff\2\54\1\uffff\2\54\1\uffff\1\54\3\uffff\12\54\1\u017e\2"+
        "\54\2\uffff\1\u0181\2\uffff\2\54\1\u0184\1\u0185\1\u0186\2\uffff"+
        "\3\54\1\u018a\1\54\3\uffff\1\u018c\1\uffff\1\54\1\uffff\1\u018e"+
        "\1\uffff\2\54\1\uffff\1\u0191\1\54\1\u0193\7\54\1\u019b\1\54\1\u019d"+
        "\2\54\1\uffff\1\u01a0\1\u01a1\1\uffff\1\54\1\u01a3\3\uffff\3\54"+
        "\1\uffff\1\54\1\uffff\1\54\1\uffff\1\54\1\u01aa\1\uffff\1\u01ab"+
        "\1\uffff\1\54\1\u01ad\1\54\1\u01af\1\54\1\u01b1\1\54\1\uffff\1\54"+
        "\1\uffff\1\u01b4\1\u01b5\2\uffff\1\u01b6\1\uffff\1\54\1\u01b8\1"+
        "\u01b9\1\54\1\u01bb\1\u01bc\2\uffff\1\54\1\uffff\1\u01be\1\uffff"+
        "\1\u01bf\1\uffff\1\54\1\u01c1\3\uffff\1\54\2\uffff\1\u01c3\2\uffff"+
        "\1\54\2\uffff\1\u01c5\1\uffff\1\u01c6\1\uffff\1\u01c7\3\uffff";
    static final String DFA34_eofS =
        "\u01c8\uffff";
    static final String DFA34_minS =
        "\1\11\1\142\1\145\1\141\1\145\1\141\1\146\1\141\1\145\1\156\1\141"+
        "\1\145\1\151\1\150\1\141\1\150\1\43\2\uffff\1\53\1\55\1\uffff\1"+
        "\154\1\157\1\56\4\uffff\2\75\1\74\1\75\1\52\1\0\3\uffff\2\0\1\uffff"+
        "\1\0\2\56\3\uffff\1\163\2\44\1\164\1\144\1\156\1\165\1\145\1\146"+
        "\1\141\1\156\1\164\2\154\1\162\2\156\1\157\1\44\1\160\1\44\1\164"+
        "\1\163\1\167\1\164\1\154\1\145\2\44\1\143\1\163\1\151\1\142\1\141"+
        "\1\160\1\172\1\141\1\145\1\151\1\160\1\145\1\162\1\145\1\164\10"+
        "\uffff\1\163\1\143\1\144\1\uffff\1\60\17\uffff\2\0\1\uffff\2\0\2"+
        "\uffff\4\0\2\uffff\2\0\3\uffff\1\60\2\56\1\uffff\1\56\1\164\1\145"+
        "\1\uffff\1\162\1\uffff\1\145\1\44\1\144\1\156\1\141\1\157\1\163"+
        "\1\164\1\143\1\145\1\163\1\44\1\143\1\141\1\163\1\155\1\uffff\1"+
        "\157\1\145\1\164\1\145\1\157\1\145\1\uffff\1\44\1\164\1\171\2\44"+
        "\1\154\1\162\2\uffff\1\153\1\164\1\166\1\164\1\154\1\144\1\165\1"+
        "\145\1\154\2\145\1\164\1\160\1\163\1\157\1\156\1\44\1\145\1\147"+
        "\2\145\1\44\1\154\1\162\1\150\1\145\1\154\1\145\1\44\1\53\1\0\1"+
        "\uffff\3\0\2\uffff\2\0\2\uffff\2\0\1\uffff\1\60\1\53\1\uffff\2\60"+
        "\2\162\1\151\1\162\1\uffff\1\44\1\144\1\153\1\162\1\163\1\151\1"+
        "\150\1\164\1\145\1\uffff\1\164\1\154\1\164\1\44\1\162\1\170\1\44"+
        "\1\162\1\141\1\44\1\162\1\uffff\2\44\2\uffff\1\44\1\162\1\141\1"+
        "\151\1\141\1\145\1\151\1\157\2\162\1\141\1\162\1\157\1\151\2\44"+
        "\1\167\1\44\1\uffff\1\44\1\147\1\157\1\156\1\uffff\2\145\2\44\1"+
        "\165\1\156\1\uffff\2\60\1\uffff\2\0\2\60\1\141\1\164\1\142\1\44"+
        "\1\uffff\2\44\1\145\1\44\1\156\1\44\1\145\1\44\1\151\1\154\1\44"+
        "\1\uffff\1\164\1\157\1\uffff\1\164\1\156\1\uffff\1\163\3\uffff\1"+
        "\151\1\147\1\156\1\164\2\143\2\156\1\163\1\143\1\44\1\146\1\143"+
        "\2\uffff\1\44\2\uffff\1\145\1\146\3\44\2\uffff\1\163\1\144\1\143"+
        "\1\44\1\165\3\uffff\1\44\1\uffff\1\165\1\uffff\1\44\1\uffff\1\157"+
        "\1\171\1\uffff\1\44\1\146\1\44\1\143\1\145\1\144\1\145\1\151\1\145"+
        "\1\164\1\44\1\154\1\44\2\145\1\uffff\2\44\1\uffff\1\162\1\44\3\uffff"+
        "\1\151\1\163\1\164\1\uffff\1\164\1\uffff\1\145\1\uffff\1\156\1\44"+
        "\1\uffff\1\44\1\uffff\1\145\1\44\1\145\1\44\1\164\1\44\1\145\1\uffff"+
        "\1\171\1\uffff\2\44\2\uffff\1\44\1\uffff\1\166\2\44\1\145\2\44\2"+
        "\uffff\1\157\1\uffff\1\44\1\uffff\1\44\1\uffff\1\144\1\44\3\uffff"+
        "\1\145\2\uffff\1\44\2\uffff\1\146\2\uffff\1\44\1\uffff\1\44\1\uffff"+
        "\1\44\3\uffff";
    static final String DFA34_maxS =
        "\1\ufaff\1\164\1\162\1\157\1\145\1\165\1\156\1\145\1\165\1\166\1"+
        "\165\1\145\1\165\1\171\1\141\1\151\1\43\2\uffff\2\75\1\uffff\1\170"+
        "\1\157\1\71\4\uffff\1\76\1\75\1\76\2\75\1\ufffe\3\uffff\2\ufffe"+
        "\1\uffff\1\ufffe\1\170\1\163\3\uffff\1\163\2\ufaff\1\164\1\144\1"+
        "\156\1\165\1\145\1\146\1\141\1\156\1\164\2\154\1\162\1\156\1\162"+
        "\1\157\1\ufaff\1\160\1\ufaff\1\164\1\172\1\167\1\164\1\154\1\145"+
        "\2\ufaff\1\143\1\163\1\157\1\142\1\166\1\160\1\172\1\145\1\162\1"+
        "\171\1\160\1\145\1\162\1\151\1\164\10\uffff\1\163\1\164\1\144\1"+
        "\uffff\1\163\17\uffff\1\ufffe\1\0\1\uffff\2\ufffe\2\uffff\4\ufffe"+
        "\2\uffff\2\ufffe\3\uffff\1\163\2\145\1\uffff\1\163\1\164\1\145\1"+
        "\uffff\1\162\1\uffff\1\145\1\ufaff\1\144\1\156\1\141\1\157\1\163"+
        "\1\164\1\143\1\145\1\163\1\ufaff\1\143\1\141\1\163\1\155\1\uffff"+
        "\1\157\1\145\2\164\1\157\1\145\1\uffff\1\ufaff\1\164\1\171\2\ufaff"+
        "\1\154\1\162\2\uffff\1\153\1\164\1\166\1\164\1\154\1\144\1\165\1"+
        "\145\1\154\2\145\1\164\1\160\1\163\1\157\1\156\1\ufaff\1\145\1\147"+
        "\2\145\1\ufaff\1\154\1\162\1\150\1\145\1\154\1\145\1\ufaff\1\71"+
        "\1\0\1\uffff\3\ufffe\2\uffff\2\ufffe\2\uffff\2\ufffe\1\uffff\1\163"+
        "\1\71\1\uffff\2\163\2\162\1\151\1\162\1\uffff\1\ufaff\1\144\1\153"+
        "\1\162\1\163\1\151\1\150\1\164\1\145\1\uffff\1\164\1\154\1\164\1"+
        "\ufaff\1\162\1\170\1\ufaff\1\162\1\141\1\ufaff\1\162\1\uffff\2\ufaff"+
        "\2\uffff\1\ufaff\1\162\1\141\1\151\1\141\1\145\1\151\1\157\2\162"+
        "\1\141\1\162\1\157\1\151\2\ufaff\1\167\1\ufaff\1\uffff\1\ufaff\1"+
        "\147\1\157\1\156\1\uffff\2\145\2\ufaff\1\165\1\156\1\uffff\1\71"+
        "\1\163\1\uffff\2\ufffe\1\71\1\163\1\141\1\164\1\142\1\ufaff\1\uffff"+
        "\2\ufaff\1\145\1\ufaff\1\156\1\ufaff\1\145\1\ufaff\1\151\1\154\1"+
        "\ufaff\1\uffff\1\164\1\157\1\uffff\1\164\1\156\1\uffff\1\163\3\uffff"+
        "\1\151\1\147\1\156\1\164\2\143\2\156\1\163\1\143\1\ufaff\1\146\1"+
        "\143\2\uffff\1\ufaff\2\uffff\1\145\1\146\3\ufaff\2\uffff\1\163\1"+
        "\144\1\143\1\ufaff\1\165\3\uffff\1\ufaff\1\uffff\1\165\1\uffff\1"+
        "\ufaff\1\uffff\1\157\1\171\1\uffff\1\ufaff\1\146\1\ufaff\1\143\1"+
        "\145\1\144\1\145\1\151\1\145\1\164\1\ufaff\1\154\1\ufaff\2\145\1"+
        "\uffff\2\ufaff\1\uffff\1\162\1\ufaff\3\uffff\1\151\1\163\1\164\1"+
        "\uffff\1\164\1\uffff\1\145\1\uffff\1\156\1\ufaff\1\uffff\1\ufaff"+
        "\1\uffff\1\145\1\ufaff\1\145\1\ufaff\1\164\1\ufaff\1\145\1\uffff"+
        "\1\171\1\uffff\2\ufaff\2\uffff\1\ufaff\1\uffff\1\166\2\ufaff\1\145"+
        "\2\ufaff\2\uffff\1\157\1\uffff\1\ufaff\1\uffff\1\ufaff\1\uffff\1"+
        "\144\1\ufaff\3\uffff\1\145\2\uffff\1\ufaff\2\uffff\1\146\2\uffff"+
        "\1\ufaff\1\uffff\1\ufaff\1\uffff\1\ufaff\3\uffff";
    static final String DFA34_acceptS =
        "\21\uffff\1\53\1\54\2\uffff\1\57\3\uffff\1\113\1\114\1\115\1\116"+
        "\6\uffff\1\141\1\142\1\143\2\uffff\1\150\3\uffff\1\163\1\164\1\167"+
        "\54\uffff\1\155\1\52\1\55\1\134\1\127\1\56\1\135\1\130\3\uffff\1"+
        "\112\1\uffff\1\117\1\120\1\145\1\121\1\126\1\122\1\124\1\125\1\123"+
        "\1\136\1\131\1\137\1\165\1\166\1\132\2\uffff\1\154\2\uffff\1\146"+
        "\1\147\4\uffff\1\153\1\152\2\uffff\1\161\1\157\1\156\3\uffff\1\162"+
        "\3\uffff\1\62\1\uffff\1\3\20\uffff\1\16\6\uffff\1\73\7\uffff\1\102"+
        "\1\103\37\uffff\1\133\3\uffff\2\151\2\uffff\1\152\1\151\2\uffff"+
        "\1\152\2\uffff\1\160\6\uffff\1\61\11\uffff\1\14\13\uffff\1\23\2"+
        "\uffff\1\24\1\25\22\uffff\1\45\4\uffff\1\50\6\uffff\1\101\2\uffff"+
        "\1\140\10\uffff\1\5\13\uffff\1\72\2\uffff\1\21\2\uffff\1\75\1\uffff"+
        "\1\77\1\100\1\26\15\uffff\1\105\1\43\1\uffff\1\106\1\46\5\uffff"+
        "\1\110\1\65\5\uffff\1\60\1\6\1\7\1\uffff\1\10\1\uffff\1\64\1\uffff"+
        "\1\13\2\uffff\1\71\17\uffff\1\40\2\uffff\1\44\2\uffff\1\144\1\51"+
        "\1\111\3\uffff\1\2\1\uffff\1\63\1\uffff\1\12\2\uffff\1\17\1\uffff"+
        "\1\22\7\uffff\1\34\1\uffff\1\36\2\uffff\1\41\1\42\1\uffff\1\47\6"+
        "\uffff\1\70\1\20\1\uffff\1\76\1\uffff\1\30\1\uffff\1\32\2\uffff"+
        "\1\37\1\104\1\107\1\uffff\1\67\1\1\1\uffff\1\11\1\15\1\uffff\1\27"+
        "\1\31\1\uffff\1\35\1\uffff\1\4\1\uffff\1\33\1\66\1\74";
    static final String DFA34_specialS =
        "\1\0\41\uffff\1\12\6\uffff\1\17\115\uffff\1\2\1\1\7\uffff\1\11\1"+
        "\14\2\uffff\1\16\1\3\114\uffff\1\4\3\uffff\1\5\2\uffff\1\7\1\13"+
        "\2\uffff\1\6\1\20\107\uffff\1\15\1\10\u00a0\uffff}>";
    static final String[] DFA34_transitionS = {
            "\2\55\1\uffff\2\55\22\uffff\1\55\1\43\1\46\1\20\1\54\1\42\1"+
            "\uffff\1\47\1\21\1\31\1\40\1\23\1\34\1\24\1\30\1\41\1\52\11"+
            "\53\1\44\1\33\1\37\1\35\1\36\1\45\1\uffff\32\54\1\22\1\uffff"+
            "\1\32\1\uffff\1\54\1\uffff\1\1\1\2\1\3\1\4\1\26\1\5\2\54\1\6"+
            "\2\54\1\7\1\27\1\10\1\11\1\12\1\54\1\13\1\14\1\15\1\54\1\16"+
            "\1\17\3\54\1\50\1\25\1\51\1\56\101\uffff\27\54\1\uffff\37\54"+
            "\1\uffff\u1f08\54\u1040\uffff\u0150\54\u0170\uffff\u0080\54"+
            "\u0080\uffff\u092e\54\u10d2\uffff\u5200\54\u5900\uffff\u0200"+
            "\54",
            "\1\57\3\uffff\1\62\7\uffff\1\63\4\uffff\1\60\1\61",
            "\1\67\3\uffff\1\64\5\uffff\1\65\2\uffff\1\66",
            "\1\72\12\uffff\1\70\2\uffff\1\71",
            "\1\73",
            "\1\74\7\uffff\1\77\5\uffff\1\75\2\uffff\1\100\2\uffff\1\76",
            "\1\101\6\uffff\1\102\1\103",
            "\1\105\3\uffff\1\104",
            "\1\106\11\uffff\1\107\5\uffff\1\110",
            "\1\112\3\uffff\1\113\3\uffff\1\111",
            "\1\114\15\uffff\1\115\2\uffff\1\116\2\uffff\1\117",
            "\1\120",
            "\1\122\12\uffff\1\123\1\121",
            "\1\124\11\uffff\1\125\4\uffff\1\127\1\uffff\1\126",
            "\1\130",
            "\1\131\1\132",
            "\1\133",
            "",
            "",
            "\1\135\21\uffff\1\136",
            "\1\140\17\uffff\1\141",
            "",
            "\1\143\13\uffff\1\144",
            "\1\145",
            "\1\146\1\uffff\12\147",
            "",
            "",
            "",
            "",
            "\1\151\1\152",
            "\1\154",
            "\1\54\1\157\1\156",
            "\1\161",
            "\1\164\4\uffff\1\165\15\uffff\1\163",
            "\40\171\1\uffff\34\171\1\167\uffc1\171",
            "",
            "",
            "",
            "\42\172\1\174\71\172\1\173\36\172\1\175\uff83\172",
            "\47\176\1\174\64\176\1\177\36\176\1\175\uff83\176",
            "",
            "\42\u0080\1\u0084\4\u0080\1\u0085\64\u0080\1\u0081\36\u0080"+
            "\1\u0083\uff83\u0080",
            "\1\u0089\1\uffff\10\u008a\2\u008b\13\uffff\1\u008c\22\uffff"+
            "\1\u0086\14\uffff\1\u008c\2\uffff\1\u0088\4\uffff\1\u0088\5"+
            "\uffff\1\u0088\4\uffff\1\u0086",
            "\1\u0089\1\uffff\12\u008d\13\uffff\1\u008c\37\uffff\1\u008c"+
            "\2\uffff\1\u0088\4\uffff\1\u0088\5\uffff\1\u0088",
            "",
            "",
            "",
            "\1\u008e",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\22\54"+
            "\1\u008f\7\54\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54"+
            "\u1040\uffff\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e"+
            "\54\u10d2\uffff\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\23\54"+
            "\1\u0091\6\54\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54"+
            "\u1040\uffff\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e"+
            "\54\u10d2\uffff\u5200\54\u5900\uffff\u0200\54",
            "\1\u0093",
            "\1\u0094",
            "\1\u0095",
            "\1\u0096",
            "\1\u0097",
            "\1\u0098",
            "\1\u0099",
            "\1\u009a",
            "\1\u009b",
            "\1\u009c",
            "\1\u009d",
            "\1\u009e",
            "\1\u009f",
            "\1\u00a0\3\uffff\1\u00a1",
            "\1\u00a2",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u00a4",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\3\54"+
            "\1\u00a5\4\54\1\u00a6\11\54\1\u00a7\1\u00a8\1\54\1\u00a9\4\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u00ab",
            "\1\u00ac\6\uffff\1\u00ad",
            "\1\u00ae",
            "\1\u00af",
            "\1\u00b0",
            "\1\u00b1",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u00b4",
            "\1\u00b5",
            "\1\u00b6\5\uffff\1\u00b7",
            "\1\u00b8",
            "\1\u00b9\16\uffff\1\u00bc\3\uffff\1\u00ba\1\uffff\1\u00bb",
            "\1\u00bd",
            "\1\u00be",
            "\1\u00bf\3\uffff\1\u00c0",
            "\1\u00c3\3\uffff\1\u00c1\10\uffff\1\u00c2",
            "\1\u00c6\13\uffff\1\u00c5\3\uffff\1\u00c4",
            "\1\u00c7",
            "\1\u00c8",
            "\1\u00c9",
            "\1\u00cb\3\uffff\1\u00ca",
            "\1\u00cc",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "\1\u00cd",
            "\1\u00ce\20\uffff\1\u00cf",
            "\1\u00d0",
            "",
            "\12\147\13\uffff\1\u00d1\37\uffff\1\u00d1\2\uffff\1\u0088\4"+
            "\uffff\1\u0088\5\uffff\1\u0088",
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
            "\40\171\1\uffff\uffde\171",
            "\1\uffff",
            "",
            "\42\172\1\174\71\172\1\173\36\172\1\175\uff83\172",
            "\uffff\u00d4",
            "",
            "",
            "\47\176\1\174\64\176\1\177\36\176\1\175\uff83\176",
            "\uffff\u00d5",
            "\42\u0080\1\u0084\4\u0080\1\u0085\64\u0080\1\u0081\36\u0080"+
            "\1\u0083\uff83\u0080",
            "\uffff\u00d6",
            "",
            "",
            "\47\u00d9\1\u00d8\64\u00d9\1\u00da\36\u00d9\1\u00db\uff83\u00d9",
            "\42\u00dd\1\u00dc\71\u00dd\1\u00de\36\u00dd\1\u00df\uff83\u00dd",
            "",
            "",
            "",
            "\12\u00e0\13\uffff\1\u00e1\37\uffff\1\u00e1\2\uffff\1\u0088"+
            "\4\uffff\1\u0088\5\uffff\1\u0088",
            "\1\u00e3\1\uffff\10\u008a\2\u008b\13\uffff\1\u008c\37\uffff"+
            "\1\u008c",
            "\1\u00e4\1\uffff\12\u008b\13\uffff\1\u008c\37\uffff\1\u008c",
            "",
            "\1\u0089\1\uffff\12\u008d\13\uffff\1\u008c\37\uffff\1\u008c"+
            "\2\uffff\1\u0088\4\uffff\1\u0088\5\uffff\1\u0088",
            "\1\u00e5",
            "\1\u00e6",
            "",
            "\1\u00e7",
            "",
            "\1\u00e8",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u00ea",
            "\1\u00eb",
            "\1\u00ec",
            "\1\u00ed",
            "\1\u00ee",
            "\1\u00ef",
            "\1\u00f0",
            "\1\u00f1",
            "\1\u00f2",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u00f4",
            "\1\u00f5",
            "\1\u00f6",
            "\1\u00f7",
            "",
            "\1\u00f8",
            "\1\u00f9",
            "\1\u00fa",
            "\1\u00fb\16\uffff\1\u00fc",
            "\1\u00fd",
            "\1\u00fe",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u0100",
            "\1\u0101",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u0104",
            "\1\u0105",
            "",
            "",
            "\1\u0106",
            "\1\u0107",
            "\1\u0108",
            "\1\u0109",
            "\1\u010a",
            "\1\u010b",
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
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u0117",
            "\1\u0118",
            "\1\u0119",
            "\1\u011a",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u011c",
            "\1\u011d",
            "\1\u011e",
            "\1\u011f",
            "\1\u0120",
            "\1\u0121",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u0123\1\uffff\1\u0123\2\uffff\12\u0124",
            "\1\uffff",
            "",
            "\42\172\1\174\71\172\1\173\36\172\1\175\uff83\172",
            "\47\176\1\174\64\176\1\177\36\176\1\175\uff83\176",
            "\42\u0080\1\u0084\4\u0080\1\u0085\64\u0080\1\u0081\36\u0080"+
            "\1\u0083\uff83\u0080",
            "",
            "",
            "\47\u00d9\1\u00d8\64\u00d9\1\u00da\36\u00d9\1\u00db\uff83\u00d9",
            "\uffff\u0126",
            "",
            "",
            "\42\u00dd\1\u00dc\71\u00dd\1\u00de\36\u00dd\1\u00df\uff83\u00dd",
            "\uffff\u0127",
            "",
            "\12\u00e0\13\uffff\1\u00e1\37\uffff\1\u00e1\2\uffff\1\u0088"+
            "\4\uffff\1\u0088\5\uffff\1\u0088",
            "\1\u0128\1\uffff\1\u0128\2\uffff\12\u0129",
            "",
            "\12\u00e0\13\uffff\1\u00e1\37\uffff\1\u00e1\2\uffff\1\u0088"+
            "\4\uffff\1\u0088\5\uffff\1\u0088",
            "\12\u00e0\13\uffff\1\u00e1\37\uffff\1\u00e1\2\uffff\1\u0088"+
            "\4\uffff\1\u0088\5\uffff\1\u0088",
            "\1\u012a",
            "\1\u012b",
            "\1\u012c",
            "\1\u012d",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u012f",
            "\1\u0130",
            "\1\u0131",
            "\1\u0132",
            "\1\u0133",
            "\1\u0134",
            "\1\u0135",
            "\1\u0136",
            "",
            "\1\u0137",
            "\1\u0138",
            "\1\u0139",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u013b",
            "\1\u013c",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u013e",
            "\1\u013f",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u0141",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u0145",
            "\1\u0146",
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
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u0154",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u0157",
            "\1\u0158",
            "\1\u0159",
            "",
            "\1\u015a",
            "\1\u015b",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u015e",
            "\1\u015f",
            "",
            "\12\u0124",
            "\12\u0124\56\uffff\1\u0088\4\uffff\1\u0088\5\uffff\1\u0088",
            "",
            "\47\u00d9\1\u00d8\64\u00d9\1\u00da\36\u00d9\1\u00db\uff83\u00d9",
            "\42\u00dd\1\u00dc\71\u00dd\1\u00de\36\u00dd\1\u00df\uff83\u00dd",
            "\12\u0129",
            "\12\u0129\56\uffff\1\u0088\4\uffff\1\u0088\5\uffff\1\u0088",
            "\1\u0160",
            "\1\u0161",
            "\1\u0162",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u0166",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u0168",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u016a",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u016c",
            "\1\u016d",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\u016f",
            "\1\u0170",
            "",
            "\1\u0171",
            "\1\u0172",
            "",
            "\1\u0173",
            "",
            "",
            "",
            "\1\u0174",
            "\1\u0175",
            "\1\u0176",
            "\1\u0177",
            "\1\u0178",
            "\1\u0179",
            "\1\u017a",
            "\1\u017b",
            "\1\u017c",
            "\1\u017d",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u017f",
            "\1\u0180",
            "",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "",
            "\1\u0182",
            "\1\u0183",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "",
            "\1\u0187",
            "\1\u0188",
            "\1\u0189",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u018b",
            "",
            "",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\u018d",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\u018f",
            "\1\u0190",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u0192",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u0194",
            "\1\u0195",
            "\1\u0196",
            "\1\u0197",
            "\1\u0198",
            "\1\u0199",
            "\1\u019a",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u019c",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u019e",
            "\1\u019f",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\u01a2",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "",
            "",
            "\1\u01a4",
            "\1\u01a5",
            "\1\u01a6",
            "",
            "\1\u01a7",
            "",
            "\1\u01a8",
            "",
            "\1\u01a9",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\u01ac",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u01ae",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u01b0",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u01b2",
            "",
            "\1\u01b3",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\u01b7",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\u01ba",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "",
            "\1\u01bd",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\u01c0",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "",
            "",
            "\1\u01c2",
            "",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "",
            "\1\u01c4",
            "",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "\1\54\13\uffff\12\54\7\uffff\32\54\4\uffff\1\54\1\uffff\32\54"+
            "\105\uffff\27\54\1\uffff\37\54\1\uffff\u1f08\54\u1040\uffff"+
            "\u0150\54\u0170\uffff\u0080\54\u0080\uffff\u092e\54\u10d2\uffff"+
            "\u5200\54\u5900\uffff\u0200\54",
            "",
            "",
            ""
    };

    static final short[] DFA34_eot = DFA.unpackEncodedString(DFA34_eotS);
    static final short[] DFA34_eof = DFA.unpackEncodedString(DFA34_eofS);
    static final char[] DFA34_min = DFA.unpackEncodedStringToUnsignedChars(DFA34_minS);
    static final char[] DFA34_max = DFA.unpackEncodedStringToUnsignedChars(DFA34_maxS);
    static final short[] DFA34_accept = DFA.unpackEncodedString(DFA34_acceptS);
    static final short[] DFA34_special = DFA.unpackEncodedString(DFA34_specialS);
    static final short[][] DFA34_transition;

    static {
        int numStates = DFA34_transitionS.length;
        DFA34_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA34_transition[i] = DFA.unpackEncodedString(DFA34_transitionS[i]);
        }
    }

    class DFA34 extends DFA {

        public DFA34(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 34;
            this.eot = DFA34_eot;
            this.eof = DFA34_eof;
            this.min = DFA34_min;
            this.max = DFA34_max;
            this.accept = DFA34_accept;
            this.special = DFA34_special;
            this.transition = DFA34_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( ABSTRACT | ASSERT | AT | ATTRIBUTE | BIND | BOUND | BREAK | CLASS | CONTINUE | DELETE | FALSE | FOR | FUNCTION | IF | IMPORT | INDEXOF | INIT | INSERT | LET | NEW | NOT | NULL | OVERRIDE | PACKAGE | POSTINIT | PRIVATE | PROTECTED | PUBLIC | READONLY | RETURN | REVERSE | SUPER | SIZEOF | STATIC | THIS | THROW | TRY | TRUE | TYPEOF | VAR | WHILE | POUND | LPAREN | LBRACKET | PLUSPLUS | SUBSUB | PIPE | AFTER | AND | AS | BEFORE | CATCH | ELSE | EXCLUSIVE | EXTENDS | FINALLY | FIRST | FROM | IN | INSTANCEOF | INTO | INVERSE | LAST | LAZY | MOD | ON | OR | REPLACE | STEP | THEN | TRIGGER | WITH | WHERE | DOTDOT | RPAREN | RBRACKET | SEMI | COMMA | DOT | EQEQ | EQ | GT | LT | LTGT | LTEQ | GTEQ | PLUS | SUB | STAR | SLASH | PERCENT | PLUSEQ | SUBEQ | STAREQ | SLASHEQ | PERCENTEQ | NOTEQ | COLON | QUES | TWEEN | SUCHTHAT | STRING_LITERAL | QUOTE_LBRACE_STRING_LITERAL | LBRACE | RBRACE_QUOTE_STRING_LITERAL | RBRACE_LBRACE_STRING_LITERAL | RBRACE | FORMAT_STRING_LITERAL | TRANSLATION_KEY | TIME_LITERAL | DECIMAL_LITERAL | OCTAL_LITERAL | HEX_LITERAL | FLOATING_POINT_LITERAL | IDENTIFIER | WS | COMMENT | LINE_COMMENT | LAST_TOKEN );";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            IntStream input = _input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA34_0 = input.LA(1);

                         
                        int index34_0 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA34_0=='a') ) {s = 1;}

                        else if ( (LA34_0=='b') ) {s = 2;}

                        else if ( (LA34_0=='c') ) {s = 3;}

                        else if ( (LA34_0=='d') ) {s = 4;}

                        else if ( (LA34_0=='f') ) {s = 5;}

                        else if ( (LA34_0=='i') ) {s = 6;}

                        else if ( (LA34_0=='l') ) {s = 7;}

                        else if ( (LA34_0=='n') ) {s = 8;}

                        else if ( (LA34_0=='o') ) {s = 9;}

                        else if ( (LA34_0=='p') ) {s = 10;}

                        else if ( (LA34_0=='r') ) {s = 11;}

                        else if ( (LA34_0=='s') ) {s = 12;}

                        else if ( (LA34_0=='t') ) {s = 13;}

                        else if ( (LA34_0=='v') ) {s = 14;}

                        else if ( (LA34_0=='w') ) {s = 15;}

                        else if ( (LA34_0=='#') ) {s = 16;}

                        else if ( (LA34_0=='(') ) {s = 17;}

                        else if ( (LA34_0=='[') ) {s = 18;}

                        else if ( (LA34_0=='+') ) {s = 19;}

                        else if ( (LA34_0=='-') ) {s = 20;}

                        else if ( (LA34_0=='|') ) {s = 21;}

                        else if ( (LA34_0=='e') ) {s = 22;}

                        else if ( (LA34_0=='m') ) {s = 23;}

                        else if ( (LA34_0=='.') ) {s = 24;}

                        else if ( (LA34_0==')') ) {s = 25;}

                        else if ( (LA34_0==']') ) {s = 26;}

                        else if ( (LA34_0==';') ) {s = 27;}

                        else if ( (LA34_0==',') ) {s = 28;}

                        else if ( (LA34_0=='=') ) {s = 29;}

                        else if ( (LA34_0=='>') ) {s = 30;}

                        else if ( (LA34_0=='<') ) {s = 31;}

                        else if ( (LA34_0=='*') ) {s = 32;}

                        else if ( (LA34_0=='/') ) {s = 33;}

                        else if ( (LA34_0=='%') ) {s = 34;}

                        else if ( (LA34_0=='!') ) {s = 35;}

                        else if ( (LA34_0==':') ) {s = 36;}

                        else if ( (LA34_0=='?') ) {s = 37;}

                        else if ( (LA34_0=='\"') ) {s = 38;}

                        else if ( (LA34_0=='\'') ) {s = 39;}

                        else if ( (LA34_0=='{') ) {s = 40;}

                        else if ( (LA34_0=='}') && (( !rightBraceLikeQuote(CUR_QUOTE_CTX) || rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 41;}

                        else if ( (LA34_0=='0') ) {s = 42;}

                        else if ( ((LA34_0>='1' && LA34_0<='9')) ) {s = 43;}

                        else if ( (LA34_0=='$'||(LA34_0>='A' && LA34_0<='Z')||LA34_0=='_'||(LA34_0>='g' && LA34_0<='h')||(LA34_0>='j' && LA34_0<='k')||LA34_0=='q'||LA34_0=='u'||(LA34_0>='x' && LA34_0<='z')||(LA34_0>='\u00C0' && LA34_0<='\u00D6')||(LA34_0>='\u00D8' && LA34_0<='\u00F6')||(LA34_0>='\u00F8' && LA34_0<='\u1FFF')||(LA34_0>='\u3040' && LA34_0<='\u318F')||(LA34_0>='\u3300' && LA34_0<='\u337F')||(LA34_0>='\u3400' && LA34_0<='\u3D2D')||(LA34_0>='\u4E00' && LA34_0<='\u9FFF')||(LA34_0>='\uF900' && LA34_0<='\uFAFF')) ) {s = 44;}

                        else if ( ((LA34_0>='\t' && LA34_0<='\n')||(LA34_0>='\f' && LA34_0<='\r')||LA34_0==' ') ) {s = 45;}

                        else if ( (LA34_0=='~') ) {s = 46;}

                         
                        input.seek(index34_0);
                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA34_120 = input.LA(1);

                         
                        int index34_120 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (!( percentIsFormat() )) ) {s = 211;}

                        else if ( ( percentIsFormat() ) ) {s = 121;}

                         
                        input.seek(index34_120);
                        if ( s>=0 ) return s;
                        break;
                    case 2 : 
                        int LA34_119 = input.LA(1);

                         
                        int index34_119 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA34_119>='\u0000' && LA34_119<='\u001F')||(LA34_119>='!' && LA34_119<='\uFFFE')) && ( percentIsFormat() )) {s = 121;}

                        else s = 210;

                         
                        input.seek(index34_119);
                        if ( s>=0 ) return s;
                        break;
                    case 3 : 
                        int LA34_133 = input.LA(1);

                         
                        int index34_133 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA34_133=='\"') && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 220;}

                        else if ( ((LA34_133>='\u0000' && LA34_133<='!')||(LA34_133>='#' && LA34_133<='[')||(LA34_133>=']' && LA34_133<='z')||(LA34_133>='|' && LA34_133<='\uFFFE')) && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 221;}

                        else if ( (LA34_133=='\\') && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 222;}

                        else if ( (LA34_133=='{') && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 223;}

                        else s = 216;

                         
                        input.seek(index34_133);
                        if ( s>=0 ) return s;
                        break;
                    case 4 : 
                        int LA34_210 = input.LA(1);

                         
                        int index34_210 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (!( percentIsFormat() )) ) {s = 293;}

                        else if ( ( percentIsFormat() ) ) {s = 121;}

                         
                        input.seek(index34_210);
                        if ( s>=0 ) return s;
                        break;
                    case 5 : 
                        int LA34_214 = input.LA(1);

                         
                        int index34_214 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA34_214=='{') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 131;}

                        else if ( ((LA34_214>='\u0000' && LA34_214<='!')||(LA34_214>='#' && LA34_214<='&')||(LA34_214>='(' && LA34_214<='[')||(LA34_214>=']' && LA34_214<='z')||(LA34_214>='|' && LA34_214<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 128;}

                        else if ( (LA34_214=='\\') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 129;}

                        else if ( (LA34_214=='\"') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 132;}

                        else if ( (LA34_214=='\'') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 133;}

                         
                        input.seek(index34_214);
                        if ( s>=0 ) return s;
                        break;
                    case 6 : 
                        int LA34_221 = input.LA(1);

                         
                        int index34_221 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA34_221=='\"') && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 220;}

                        else if ( ((LA34_221>='\u0000' && LA34_221<='!')||(LA34_221>='#' && LA34_221<='[')||(LA34_221>=']' && LA34_221<='z')||(LA34_221>='|' && LA34_221<='\uFFFE')) && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 221;}

                        else if ( (LA34_221=='\\') && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 222;}

                        else if ( (LA34_221=='{') && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 223;}

                         
                        input.seek(index34_221);
                        if ( s>=0 ) return s;
                        break;
                    case 7 : 
                        int LA34_217 = input.LA(1);

                         
                        int index34_217 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA34_217=='\'') && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 216;}

                        else if ( ((LA34_217>='\u0000' && LA34_217<='&')||(LA34_217>='(' && LA34_217<='[')||(LA34_217>=']' && LA34_217<='z')||(LA34_217>='|' && LA34_217<='\uFFFE')) && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 217;}

                        else if ( (LA34_217=='\\') && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 218;}

                        else if ( (LA34_217=='{') && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 219;}

                         
                        input.seek(index34_217);
                        if ( s>=0 ) return s;
                        break;
                    case 8 : 
                        int LA34_295 = input.LA(1);

                         
                        int index34_295 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA34_295=='{') && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 223;}

                        else if ( ((LA34_295>='\u0000' && LA34_295<='!')||(LA34_295>='#' && LA34_295<='[')||(LA34_295>=']' && LA34_295<='z')||(LA34_295>='|' && LA34_295<='\uFFFE')) && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 221;}

                        else if ( (LA34_295=='\\') && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 222;}

                        else if ( (LA34_295=='\"') && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 220;}

                         
                        input.seek(index34_295);
                        if ( s>=0 ) return s;
                        break;
                    case 9 : 
                        int LA34_128 = input.LA(1);

                         
                        int index34_128 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA34_128=='\'') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 133;}

                        else if ( (LA34_128=='\"') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 132;}

                        else if ( (LA34_128=='\\') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 129;}

                        else if ( ((LA34_128>='\u0000' && LA34_128<='!')||(LA34_128>='#' && LA34_128<='&')||(LA34_128>='(' && LA34_128<='[')||(LA34_128>=']' && LA34_128<='z')||(LA34_128>='|' && LA34_128<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 128;}

                        else if ( (LA34_128=='{') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 131;}

                         
                        input.seek(index34_128);
                        if ( s>=0 ) return s;
                        break;
                    case 10 : 
                        int LA34_34 = input.LA(1);

                         
                        int index34_34 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA34_34=='=') ) {s = 119;}

                        else if ( ((LA34_34>='\u0000' && LA34_34<='\u001F')||(LA34_34>='!' && LA34_34<='<')||(LA34_34>='>' && LA34_34<='\uFFFE')) && ( percentIsFormat() )) {s = 121;}

                        else s = 120;

                         
                        input.seek(index34_34);
                        if ( s>=0 ) return s;
                        break;
                    case 11 : 
                        int LA34_218 = input.LA(1);

                         
                        int index34_218 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA34_218>='\u0000' && LA34_218<='\uFFFE')) && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 294;}

                         
                        input.seek(index34_218);
                        if ( s>=0 ) return s;
                        break;
                    case 12 : 
                        int LA34_129 = input.LA(1);

                         
                        int index34_129 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA34_129>='\u0000' && LA34_129<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 214;}

                         
                        input.seek(index34_129);
                        if ( s>=0 ) return s;
                        break;
                    case 13 : 
                        int LA34_294 = input.LA(1);

                         
                        int index34_294 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA34_294=='{') && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 219;}

                        else if ( ((LA34_294>='\u0000' && LA34_294<='&')||(LA34_294>='(' && LA34_294<='[')||(LA34_294>=']' && LA34_294<='z')||(LA34_294>='|' && LA34_294<='\uFFFE')) && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 217;}

                        else if ( (LA34_294=='\\') && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 218;}

                        else if ( (LA34_294=='\'') && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 216;}

                         
                        input.seek(index34_294);
                        if ( s>=0 ) return s;
                        break;
                    case 14 : 
                        int LA34_132 = input.LA(1);

                         
                        int index34_132 = input.index();
                        input.rewind();
                        s = -1;
                        if ( (LA34_132=='\'') && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 216;}

                        else if ( ((LA34_132>='\u0000' && LA34_132<='&')||(LA34_132>='(' && LA34_132<='[')||(LA34_132>=']' && LA34_132<='z')||(LA34_132>='|' && LA34_132<='\uFFFE')) && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 217;}

                        else if ( (LA34_132=='\\') && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 218;}

                        else if ( (LA34_132=='{') && ( rightBraceLikeQuote(SNG_QUOTE_CTX) )) {s = 219;}

                        else s = 215;

                         
                        input.seek(index34_132);
                        if ( s>=0 ) return s;
                        break;
                    case 15 : 
                        int LA34_41 = input.LA(1);

                         
                        int index34_41 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA34_41>='\u0000' && LA34_41<='!')||(LA34_41>='#' && LA34_41<='&')||(LA34_41>='(' && LA34_41<='[')||(LA34_41>=']' && LA34_41<='z')||(LA34_41>='|' && LA34_41<='\uFFFE')) && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 128;}

                        else if ( (LA34_41=='\\') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 129;}

                        else if ( (LA34_41=='{') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 131;}

                        else if ( (LA34_41=='\"') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 132;}

                        else if ( (LA34_41=='\'') && (( rightBraceLikeQuote(DBL_QUOTE_CTX) || rightBraceLikeQuote(SNG_QUOTE_CTX) ))) {s = 133;}

                        else s = 130;

                         
                        input.seek(index34_41);
                        if ( s>=0 ) return s;
                        break;
                    case 16 : 
                        int LA34_222 = input.LA(1);

                         
                        int index34_222 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA34_222>='\u0000' && LA34_222<='\uFFFE')) && ( rightBraceLikeQuote(DBL_QUOTE_CTX) )) {s = 295;}

                         
                        input.seek(index34_222);
                        if ( s>=0 ) return s;
                        break;
            }
            if (state.backtracking>0) {state.failed=true; return -1;}
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 34, _s, input);
            error(nvae);
            throw nvae;
        }
    }
 

}