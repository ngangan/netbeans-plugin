/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package org.netbeans.modules.javafx.fxd.composer.source;

import org.netbeans.editor.CharSeq;

/**
 *
 * @author Pavel Benes
 */
public class TextParser {
    public enum Direction {
        FORWARD,
        BACKWARD
    };
    
    private final CharSeq m_text;
    private       int     m_position;
    
    public TextParser( final CharSeq text) {
        m_text = text;
        m_position = 0;
    }
    
    public int getPosition() {
        return m_position;
    }
    
    public int getSize() {
        return m_text.length();
    }
    
    public boolean gotoChar( final char c) {
        int pos = m_position;
        int ch;
        int p = m_position;
        
        while ( (ch=fetchChar(Direction.FORWARD)) != -1) {
            if ( ch == c) {
                m_position = p;
                assert getChar() == c;
                return true;
            }
            p = m_position;
        }
        m_position = pos;
        return false;
    }
    
    public int skipNonWhite( final Direction direction) {
        int ch;
        int pos = m_position;
        
        while( (ch=fetchChar(direction)) != -1) {
            if ( ch <= ' ') {
                m_position = pos;
                assert getChar() == ch;
                return ch;
            }
            pos = m_position;
        }
        return -1;
    }

    public int getChar() {
        if ( m_position >= 0 && m_position < m_text.length()) {
            return m_text.charAt(m_position);
        } else {
            return -1;
        }
    }
    public int skipWhite( final Direction direction) {
        int ch;
        int pos = m_position;
        
        while( (ch=fetchChar(direction)) != -1) {
            if ( ch > ' ') {
                m_position = pos;
                assert getChar() == ch;
                return ch;
            }
            pos = m_position;
        }
        return -1;
    }
    
    public CharSequence subsequence( final int startPos, final int endPos) {
        final int length = endPos - startPos;
        
        return new CharSequence() {
            public int length() {
                return length;
            }

            public char charAt(int index) {
                return m_text.charAt(startPos + index);
            }

            public CharSequence subSequence(int start, int end) {
                throw new UnsupportedOperationException("Not supported yet.");
            }
            
            @Override
            public String toString() {
                StringBuilder sb = new StringBuilder(length);
                for (int i = 0; i < length; i++) {
                    sb.append( charAt(i));
                }
                return sb.toString();
            }
        };
    }
    
    public int fetchChar( final Direction direction) {
        //TODO ignore comments
        if ( direction == Direction.FORWARD) {
            if ( m_position < m_text.length()) {
                return m_text.charAt(m_position++);
            } 
        } else {
            if (m_position > 0) {
                return m_text.charAt(m_position--);
            }
        }
        return -1;
    }
}
