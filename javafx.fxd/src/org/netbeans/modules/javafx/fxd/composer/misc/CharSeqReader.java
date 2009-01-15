/*
 *  Copyright 2008 Sun Microsystems, Inc. All rights reserved.
 *  SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package org.netbeans.modules.javafx.fxd.composer.misc;

import java.io.IOException;
import java.io.Reader;
import org.netbeans.editor.CharSeq;

/**
 *
 * @author Pavel Benes
 */
public class CharSeqReader extends Reader {
    private final CharSeq m_charSeq;
    private       int     m_pos;

    public CharSeqReader( CharSeq charSeq) {
        m_charSeq = charSeq;
        m_pos     = 0;
    }

    @Override
    public int read(char[] cbuf, int off, int len) throws IOException {
        int available = m_charSeq.length() - m_pos;

        if ( available > 0) {
            if ( len > available) {
                len = available;
            }
            int endOff = off + len;
            while( off < endOff) {
                cbuf[off++] = m_charSeq.charAt(m_pos++);
            }
            return len;
        } else {
            return -1;
        }
    }

    @Override
    public void close() throws IOException {
    }
}
