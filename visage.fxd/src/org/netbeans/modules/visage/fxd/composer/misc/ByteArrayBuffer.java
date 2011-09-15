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
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 */
package org.netbeans.modules.visage.fxd.composer.misc;

import java.io.*;

public final class ByteArrayBuffer extends OutputStream {
    private byte m_buf[];
    private int m_count;

    public ByteArrayBuffer() {
        this(32);
    }

    public ByteArrayBuffer(int capacity) {
        if(capacity <= 0) {
            throw new IllegalArgumentException();
        } else {
            m_buf = new byte[capacity];
            return;
        }
    }

    public ByteArrayBuffer(byte [] buff) {
        m_buf = buff;
    }

    public void write(InputStream inputstream) throws IOException {
        do {
            int i;
            int j;
            do {
                i = m_buf.length - m_count;
                j = inputstream.read(m_buf, m_count, i);
                if(j < 0)
                    return;
                m_count += j;
            } while(i != j);
            ensureCapacity(m_buf.length * 2);
        } while(true);
    }

    public void write(int b) {
        int j = m_count + 1;
        ensureCapacity(j);
        m_buf[m_count] = (byte)b;
        m_count = j;
    }

    @Override
    public void write(byte abyte0[], int i, int j) {
        int k = m_count + j;
        ensureCapacity(k);
        System.arraycopy(abyte0, i, m_buf, m_count, j);
        m_count = k;
    }

    private void ensureCapacity(int capacity) {
        if(capacity > m_buf.length) {
            byte abyte0[] = new byte[Math.max(m_buf.length << 1, capacity)];
            System.arraycopy(m_buf, 0, abyte0, 0, m_count);
            m_buf = abyte0;
        }
    }

    public void writeTo(OutputStream outputstream) throws IOException {
        outputstream.write(m_buf, 0, m_count);
    }

    public void reset() {
        m_count = 0;
    }

    public int size() {
        return m_count;
    }

    @Override
    public void close() {
    }

    public InputStream newInputStream() {
        return new ByteArrayInputStream(m_buf, 0, m_count);
    }

    public InputStream newInputStream(int i, int j) {
        return new ByteArrayInputStream(m_buf, i, j);
    }
}
