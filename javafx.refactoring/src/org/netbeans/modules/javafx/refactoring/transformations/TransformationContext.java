/*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 *  Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 * 
 *  The contents of this file are subject to the terms of either the GNU
 *  General Public License Version 2 only ("GPL") or the Common
 *  Development and Distribution License("CDDL") (collectively, the
 *  "License"). You may not use this file except in compliance with the
 *  License. You can obtain a copy of the License at
 *  http://www.netbeans.org/cddl-gplv2.html
 *  or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 *  specific language governing permissions and limitations under the
 *  License.  When distributing the software, include this License Header
 *  Notice in each file and include the License file at
 *  nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Sun in the GPL Version 2 section of the License file that
 *  accompanied this code. If applicable, add the following below the
 *  License Header, with the fields enclosed by brackets [] replaced by
 *  your own identifying information:
 *  "Portions Copyrighted [year] [name of copyright owner]"
 * 
 *  Contributor(s):
 * 
 *  Portions Copyrighted 1997-2009 Sun Microsystems, Inc.
 */

package org.netbeans.modules.javafx.refactoring.transformations;

import java.util.SortedSet;
import java.util.TreeSet;
import java.util.logging.Logger;

/**
 *
 * @author Jaroslav Bachorik
 */
final public class TransformationContext {
    final private static Logger LOGGER = Logger.getLogger(TransformationContext.class.getName());

    final private static class Change implements Comparable<Change> {
        final private int offset;
        final private int charDiff;

        public Change(int offset, int charDiff) {
            this.offset = offset;
            this.charDiff = charDiff;
        }
        
        public int compareTo(Change o) {
            return Integer.valueOf(offset).compareTo(Integer.valueOf(o.offset));
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final Change other = (Change) obj;
            if (this.offset != other.offset) {
                return false;
            }
            return true;
        }

        @Override
        public int hashCode() {
            int hash = 5;
            hash = 29 * hash + this.offset;
            return hash;
        }

        
    }

    final private SortedSet<Change> changes = new TreeSet<Change>();

    public void replaceText(int offset, int oldLen, int newLen) {
        int offsetDiff = newLen > oldLen ? oldLen : newLen;
        int changeDiff = newLen - oldLen;

        offset = getRealOffset(offset);

        changes.add(new Change(offset + offsetDiff, changeDiff));
    }

    public int getRealOffset(int offset) {
        int diff = 0;
        for(Change ch : changes) {
            if (offset > ch.offset) {
                diff += ch.charDiff;
            }
        }
        return offset + diff;
    }
}
