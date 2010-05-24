/*
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 *  Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 *  Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 *  Other names may be trademarks of their respective owners.
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
 *  nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Oracle in the GPL Version 2 section of the License file that
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

/**
 *
 * @author Jaroslav Bachorik
 */
final public class TransformationContext {
    final private static Logger LOGGER = Logger.getLogger(TransformationContext.class.getName());

    final private Object sortedLock = new Object();
    // @GuardedBy sortedLock
    private boolean isSorted = true;

    final private static class Change implements Comparable<Change> {
        final private int offset;
        final private int charDiff;

        public Change(int offset, int charDiff) {
            this.offset = offset;
            this.charDiff = charDiff;
        }
        
        public int compareTo(Change o) {
            if (offset < o.offset) {
                return -1;
            } else if (offset > o.offset) {
                return 1;
            } else {
                if (charDiff < o.charDiff) {
                    return -1;
                } else if (charDiff > o.charDiff) {
                    return 1;
                } else {
                    return 0;
                }
            }
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
            if (this.charDiff != other.charDiff) {
                return false;
            }
            return true;
        }

        @Override
        public int hashCode() {
            int hash = 5;
            hash = 71 * hash + this.offset;
            hash = 71 * hash + this.charDiff;
            return hash;
        }
        
    }

    final private List<Change> changes = new ArrayList<Change>();

    public void replaceText(int offset, int oldLen, int newLen) {
        int changeDiff = newLen - oldLen;
        if (changeDiff == 0) return;

        if (changeDiff > 0) {
            offset = offset + oldLen;
        } else {
            offset = offset + newLen;
        }
        changes.add(new Change(offset, changeDiff));
        synchronized(sortedLock) {
            isSorted = false;
        }
    }

    public int getRealOffset(int offset) {
        int diff = 0;
        synchronized(sortedLock) {
            if (!isSorted) {
                Collections.sort(changes);
                isSorted = true;
            }
        }
        for(Change ch : changes) {
            if (offset > ch.offset) {
                diff += ch.charDiff;
            }
        }
        return offset + diff;
    }
}
